import datetime
import subprocess
import os
import sys
import copy
import orgparse
from bs4 import BeautifulSoup
from jinja2 import Environment, FileSystemLoader
import argparse
from collections import defaultdict
import glob
from dom import (
    add_article_footer,
    merge_toc,
    get_soups,
    soup_decorate_per_html,
    get_titles,
    extract_timestamps,
)
from typing import List, Union

from utils import (
    change_dir,
    extract_suffix,
    extract_suffix_from_prefix,
    format_prefixes,
    get_path_from_orglink,
    get_title_from_orglink,
    is_valid_orgpath,
    normalize_path,
    do_need_modified,
    extract_code_blocks,
    get_bindings_from_text,
    print_green,
    print_yellow,
    print_red,
)


ORG_CHANGE_DIR = os.path.dirname(os.path.realpath(sys.argv[0]))
os.chdir(ORG_CHANGE_DIR)
WWW = os.path.dirname(ORG_CHANGE_DIR)


def publish_via_index(index_org, verbose=False, republish_all=False):
    """
    publish all valid posts mentioned in index.org
    """

    site_info = extract_site_info_from_index_org(
        index_org, republish_all=republish_all
    )

    site_info["categories"] = defaultdict(list)

    # collect categories and publish
    for post_info in site_info["posts"]:
        # e.g. ./posts/a.org
        for category in post_info.get("categories", []):
            if category == "":
                continue
            site_info["categories"][category].append(post_info)
        if post_info["need_update"]:
            publish_single_file(post_info, verbose)
        else:
            print_green(f"no update, skip {post_info['html_path_abs2www']}")

    single_page_postprocessing(site_info)

    generate_index_html(site_info)

    generate_category_html(site_info)


def extract_site_info_from_index_org(orgfile, republish_all=False):
    """
    extract metadata from index.org, return a dict
    get all child nodes from a level1 1 node with tag 'post'

    >>> extract_meta_from_index_org('tests/index.org')
    {'posts': [{'path': '/home/pipz/codes/orgpost/tests/demo.org', 'theme': 'darkfloat', 'categories': ['sample', 'test']}, {'path': '/home/pipz/codes/orgpost/tests/index.org', 'theme': 'darkfloat', 'categories': ['']}]}
    """

    with change_dir(os.path.dirname(orgfile)):
        org = orgparse.load(orgfile)
        posts = []
        site_info = {}

        for node in org:
            if "noexport" in node.tags:
                continue
            if "post" in node.tags and node.level == 1:
                site_info = update_site_info(node, site_info)
                assert (
                    site_info != {}
                ), f"please define site level config in python src block under {node.heading}"
            if "post" in node.tags and node.level == 2:
                post_info = index_node_process(node, copy.deepcopy(site_info))
                if post_info is None:  # wrong path
                    continue
                post_info["draft"] = "draft" in node.tags
                if republish_all:
                    post_info["need_update"] = True
                posts.append(post_info)
        site_info["posts"] = posts
    return site_info


def update_site_info(node, site_info: dict):
    # site level config
    src_blocks = extract_code_blocks(node.get_body())
    for language, content in src_blocks:
        if language == "python":
            variable_setting = get_bindings_from_text(content["body"])
            site_info.update(variable_setting)
        if language == "emacs-lisp":
            site_info["setq"] = content["body"]
    site_info["publish_folder"] = normalize_path(site_info["publish_folder"])
    site_info["index_template"] = normalize_path(site_info["index_template"])
    site_info["org_prefixes"] = format_prefixes(site_info["org_prefixes"])
    site_info["theme"] = site_info.get("theme", "darkfloat")
    return site_info


def index_node_process(node, post_info):
    publish_folder = post_info.get("publish_folder", "./example/www")
    prefixes = post_info.get("org_prefixes", [])

    heading = node.get_heading(format="raw")
    org_path_abs2sys = normalize_path(get_path_from_orglink(heading))
    if not is_valid_orgpath(org_path_abs2sys):
        print_yellow(f"invalid org path: {org_path_abs2sys}, skip")
        return None

    post_info["title"] = get_title_from_orglink(heading)
    org_path_rel2prefix = extract_suffix(org_path_abs2sys, prefixes)

    html_path_rel2publish = org_path_rel2prefix.replace(".org", ".html")

    post_info.update(
        {
            "org_path_abs2sys": org_path_abs2sys,
            "html_path_rel2publish": html_path_rel2publish,
        }
    )

    src_blocks = extract_code_blocks(node.get_body())
    for language, content in src_blocks:
        if language == "python":
            variable_setting = get_bindings_from_text(content["body"])
            post_info.update(variable_setting)
        if language == "emacs-lisp":
            post_info["setq"] += content["body"]
    multipage_index = post_info.get("multipage_index", False)

    html_path_abs2sys = os.path.join(publish_folder, html_path_rel2publish)

    if multipage_index:
        # e.g. ~/www/posts/book/index.html
        html_path_abs2sys = html_path_abs2sys.replace(".html", "/index.html")
    html_path_rel2www = extract_suffix_from_prefix(html_path_abs2sys, WWW)
    html_path_abs2www = "/" + html_path_rel2www

    post_info["need_update"] = do_need_modified(
        post_info["theme"],
        org_path_abs2sys,
        html_path_abs2sys,
    )

    post_info.update(
        {
            "html_path_abs2sys": html_path_abs2sys,
            "html_path_abs2www": html_path_abs2www,
        }
    )

    return post_info


def publish_single_file(
    post_info, verbose=False
) -> Union[BeautifulSoup, List[BeautifulSoup]]:
    """
    publish a single org file:
    - call export_to_html to generate html file
    - call extract_links_from_html to get all images file path
    - call rsync_copy to copy all images to publish_folder
    """

    html_path_abs2sys = post_info["html_path_abs2sys"]

    # e.g. /www/posts/
    html_folder = os.path.dirname(html_path_abs2sys)

    if not os.path.exists(html_folder):
        os.makedirs(html_folder)

    html_folder = os.path.dirname(post_info["html_path_abs2sys"])

    if not os.path.exists(html_folder):
        os.makedirs(html_folder)

    final_export_elisp = "(org-html-export-to-html)"
    multipage_index = post_info.get("multipage_index", False)
    if multipage_index:
        final_export_elisp = (
            f'(org-export-each-headline-to-html "{html_folder}")'
        )
        for file in glob.glob(os.path.join(html_folder, "*")):
            os.remove(file)
    category = ",".join(post_info.get("categories", ""))
    elisp_code = f"""
    (progn 
        (setq default-directory "{html_folder}") 
        (setq publish-directory "{post_info.get('publish_folder')}") 
        (setq categories "{category}") 
        (setq github-issue-link "{post_info.get('github_issue_link', '#')}") 
        {post_info.get('setq')}
        {final_export_elisp})
    """

    theme = post_info.get("theme")
    theme_path = f"{ORG_CHANGE_DIR}/themes/{theme}"
    orgfile = post_info["org_path_abs2sys"]

    _export_to_html(theme_path, orgfile, elisp_code, verbose=verbose)
    if multipage_index:
        multipage_postprocessing(post_info, html_folder)
    print("published to {}".format(html_path_abs2sys))


def _export_to_html(theme_path, orgfile, elisp_code, verbose=False):
    """
    a wrapper of org-html-export-to-html
    """
    cmd = [
        "emacs",
        "--batch",
        f"--chdir={theme_path}",
        "--load",
        "../general.el",
        "--load",
        "export.el",
        orgfile,
        "--eval",
        elisp_code,
        "--kill",
    ]
    process = subprocess.Popen(
        cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE
    )

    output, error = process.communicate()
    if "Debugger" in error.decode("utf-8"):
        print(" ".join(cmd))
        print(output.decode("utf-8"))
        print_red(error.decode("utf-8"))
        raise Exception("Error Happened! Please check your elisp file")

    if verbose:
        print_yellow(" ".join(cmd))
        print_yellow(output.decode("utf-8"))
        print_yellow(error.decode("utf-8"))


def multipage_postprocessing(post_info, html_folder):
    with change_dir(html_folder):
        html_files = glob.glob("*.html")
        html_files_without_index = [
            html for html in html_files if not html.endswith("index.html")
        ]
        html_files = ["index.html"] + sorted(html_files_without_index)

        soups = get_soups(html_files)
        soups = merge_toc(html_files, soups)
        titles = get_titles(soups)
        add_article_footer(html_files, soups, titles)
        for file, soup in zip(html_files, soups):
            if file.endswith("index.html"):
                # soup = change_index_title(soup, post_info["title"])
                post_info["soup"] = soup
            soup = soup_decorate_per_html(post_info, soup)

            with open(file, "w") as f:
                f.write(soup.prettify())


def merge_single_pages_footer(site_info):
    visible_soups = []
    visible_htmls = []
    visible_titles = []
    for post_info in site_info["posts"]:
        # 只有可见文章才需要添加 footer, 那些不需要更新的也要加入，提供上下文
        if not post_info.get("draft", False):
            post_info["idx"] = len(visible_htmls)
            visible_soups.append(
                None if "soup" not in post_info else post_info["soup"]
            )
            visible_htmls.append(post_info["html_path_abs2www"])
            visible_titles.append(post_info["title"])

    add_article_footer(visible_htmls, visible_soups, visible_titles)


def save_single_pages_soup(site_info):
    # 保存所有 soup，无论是否可见
    for post_info in site_info["posts"]:
        if "soup" not in post_info:
            continue
        soup = post_info["soup"]
        path = post_info["html_path_abs2sys"]
        with open(path, "w") as f:
            f.write(soup.prettify())


def merge_anthology_toc(site_info):
    anthologies_soup = defaultdict(list)
    anthologies_path = defaultdict(list)
    anthologies = defaultdict(list)
    for i, post_info in enumerate(site_info["posts"]):
        if post_info.get("draft", False):
            continue
        anthology = post_info.get("anthology", f"sigle_{i}")
        assert (
            type(anthology) != list
        ), f"{post_info['title']}'s anthology should be unique"
        anthologies_soup[anthology].append(post_info["soup"])
        anthologies_path[anthology].append(post_info["html_path_abs2www"])
        anthologies[anthology].append(post_info)
    for anth in anthologies_soup.keys():
        soups = merge_toc(anthologies_path[anth], anthologies_soup[anth])
        for post_info, soup in zip(anthologies[anth], soups):
            post_info[soup] = soup


def single_page_postprocessing(site_info):
    for post_info in site_info["posts"]:
        if "soup" not in post_info:
            # 对所有 post 都要读取 soup，用于提供合并 toc 的上下文
            post_info["soup"] = get_soups([post_info["html_path_abs2sys"]])[0]
        # 对所有 post 都提取时间信息，用于显示在 index list 和 category list 页面
        extract_timestamps(post_info)

        # 不需要对 multipage 的 index.html 进行单 soup 修饰
        if (
            not post_info.get("multipage_index", False)
            and post_info["need_update"]
        ):
            post_info["soup"] = soup_decorate_per_html(
                post_info, post_info["soup"]
            )

    merge_anthology_toc(site_info)
    merge_single_pages_footer(site_info)
    save_single_pages_soup(site_info)


def generate_index_html(site_info):
    """
    generate index.html from index.org
    """
    index = site_info["index_template"]
    env = Environment(loader=FileSystemLoader(os.path.dirname(index)))
    template = env.get_template(os.path.basename(index))
    visible_posts = [
        x for x in site_info["posts"] if not x.get("draft", False)
    ]

    data = {
        "year": datetime.datetime.now().year,
        "posts": visible_posts,
    }

    for key in [
        "beian",
        "sitename",
        "github_url",
        "github_name",
    ]:
        if key in site_info:
            data[key] = site_info[key]

    rendered_template = template.render(data)
    index_html = os.path.join(site_info["publish_folder"], "index.html")
    with open(index_html, "w") as f:
        f.write(rendered_template)
        print(f"{index_html} generated")


def generate_category_html(site_info):
    """
    generate categories/tag.html from info
    """
    index = site_info["index_template"]
    publish_folder = site_info["publish_folder"]
    env = Environment(loader=FileSystemLoader(os.path.dirname(index)))
    category_template = "category.html"
    template = env.get_template(category_template)
    categories_dir = os.path.join(publish_folder, "categories")
    if not os.path.exists(categories_dir):
        os.makedirs(categories_dir)

    for category in site_info["categories"]:
        data = {
            "publish_offset": os.path.relpath(publish_folder, WWW),
            "year": datetime.datetime.now().year,
            "section": category,
            "posts": site_info["categories"][category],
        }

        for key in ["beian", "sitename", "github_url", "github_name"]:
            if key in site_info:
                data[key] = site_info[key]
        rendered_template = template.render(data)

        with open(
            os.path.join(publish_folder, "categories", f"{category}.html"), "w"
        ) as f:
            f.write(rendered_template)

    template = env.get_template("categories.html")
    data = {
        "year": datetime.datetime.now().year,
        "categories": [
            (cate, len(lst)) for cate, lst in site_info["categories"].items()
        ],
    }
    rendered_template = template.render(data)
    categories_index_html = os.path.join(
        publish_folder, "categories", "index.html"
    )
    with open(categories_index_html, "w") as f:
        f.write(rendered_template)
        print(f"{categories_index_html} generated")


if __name__ == "__main__":
    # import doctest

    # doctest.testmod()
    parser = argparse.ArgumentParser(description="Publish website")
    # Add arguments for index file and publish folder
    parser.add_argument(
        "--index",
        type=str,
        default="example/index.org",
        help="path to org index file",
    )
    parser.add_argument(
        "--verbose", action="store_true", help="increase output verbosity"
    )
    parser.add_argument("--all", action="store_true", help="publish all posts")
    args = parser.parse_args()
    index_org = normalize_path(args.index)
    publish_via_index(index_org, verbose=args.verbose, republish_all=args.all)
