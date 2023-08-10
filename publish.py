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
    merge_toc,
    get_soups,
    soup_decorate_per_html,
    get_multipages_titles,
    extract_time_version,
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
import pickle


ORG_CHANGE_DIR = os.path.dirname(os.path.realpath(sys.argv[0]))
os.chdir(ORG_CHANGE_DIR)
WWW = os.path.dirname(ORG_CHANGE_DIR)
CACHE_PATH = os.path.join(WWW, ".orgchange_cache")
global_cache = {}
if os.path.exists(CACHE_PATH):
    with open(CACHE_PATH, "rb") as f:
        global_cache = pickle.load(f)


def publish_via_index(index_org, verbose=False, republish_all=False):
    """
    publish all valid posts mentioned in index.org
    """

    site_info = extract_site_info_from_index_org(
        index_org, republish_all=republish_all
    )

    # collect categories and publish
    for post_info in site_info["posts"]:
        # e.g. ./posts/a.org
        for category in post_info.get("categories", []):
            if category == "":
                continue
            # distinguish with categories in post_info
            site_info["categories_map"][category].append(post_info)
        if post_info["need_update"]:
            publish_single_file(post_info, verbose)
            post_info["soup"] = get_soups([post_info["html_path_abs2sys"]])[0]
        else:
            print_green(f"no update, skip {post_info['html_path_abs2www']}")

    single_page_postprocessing(site_info)
    if site_info["need_update"]:
        generate_index_html(site_info)
        generate_category_html(site_info)
    else:
        print_green("no update, skip index.html and category.html")


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
        site_info = {
            "year": datetime.datetime.now().year,
        }
        i = 0

        for node in org:
            if "noexport" in node.tags:
                continue
            if "post" in node.tags and node.level == 1:
                site_info = update_site_info(node, site_info)
                assert (
                    site_info != {}
                ), f"please define site level config in python src block under {node.heading}"
            if "post" in node.tags and node.level == 2:
                # use shallow copy, make it like inheritance
                post_info = index_node_process(node, copy.copy(site_info))
                if post_info is None:  # wrong path
                    continue
                post_info["draft"] = "draft" in node.tags
                if not "draft" in node.tags:
                    i += 1
                    post_info["idx"] = i
                if republish_all:
                    post_info["need_update"] = True
                if post_info["need_update"]:
                    site_info["need_update"] = True
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
            site_info["user_elisp"] = content["body"]
    # publish_folder is used for generate dynamic pages(e.g. categories, index.html, about.html)
    site_info["publish_folder"] = normalize_path(site_info["publish_folder"])
    site_info["index_template"] = normalize_path(site_info["index_template"])
    site_info["org_prefixes"] = format_prefixes(site_info["org_prefixes"])
    site_info["theme"] = site_info.get("theme", "darkfloat")
    site_info["title"] = site_info.get("site_name", "Orgchange site")
    site_info["need_update"] = False
    # inverse map from id to html path
    site_info["id_map"] = {}
    # inverse map from theoritical org-export html path to real publish path
    site_info["html_map"] = {}
    site_info["categories_map"] = defaultdict(list)
    site_info[
        "emacs_org_version"
    ] = []  # use list to share across all post_info
    publish_folder_abs2www = "/" + extract_suffix_from_prefix(
        site_info["publish_folder"], WWW
    )
    if not publish_folder_abs2www.endswith(os.sep):
        publish_folder_abs2www += os.sep

    site_info["publish_folder_abs2www"] = publish_folder_abs2www
    return site_info


def post_title_path_prepare(node, post_info):
    """
    generate html_path_abs2sys, html_path_abs2www, org_path_abs2sys
    """
    publish_folder = post_info.get("publish_folder", "./example/www")

    prefixes = post_info.get("org_prefixes", [])
    heading = node.get_heading(format="raw")

    org_path_abs2sys = normalize_path(get_path_from_orglink(heading))
    if not is_valid_orgpath(org_path_abs2sys):
        print_yellow(f"invalid org path: {org_path_abs2sys}, skip")

        return None
    org_path_rel2prefix = extract_suffix(org_path_abs2sys, prefixes)

    html_path_rel2publish = org_path_rel2prefix.replace(".org", ".html")
    html_path_abs2sys = os.path.join(publish_folder, html_path_rel2publish)

    multipage_index = post_info.get("multipage_index", False)
    html_path_theoritical = os.path.basename(html_path_abs2sys)
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
            "title": get_title_from_orglink(heading),
            "html_path_theoritical": html_path_theoritical,
            "html_path_abs2sys": html_path_abs2sys,
            "html_path_abs2www": html_path_abs2www,
            "org_path_abs2sys": org_path_abs2sys,
            "html_path_rel2publish": html_path_rel2publish,
        }
    )
    post_info["html_map"][html_path_theoritical] = html_path_abs2www


def index_node_process(node, post_info):
    src_blocks = extract_code_blocks(node.get_body())
    for language, content in src_blocks:
        if language == "python":
            variable_setting = get_bindings_from_text(content["body"])
            post_info.update(variable_setting)
        if language == "emacs-lisp":
            post_info["user_elisp"] += content["body"]

    post_title_path_prepare(node, post_info)

    return post_info


def build_maps(post_info):
    if post_info.get("draft", False):
        return

    def id_contains_dot(tag_id):
        return tag_id.find(".") != -1 if tag_id else False

    soup = post_info["soup"]
    # 搜索所有的 h1, h2, h3, h4, 和 h5 标签
    headings = soup.find_all(
        ["h1", "h2", "h3", "h4", "h5"], id=id_contains_dot
    )
    for heading in headings:
        post_info["id_map"][heading["id"]] = post_info["html_path_abs2www"]


def bib_hook_parse(post_info):
    """
    insert #+bibliography and  #+cite_export into org buffer before export if user set bib_info variable
    bib_info is a string, or a tuple with two or three elements, e.g.
    ("~/org/lib/zotero.bib")
    ("~/org/lib/zotero.bib", "acl")
    ("~/org/lib/zotero.bib", "acl", "f")
    """
    bib_file, csl_style, cite_style = "", "acl", "f"
    if "bib_info" in post_info:
        bib_info = post_info["bib_info"]
        if type(bib_info) == str:
            bib_file = bib_info
        else:
            assert type(bib_info) in [
                tuple,
                list,
            ], "bib_info should be a str or tuple/list with 2 or 3 elements"

            if len(bib_info) == 2:
                bib_file, csl_style = bib_info
            elif len(bib_info) == 3:
                bib_file, csl_style, cite_style = bib_info
            else:
                raise Exception(
                    "bib_info should be a str or tuple/list with 2 or 3 elements"
                )
        csl_file = f"{ORG_CHANGE_DIR}/themes/static/{csl_style}.csl"

        return f"""
        (add-hook 'org-export-before-parsing-hook
        (change/org-make-add-bibliography "{bib_file}" "{csl_file}" "{cite_style}"))
        """

    return ""


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

    final_export_elisp = "(org-html-export-to-html)"
    multipage_index = post_info.get("multipage_index", False)
    if multipage_index:
        final_export_elisp = (
            f'(org-export-each-headline-to-html "{html_folder}")'
        )
        for file in glob.glob(os.path.join(html_folder, "*")):
            os.remove(file)
    elisp_code = f"""
    (progn 
        (setq default-directory "{html_folder}") 
        {post_info.get('user_elisp')}
        {bib_hook_parse(post_info)}
        {final_export_elisp})
    """

    theme = post_info.get("theme")
    theme_path = f"{ORG_CHANGE_DIR}/themes/{theme}"
    orgfile = post_info["org_path_abs2sys"]

    _export_to_html(theme_path, orgfile, elisp_code, verbose=verbose)
    if multipage_index:
        multipages_prepare(post_info, html_folder)
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


def multipages_prepare(post_info, html_folder):
    html_folder_rel2www = extract_suffix_from_prefix(html_folder, WWW)
    html_folder_abs2www = "/" + html_folder_rel2www
    with change_dir(html_folder):
        html_files = glob.glob("*.html")
        html_files_without_index = [
            html for html in html_files if not html.endswith("index.html")
        ]
        html_files = ["index.html"] + sorted(html_files_without_index)
        soups = get_soups(html_files)

        titles = get_multipages_titles(soups)
        # add_article_footer(html_files, soups, titles)
        posts = []
        for file, soup, title in zip(html_files, soups, titles):
            sub_post_info = copy.copy(post_info)
            sub_post_info["title"] = str(title)

            sub_post_info["soup"] = soup
            if file.endswith("index.html"):
                post_info["soup"] = soup

            header_titles = sub_post_info["soup"].find(
                "header", {"class": "header-titles"}
            )
            if header_titles:
                sub_post_info["header_titles"] = str(header_titles)

            sub_post_info["html_path_abs2www"] = os.path.join(
                html_folder_abs2www, file
            )
            sub_post_info["html_path_abs2sys"] = os.path.join(
                html_folder, file
            )
            build_maps(sub_post_info)
            posts.append(sub_post_info)
            # soup = change_index_title(soup, post_info["title"])

        merge_toc(posts)
        post_info["multipages"] = posts


def collect_prev_next_and_generate(posts):
    """
    if not draft, chain each post with prev and next post
    """
    for i, post_info in enumerate(posts):
        post_info["next"] = ("", "")
        post_info["prev"] = ("", "")
        if not post_info.get("draft", False):
            if i < len(posts) - 1:
                post_info["next"] = (
                    posts[i + 1]["html_path_abs2www"],
                    posts[i + 1]["title"],
                )
            if i > 0:
                post_info["prev"] = (
                    posts[i - 1]["html_path_abs2www"],
                    posts[i - 1]["title"],
                )

        if post_info["need_update"]:
            generate_post_page(post_info)


def separate_draft_posts(site_info):
    """
    最细粒度的 post
    """
    all_visible_posts = []
    all_draft_posts = []

    for post in site_info["posts"]:
        if "multipages" not in post:
            if not post.get("draft", False):
                all_visible_posts.append(post)
            else:
                all_draft_posts.append(post)
        else:
            for p in post["multipages"]:
                if not p.get("draft", False):
                    all_visible_posts.append(p)
                else:
                    all_draft_posts.append(p)
    site_info["all_draft_posts"] = all_draft_posts
    site_info["all_visible_posts"] = all_visible_posts
    return all_draft_posts + all_visible_posts


def merge_anthology_toc(site_info):
    anthologies = defaultdict(list)
    for i, post_info in enumerate(site_info["posts"]):
        if post_info.get("multipage_index", False):
            continue
        anthology = post_info.get("anthology", f"single_{i}")
        assert (
            type(anthology) != list
        ), f"{post_info['title']}'s anthology should be unique"

        anthologies[anthology].append(post_info)
    for anth in anthologies.keys():
        merge_toc(anthologies[anth])


def single_page_postprocessing(site_info):
    all_posts = separate_draft_posts(site_info)
    for post_info in all_posts:
        # 对所有 post 都提取时间信息（或者从缓存读取），用于显示在 post ，index list 和 category list 页面
        extract_time_version(post_info, cache=global_cache)
        if post_info["need_update"]:
            soup_decorate_per_html(post_info)
    # dump cache
    with open(CACHE_PATH, "wb") as f:
        pickle.dump(global_cache, f)
    merge_anthology_toc(site_info)
    collect_prev_next_and_generate(site_info["all_draft_posts"])
    collect_prev_next_and_generate(site_info["all_visible_posts"])


def generate_post_page(post_info):
    """
    generate index.html from index.org
    """

    index = post_info["index_template"]
    env = Environment(loader=FileSystemLoader(os.path.dirname(index)))
    article_template = "article.html"
    template = env.get_template(article_template)

    header_titles = post_info["soup"].find(
        "header", {"class": "header-titles"}
    )
    if header_titles:
        post_info["header_titles"] = str(header_titles)

    table_of_contents = post_info["soup"].find(
        "nav", {"id": "table-of-contents"}
    )
    if table_of_contents:
        post_info["table_of_contents"] = str(table_of_contents)

    emacs_scripts = post_info["soup"].find_all("script")
    if emacs_scripts:
        post_info["emacs_scripts"] = "\n".join(
            [str(script) for script in emacs_scripts]
        )

    org_main = post_info["soup"].find("div", {"id": "org-main"})
    if table_of_contents:
        post_info["org_main"] = str(org_main)
    rendered_template = template.render(post_info)

    with open(post_info["html_path_abs2sys"], "w") as f:
        f.write(rendered_template)
        print(f"{post_info['html_path_abs2sys']} generated")


def generate_index_html(site_info):
    """
    generate index.html from index.org
    """
    index_template = site_info["index_template"]
    env = Environment(loader=FileSystemLoader(os.path.dirname(index_template)))
    template = env.get_template(os.path.basename(index_template))
    visible_posts = [
        x for x in site_info["posts"] if not x.get("draft", False)
    ]

    # the diffenece between visible_posts and all_visible_posts is that visible_posts
    # only contain index post in multipages
    site_info["visible_posts"] = visible_posts

    rendered_template = template.render(site_info)
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

    for category in site_info["categories_map"]:
        category_info = copy.copy(site_info)
        # Draft posts will be deemed as visible posts.
        category_info.update(
            {
                "publish_offset": os.path.relpath(publish_folder, WWW),
                "section": category,
                "visible_posts": site_info["categories_map"][category],
            }
        )

        rendered_template = template.render(category_info)

        with open(
            os.path.join(publish_folder, "categories", f"{category}.html"), "w"
        ) as f:
            f.write(rendered_template)

    template = env.get_template("categories.html")
    site_info["categories_len"] = [
        (cate, len(lst)) for cate, lst in site_info["categories_map"].items()
    ]
    rendered_template = template.render(site_info)
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
