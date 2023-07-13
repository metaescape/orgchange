import datetime
import subprocess
import os
import sys
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
    read_config,
    do_need_modified,
    extract_code_blocks,
    get_bindings_from_text,
)


ORG_CHANGE_DIR = os.path.dirname(os.path.realpath(sys.argv[0]))
os.chdir(ORG_CHANGE_DIR)
WWW = os.path.dirname(ORG_CHANGE_DIR)


def index_node_process(node, publish_folder, prefixes, theme):
    heading = node.get_heading(format="raw")
    org_path_abs2sys = normalize_path(get_path_from_orglink(heading))
    if not is_valid_orgpath(org_path_abs2sys):
        return None

    title = get_title_from_orglink(heading)

    org_path_rel2prefix = extract_suffix(org_path_abs2sys, prefixes)

    html_path_rel2publish = org_path_rel2prefix.replace(".org", ".html")

    info = {
        "theme": theme,
        "title": title,
        "org_path_abs2sys": org_path_abs2sys,
        "html_path_rel2publish": html_path_rel2publish,
    }

    src_blocks = extract_code_blocks(node.get_body())
    for language, content in src_blocks:
        if language == "python":
            variable_setting = get_bindings_from_text(content["body"])
            info.update(variable_setting)

    multipage_index = info.get("multipage_index", False)
    html_path_abs2sys = os.path.join(publish_folder, html_path_rel2publish)

    if multipage_index:
        # e.g. ~/www/posts/book/index.html
        html_path_abs2sys = html_path_abs2sys.replace(".html", "/index.html")
    html_path_rel2www = extract_suffix_from_prefix(html_path_abs2sys, WWW)
    html_path_abs2www = "/" + html_path_rel2www

    info.update(
        {
            "html_path_abs2sys": html_path_abs2sys,
            "html_path_abs2www": html_path_abs2www,
        }
    )

    return info


def _export_to_html(theme, orgfile, elisp_code, verbose=False):
    """
    a wrapper of org-html-export-to-html
    """
    cmd = [
        "emacs",
        "--batch",
        f"--chdir={theme}",
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
        print(error.decode("utf-8"))
        raise Exception("Error Happened! Please check your elisp file")
    if verbose:
        print(" ".join(cmd))
        print(output.decode("utf-8"))
        print(error.decode("utf-8"))


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


def get_visible_post_ids(meta):
    visible_ids = [
        i
        for i, post in enumerate(meta["posts"])
        if not post.get("draft", False)
    ]

    return visible_ids


def single_page_postprocessing(meta):
    visible_ids = get_visible_post_ids(meta)
    soups = []
    html_files = []
    titles = []

    for i, post_info in enumerate(meta["posts"]):
        if "soup" not in post_info:
            # 即便是 draft 也要读取并修饰代码块等
            post_info["soup"] = get_soups([post_info["html_path_abs2sys"]])[0]

        if not post_info["html_path_abs2sys"].endswith("index.html"):
            # 不需要对 multipage 的 index.html 进行修饰
            post_info["soup"] = soup_decorate_per_html(
                post_info, post_info["soup"]
            )
            if post_info["need_update"]:
                # 只对需要更新的 html 进行 toc 合并,否则会新增一个 toc, 而且既然不更新也没必要处理
                post_info["soup"] = merge_toc(
                    [post_info["html_path_abs2sys"]], [post_info["soup"]]
                )[0]

        # 只有可见文章才需要添加 footer, 那些不需要更新的也要加入，提供上下文
        if not post_info.get("draft", False):
            soups.append(
                None if "soup" not in post_info else post_info["soup"]
            )
            html_files.append("/" + post_info["html_path_rel2publish"])
            titles.append(post_info["title"])

    add_article_footer(html_files, soups, titles)

    # 保存所有 soup，无论是否可见
    for i, post_info in enumerate(meta["posts"]):
        if "soup" not in post_info:
            continue
        soup = post_info["soup"]
        path = post_info["html_path_abs2sys"]
        path = path if type(path) != list else path[0]
        with open(path, "w") as f:
            f.write(soup.prettify())


# @cache("index.html")
def export_to_multiple_htmls(
    orgfile, html_folder, theme, publish_folder, verbose, **kwargs
):
    """
    export one org files to multiple html files, each heading is a html file
    """
    elisp_code = f"""
    (progn 
        (setq default-directory "{html_folder}") 
        (setq publish-directory "{publish_folder}") 
        (setq categories "{kwargs.get('categories', '')}") 
        (setq github-issue-link "{kwargs.get('github_issue_link', '#')}") 
        {kwargs.get('setq')}
        (org-export-each-headline-to-html "{html_folder}"))
    """

    _export_to_html(theme, orgfile, elisp_code, verbose=verbose)


# @cache("") # TODO Maybe merge with export_to_multiple_htmls
def export_to_single_html(
    orgfile, html_folder, theme, publish_folder, verbose, **kwargs
):
    """
    call org-html-export-to-html on `orgfile`, gerenating html file in `html_folder` using `theme`

    >>> export_to_html('../tests/demo.org', '/tmp', 'darkfloat')

    test cmdline
    emacs --batch --chdir=/data/codes/hugchangelife/orgchange/themes/darkfloat --load export.el /home/pipz/org/design/web/posts/20211101_picture_language_matplotlib.org --eval '(progn (setq default-directory \"/home/pipz/codes/hugchangelife/posts\") (setq publish-directory \"/home/pipz/codes/hugchangelife\") (org-html-export-to-html))' --kill
    """
    elisp_code = f"""
    (progn 
        (setq default-directory "{html_folder}") 
        (setq publish-directory "{publish_folder}") 
        (setq categories "{kwargs.get('categories', '')}") 
        (setq github-issue-link "{kwargs.get('github_issue_link', '#')}") 
        {kwargs.get('setq')}
        (org-html-export-to-html))
    """

    _export_to_html(theme, orgfile, elisp_code, verbose=verbose)


def extract_meta_from_index_org(orgfile, publish_folder, prefixes, theme):
    """
    extract metadata from index.org, return a dict
    get all child nodes from a level1 1 node with tag 'post'

    >>> extract_meta_from_index_org('tests/index.org')
    {'posts': [{'path': '/home/pipz/codes/orgpost/tests/demo.org', 'theme': 'darkfloat', 'categories': ['sample', 'test']}, {'path': '/home/pipz/codes/orgpost/tests/index.org', 'theme': 'darkfloat', 'categories': ['']}]}
    """
    with change_dir(os.path.dirname(orgfile)):
        org = orgparse.load(orgfile)
        posts = []
        for node in org:
            # ignore nodes with noexport tag
            if "noexport" in node.tags:
                continue
            if "post" in node.tags and node.level == 2:
                property_keys = list(node._properties.keys())
                for key in property_keys:
                    node._properties[key.lower()] = node._properties[key]

                post_info = index_node_process(
                    node, publish_folder, prefixes, theme
                )
                if post_info:
                    posts.append(post_info)

    return {"posts": posts}


def publish_single_file(
    post_info, publish_folder, verbose=False
) -> Union[BeautifulSoup, List[BeautifulSoup]]:
    """
    publish a single org file:
    - call export_to_html to generate html file
    - call extract_links_from_html to get all images file path
    - call rsync_copy to copy all images to publish_folder
    """
    multipage_index = post_info.get("multipage_index", False)

    org_path_abs2sys = post_info["org_path_abs2sys"]
    html_path_abs2sys = post_info["html_path_abs2sys"]

    # e.g. /www/posts/
    html_folder = os.path.dirname(html_path_abs2sys)

    if not os.path.exists(html_folder):
        os.makedirs(html_folder)

    theme = post_info.get("theme")
    theme_path = f"{ORG_CHANGE_DIR}/themes/{theme}"

    export_func = export_to_single_html

    if multipage_index:
        export_func = export_to_multiple_htmls
        # clear html_folder
        for file in glob.glob(os.path.join(html_folder, "*")):
            os.remove(file)

    export_func(
        org_path_abs2sys,
        html_folder,
        theme_path,
        publish_folder,
        verbose,
        **post_info.get("context", {}),
    )
    if multipage_index:
        multipage_postprocessing(post_info, html_folder)
    print("published to {}".format(html_path_abs2sys))


def generate_index_html(config, info, publish_folder):
    """
    generate index.html from index.org
    """
    index = info["index_template"]
    env = Environment(loader=FileSystemLoader(os.path.dirname(index)))
    template = env.get_template(os.path.basename(index))
    visible_posts = [x for x in info["posts"] if not x.get("draft", False)]
    for post in visible_posts:
        with open(post["html_path_abs2sys"], "r") as f:
            soup = BeautifulSoup(f, "html.parser")
        try:
            post["created"] = (
                soup.find("span", {"id": "created-timestamp"})
                .text.strip()
                .split(" ")[0]
            )
            post["last_modify"] = (
                soup.find("span", {"id": "last-modify-timestamp"})
                .text.strip()
                .split(" ")[0]
            )
        except:
            print(f"ignore postprocess for {post['html_path_abs2sys']}")
            pass

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
        if key in config:
            data[key] = config[key]

    rendered_template = template.render(data)
    index_html = os.path.join(publish_folder, "index.html")
    with open(index_html, "w") as f:
        f.write(rendered_template)
        print(f"{index_html} generated")


def generate_category_html(config, info, publish_folder):
    """
    generate categories/tag.html from info
    """
    index = info["index_template"]

    env = Environment(loader=FileSystemLoader(os.path.dirname(index)))
    category_template = "category.html"
    template = env.get_template(category_template)
    categories_dir = os.path.join(publish_folder, "categories")
    if not os.path.exists(categories_dir):
        os.makedirs(categories_dir)

    for category in info["categories"]:
        data = {
            "publish_offset": os.path.relpath(publish_folder, WWW),
            "year": datetime.datetime.now().year,
            "section": category,
            "posts": info["categories"][category],
        }

        for key in ["beian", "sitename", "github_url", "github_name"]:
            if key in config:
                data[key] = config[key]

        rendered_template = template.render(data)

        with open(
            os.path.join(publish_folder, "categories", f"{category}.html"), "w"
        ) as f:
            f.write(rendered_template)

    template = env.get_template("categories.html")
    data = {
        "year": datetime.datetime.now().year,
        "categories": [
            (cate, len(lst)) for cate, lst in info["categories"].items()
        ],
    }
    rendered_template = template.render(data)
    categories_index_html = os.path.join(
        publish_folder, "categories", "index.html"
    )
    with open(categories_index_html, "w") as f:
        f.write(rendered_template)
        print(f"{categories_index_html} generated")


def publish_via_index(config, verbose=False):
    """
    publish all valid posts mentioned in index.org
    """
    index_org = normalize_path(config["index_org"])
    publish_folder = normalize_path(
        config.get("publish_folder", "./example/www")
    )
    prefixes = format_prefixes(config["org_prefixes"])

    meta = extract_meta_from_index_org(
        index_org,
        publish_folder,
        prefixes,
        config.get("default_theme", "darkfloat"),
    )

    meta["index_template"] = config["index_template"]
    meta["categories"] = defaultdict(list)
    # first pass on all posts, generate html relative path
    for i, post_info in enumerate(meta["posts"]):
        # e.g. ./posts/a.org

        for category in post_info.get("categories", []):
            if category == "":
                continue
            meta["categories"][category].append(
                {
                    "html_path_abs2www": post_info["html_path_abs2www"],
                    "title": post_info["title"],
                }
            )
        post_info["need_update"] = do_need_modified(
            post_info["theme"],
            post_info["org_path_abs2sys"],
            post_info["html_path_abs2sys"],
        )
        post_info["prefixes"] = prefixes

    setq = ""
    for var, value in config.get("setq", {}).items():
        # 只支持从外部设置 string 变量
        if value == "nil" or type(value) != str:
            setq += f"""(setq {var} {value})"""
        else:
            setq += f"""(setq {var} "{value}")"""

    for i, post_info in enumerate(meta["posts"]):
        post_info["context"] = {}
        context = post_info.get("context")
        if "site_repo" in config:
            site_repo = config["site_repo"]
            context["github_issue_link"] = os.path.join(
                site_repo, "issues/new"
            )
        context["categories"] = post_info.get("categories", [])
        context["setq"] = setq
        if post_info["need_update"]:
            publish_single_file(post_info, publish_folder, verbose)

    single_page_postprocessing(meta)

    generate_index_html(config, meta, publish_folder)

    generate_category_html(config, meta, publish_folder)


if __name__ == "__main__":
    # import doctest

    # doctest.testmod()
    parser = argparse.ArgumentParser(description="Publish website")
    # Add arguments for index file and publish folder
    parser.add_argument(
        "--config",
        type=str,
        default="config.json",
        help="path to json config file",
    )
    parser.add_argument(
        "--verbose", action="store_true", help="increase output verbosity"
    )
    args = parser.parse_args()

    config = read_config(args.config)
    publish_via_index(config, verbose=args.verbose)
