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
    _add_article_footer,
    _merge_toc,
    pygment_and_paren_match_all,
    get_soups,
)

from utils import (
    cache,
    change_dir,
    extract_links_from_html,
    extract_suffix,
    extract_suffix_from_prefix,
    format_prefixes,
    get_path_from_orglink,
    get_title_from_orglink,
    is_valid_orgpath,
    normalize_path,
    read_config,
    rsync_copy,
)


ORG_CHANGE_DIR = os.path.dirname(os.path.realpath(sys.argv[0]))
os.chdir(ORG_CHANGE_DIR)
WWW = os.path.dirname(ORG_CHANGE_DIR)


def index_node_process(node, publish_folder, prefixes, theme):
    heading = node.get_heading(format="raw")
    org_path_abs2sys = normalize_path(get_path_from_orglink(heading))
    if not is_valid_orgpath(org_path_abs2sys):
        return None
    list_index = node.get_property("index", False)
    list_index = False if list_index == "nil" else list_index

    title = get_title_from_orglink(heading)

    org_path_rel2prefix = extract_suffix(org_path_abs2sys, prefixes)

    html_path_rel2publish = org_path_rel2prefix.replace(".org", ".html")

    html_path_abs2sys = os.path.join(publish_folder, html_path_rel2publish)
    if list_index:
        # e.g. ~/www/posts/book/index.html
        html_path_abs2sys = html_path_abs2sys.replace(".html", "/index.html")
    html_path_rel2www = extract_suffix_from_prefix(html_path_abs2sys, WWW)
    html_path_abs2www = "/" + html_path_rel2www

    return {
        "theme": node.get_property("theme", theme),
        "list_index": list_index,
        "draft": node.get_property("draft", False),
        "categories": [
            x.strip() for x in node.get_property("categories", "").split(",")
        ],
        "title": title,
        "org_path_abs2sys": org_path_abs2sys,
        "html_path_rel2publish": html_path_rel2publish,
        "html_path_abs2sys": html_path_abs2sys,
        "html_path_abs2www": html_path_abs2www,
    }


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


def multipage_postprocessing(orgfile, html_folder):
    org = orgparse.load(orgfile)
    html_files = []
    for node in org:
        if "noexport" in node.tags or node.level != 1:
            continue
        heading = node.get_heading(format="raw")
        html_files.append(f"{heading.strip()}.html")

    with change_dir(html_folder):
        soups = get_soups(html_files)
        soups = _merge_toc(html_files, soups)
        for soup in soups:
            soup = pygment_and_paren_match_all(soup)
        _add_article_footer(html_files, soups)
        for file, soup in zip(html_files, soups):
            with open(file, "w") as f:
                f.write(soup.prettify())


def get_visible_posts(meta):
    html_files = [
        post["html_path_abs2www"]
        for post in meta["posts"]
        if not (post["list_index"] or post["draft"])
    ]
    titles = [
        post["title"]
        for post in meta["posts"]
        if not (post["list_index"] or post["draft"])
    ]
    return html_files, titles


def single_page_postprocessing(html_files, titles=[]):
    soups = []
    with change_dir(WWW):
        for html in html_files:
            with open(html[1:], "r") as f:
                soup = BeautifulSoup(f, "html.parser")
                soup = _merge_toc([html], [soup])[0]
                soup = pygment_and_paren_match_all(soup)  # code highlight
                soups.append(soup)

        _add_article_footer(html_files, soups, titles)

        for html, soup in zip(html_files, soups):
            with open(html[1:], "w") as f:
                f.write(soup.prettify())


@cache("index.html")
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
    multipage_postprocessing(orgfile, html_folder)


@cache("")
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


def publish_single_file(publish_info, publish_folder, verbose=False):
    """
    publish a single org file:
    - generate  target_folder from publish_folder and target_file_path
    - call export_to_html to generate html file
    - call extract_links_from_html to get all images file path
    - call rsync_copy to copy all images to publish_folder
    """
    list_index = publish_info.get("list_index", False)

    org_path_abs2sys = publish_info["org_path_abs2sys"]
    html_path_abs2sys = publish_info["html_path_abs2sys"]

    # e.g. ~/org/posts/
    org_folder = os.path.dirname(org_path_abs2sys)

    # e.g. /www/posts/
    html_folder = os.path.dirname(html_path_abs2sys)

    if not os.path.exists(html_folder):
        os.makedirs(html_folder)

    theme = publish_info.get("theme")
    theme_path = f"{ORG_CHANGE_DIR}/themes/{theme}"

    export_func = export_to_single_html
    target_file_pathes = [html_path_abs2sys]
    if list_index:
        export_func = export_to_multiple_htmls
        # get all html fils under target_folder
        target_file_pathes = glob.glob(os.path.join(html_folder, "*.html"))

    export_func(
        org_path_abs2sys,
        html_folder,
        theme_path,
        publish_folder,
        verbose,
        **publish_info.get("context", {}),
    )

    img_urls = extract_links_from_html(target_file_pathes)

    for img_url in img_urls:
        with change_dir(org_folder):
            # mv url from ~/org/posts/imgs/... to /www/posts/imgs/...
            rsync_copy(img_url, html_folder)

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

        for category in post_info["categories"]:
            if category == "":
                continue
            meta["categories"][category].append(
                {
                    "html_path_abs2www": post_info["html_path_abs2www"],
                    "title": post_info["title"],
                }
            )
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
        context["categories"] = ",".join(post_info["categories"])
        context["setq"] = setq

        publish_single_file(post_info, publish_folder, verbose)

    visible_htmls, visible_titles = get_visible_posts(meta)

    single_page_postprocessing(visible_htmls, visible_titles)

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
