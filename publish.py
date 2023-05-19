import datetime
import subprocess
import os
import sys
import shutil
import orgparse
import re
from bs4 import BeautifulSoup
import json
from jinja2 import Environment, FileSystemLoader
import argparse
from contextlib import contextmanager
from collections import defaultdict
import glob


RE_LINK = re.compile(
    r"""
    (?:
        \[ \[
            (?P<desc0> [^\]]+)
        \] \]
    ) |
    (?:
        \[ \[
            (?P<link1> [^\]]+)
        \] \[
            (?P<desc1> [^\]]+)
        \] \]
    )
    """,
    re.VERBOSE,
)

ORG_CHANGE_DIR = os.path.dirname(os.path.realpath(sys.argv[0]))
os.chdir(ORG_CHANGE_DIR)
WWW = os.path.dirname(ORG_CHANGE_DIR)


def read_config(config_file):
    with open(config_file, "r") as f:
        config = json.load(f)

    return {} if not os.path.exists(config_file) else config


@contextmanager
def change_dir(directory):
    old_dir = os.getcwd()
    os.chdir(directory)
    try:
        yield
    finally:
        os.chdir(old_dir)


def is_valid_orgpath(path):
    """
    check if path is valid

    >>> is_valid_orgpath('/usr/bin/python')
    False
    >>> is_valid_orgpath('/tmp/a')
    False
    """

    return os.path.exists(path) and path.endswith(".org")


def get_path_from_orglink(raw_link):
    """
    extract path from org link

    >>> get_path_from_orglink('[[/tmp/a.org][test demo]]')
    '/tmp/a.org'
    >>> get_path_from_orglink('[[/tmp/a.org]]')
    '/tmp/a.org'
    >>> get_path_from_orglink('not a link')
    'not a link'
    """
    path = RE_LINK.sub(
        lambda m: m.group("desc0") or m.group("link1"), raw_link
    )
    if path.startswith("file:"):
        path = path[5:]
    return path


def get_title_from_orglink(raw_link):
    """
    extract path from org link

    >>> get_path_from_orglink('[[/tmp/a.org][test demo]]')
    'test demo'
    >>> get_path_from_orglink('[[/tmp/a.org]]')
    ''
    >>> get_path_from_orglink('not a link')
    'not a link'
    """
    return RE_LINK.sub(
        lambda m: m.group("desc0") or m.group("desc1"), raw_link
    )


def normalize_path(path: str, folder=None) -> str:
    """
    normalize path to absolute path
    """
    if path.startswith("~"):
        path = os.path.realpath(os.path.expanduser(path))
    if path.startswith("/"):
        return os.path.realpath(path)
    if folder:
        return os.path.abspath(os.path.realpath(os.path.join(folder, path)))
    return os.path.abspath(path)


def format_prefixes(prefixes):
    """
    org_prefix is a list of prefixes that are used to identify which org files
    will be published. For example, if the org_prefix is ["~/post"], then only
    org file such as `~/post/blog/2020-01-01.org` will be published to `web/blog/2020-01-01.html`
    `web/` is the main folder for all published files.
    """
    formatted_prefixes = []
    for prefix in prefixes:
        prefix = normalize_path(prefix)
        formatted_prefixes.append(os.path.realpath(prefix))

    return sorted(formatted_prefixes, key=len, reverse=True)


def extract_links_from_html(pathes):
    img_urls = []
    for path in pathes:
        with open(path, "r") as f:
            html_content = f.read()

        # Parse the HTML content using BeautifulSoup
        soup = BeautifulSoup(html_content, "html.parser")

        # Find all image tags in the HTML document
        img_tags = soup.find_all("img")

        # Extract the image URLs from the image tags using regular expressions
        img_urls.extend([img["src"] for img in img_tags if "src" in img.attrs])
    return img_urls


def rsync_copy(file_suffix, target_folder):
    target_file = os.path.join(target_folder, file_suffix)

    target_folder = os.path.dirname(target_file)

    # Create the folder if it doesn't exist
    if not os.path.exists(target_folder):
        os.makedirs(target_folder)

    # Copy the source file to the target file only if it has been modified
    check_modified_time_hook(file_suffix, target_file, shutil.copy2)


def check_modified_time_hook(file1, file2, f):
    """
    check if file1 is modified after file2, if so, call f(file1, file2)
    """
    if not os.path.exists(file2) or os.path.getmtime(file1) > os.path.getmtime(
        file2
    ):
        f(file1, file2)


def extract_suffix(file_path, prefixes):
    """
    extract file suffix from file_path if file_path starts with any prefix in prefixes
    return the first match
    """
    for prefix in prefixes:
        try:
            suffix = extract_suffix_from_prefix(file_path, prefix)
            return suffix
        except ValueError:
            continue
    raise ValueError("No prefix matches the file path.")


def extract_suffix_from_prefix(file_path, prefix):
    """
    extract file suffix from file_path if file_path starts with prefix
    """
    if not prefix.endswith(os.sep):
        prefix += os.sep

    if file_path.startswith(prefix):
        suffix = file_path[len(prefix) :]
        return suffix
    else:
        raise ValueError(
            "The file path does not start with the prefix folder."
        )


def new_soup(soup):
    return BeautifulSoup(str(soup), "html.parser")


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

    html_path_rel2www = extract_suffix_from_prefix(html_path_abs2sys, WWW)
    html_path_abs2www = "/" + html_path_rel2www
    if list_index:
        # e.g. ~/www/posts/book/index.html
        html_path_abs2sys = html_path_abs2sys.replace(".html", "/index.html")

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


# utils end, workflow begin:


def _export_to_html(theme, orgfile, elisp_code, verbose=False):
    """
    a wrapper of org-html-export-to-html
    """
    cmd = [
        "emacs",
        "--batch",
        f"--chdir={ORG_CHANGE_DIR}/themes/{theme}",
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


def _merge_toc(html_files):
    # 创建一个字典，用于存储每个文件的table of contents元素

    tocs = []
    soups = []

    # 第一次遍历：获取每个文件的目录
    for file in html_files:
        # 打开文件并解析HTML
        with open(file, "r") as f:
            soup = BeautifulSoup(f, "html.parser")
        try:
            title = soup.find("h1", {"class": "title"}).contents[0]

        except AttributeError:
            raise AttributeError(
                f"\n--> ERROR: you may need to add a #+title in your org file: {os.path.abspath( os.curdir)}.org\n"
            )
        title_li_str = f'<li> <a href="{file}">{title}</a></li>'
        title_li = new_soup(title_li_str)

        local_toc = new_soup(title_li_str)

        toc = soup.find("div", {"id": "text-table-of-contents"})
        if toc:
            local_toc.append(new_soup(toc.ul))

        # print(local_toc.prettify(), title_li.prettify())
        soups.append(soup)
        tocs.append((local_toc, title_li))

    global_tocs = []
    for i, (local_toc, title_li) in enumerate(tocs):
        global_toc_str = f"""
        <nav id="global-toc">   
            <ul>
            </ul>
        </nav>
        """

        global_toc = BeautifulSoup(global_toc_str, "html.parser")
        global_toc.ul.extend([new_soup(x[-1]) for x in tocs[:i]])
        global_toc.ul.append(local_toc)
        global_toc.ul.extend([new_soup(x[-1]) for x in tocs[i + 1 :]])
        global_tocs.append(global_toc)

    for file, soup, global_toc in zip(html_files, soups, global_tocs):
        # print(global_toc)

        container = soup.find("div", {"class": "container"})

        # if not container.find("nav", {"id": "global-toc"}):
        container.insert(0, new_soup(global_toc))
        # 如果不用 new_soup,  global_toc 会被清空，为什么？

    return soups


def _insert_paginav(paginav, links, titles, i, cls="prev"):
    """
    insert a paginav element with link and title
    """
    n = len(links)
    if n == 0:
        return
    nav_tag = paginav.find("a", {"class": cls})
    idx = 1 if cls == "prev" else 0
    span = nav_tag.find_all("span")[idx]
    _offset = -1 if cls == "prev" else 1
    alt_idx = (i + _offset) % n
    if titles:
        span.string = titles[alt_idx]
    else:
        span.string = os.path.splitext(os.path.basename(links[alt_idx]))[0]

    nav_tag["href"] = links[alt_idx]


def _add_article_footer(html_files, soups, titles=[]):
    """
    add article footer to each html file
    """
    n = len(html_files)
    for i in range(n):
        _, soup = html_files[i], soups[i]
        paginav = soup.find("nav", {"class": "paginav"})
        _insert_paginav(paginav, html_files, titles, i, cls="prev")
        _insert_paginav(paginav, html_files, titles, i, cls="next")


def multipage_postprocessing(orgfile, html_folder):
    org = orgparse.load(orgfile)
    html_files = []
    for node in org:
        if "noexport" in node.tags or node.level != 1:
            continue
        heading = node.get_heading(format="raw")
        html_files.append(f"{heading.strip()}.html")

    with change_dir(html_folder):
        soups = _merge_toc(html_files)
        _add_article_footer(html_files, soups)
        for file, soup in zip(html_files, soups):
            with open(file, "w") as f:
                f.write(soup.prettify())


def single_page_postprocessing(meta):
    html_files = [
        post["html_path_abs2www"]
        for post in meta["posts"]
        if not post["list_index"] or not post["draft"]
    ]
    titles = [
        post["title"]
        for post in meta["posts"]
        if not post["list_index"] or not post["draft"]
    ]
    soups = []

    with change_dir(WWW):
        for html in html_files:
            with open(html[1:], "r") as f:
                soup = BeautifulSoup(f, "html.parser")
                soups.append(soup)

        _add_article_footer(html_files, soups, titles)
        for html, soup in zip(html_files, soups):
            with open(html[1:], "w") as f:
                f.write(soup.prettify())


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
        (org-export-each-headline-to-html "{html_folder}"))
    """

    _export_to_html(theme, orgfile, elisp_code, verbose=verbose)
    multipage_postprocessing(orgfile, html_folder)


def export_to_single_html(
    orgfile, target_folder, theme, www_folder, verbose, **kwargs
):
    """
    call org-html-export-to-html on `orgfile`, gerenating html file in `target_folder` using `theme`

    >>> export_to_html('../tests/demo.org', '/tmp', 'darkfloat')

    test cmdline
    emacs --batch --chdir=/data/codes/hugchangelife/orgchange/themes/darkfloat --load export.el /home/pipz/org/design/web/posts/20211101_picture_language_matplotlib.org --eval '(progn (setq default-directory \"/home/pipz/codes/hugchangelife/posts\") (setq publish-directory \"/home/pipz/codes/hugchangelife\") (org-html-export-to-html))' --kill
    """
    elisp_code = f"""
    (progn 
        (setq default-directory "{target_folder}") 
        (setq publish-directory "{www_folder}") 
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
    - generate  target_folder from www_folder and target_file_path
    - call export_to_html to generate html file
    - call extract_links_from_html to get all images file path
    - call rsync_copy to copy all images to www_folder
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
    export_func = export_to_single_html
    target_file_pathes = [html_path_abs2sys]
    if list_index:
        export_func = export_to_multiple_htmls
        # get all html fils under target_folder
        target_file_pathes = glob.glob(os.path.join(html_folder, "*.html"))

    export_func(
        org_path_abs2sys,
        html_folder,
        theme,
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


def generate_index_html(config, info, www_folder):
    """
    generate index.html from index.org
    """
    index = info["index_template"]
    env = Environment(loader=FileSystemLoader(os.path.dirname(index)))
    template = env.get_template(os.path.basename(index))
    visible_posts = [x for x in info["posts"] if not x.get("draft", False)]
    data = {
        "year": datetime.datetime.now().year,
        "posts": visible_posts,
    }

    for key in ["beian", "sitename", "github_url", "github_name"]:
        if key in config:
            data[key] = config[key]

    rendered_template = template.render(data)
    index_html = os.path.join(www_folder, "index.html")
    with open(index_html, "w") as f:
        f.write(rendered_template)
        print(f"{index_html} generated")


def generate_category_html(config, info, www_folder):
    """
    generate categories/tag.html from info
    """
    index = info["index_template"]

    env = Environment(loader=FileSystemLoader(os.path.dirname(index)))
    category_template = "category.html"
    template = env.get_template(category_template)
    categories_dir = os.path.join(www_folder, "categories")
    if not os.path.exists(categories_dir):
        os.makedirs(categories_dir)

    for category in info["categories"]:
        data = {
            "year": datetime.datetime.now().year,
            "section": category,
            "posts": info["categories"][category],
        }

        for key in ["beian", "sitename", "github_url", "github_name"]:
            if key in config:
                data[key] = config[key]

        rendered_template = template.render(data)

        with open(
            os.path.join(www_folder, "categories", f"{category}.html"), "w"
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
        www_folder, "categories", "index.html"
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

    for i, post_info in enumerate(meta["posts"]):
        post_info["context"] = {}
        context = post_info.get("context")
        if "site_repo" in config:
            site_repo = config["site_repo"]
            context["github_issue_link"] = os.path.join(
                site_repo, "issues/new"
            )
        context["categories"] = ",".join(post_info["categories"])

        publish_single_file(post_info, publish_folder, verbose)

    single_page_postprocessing(meta)

    generate_index_html(config, meta, publish_folder)

    generate_category_html(config, meta, publish_folder)


if __name__ == "__main__":
    import doctest

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
