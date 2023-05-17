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

MAIN_DIR = os.path.dirname(os.path.realpath(sys.argv[0]))
os.chdir(MAIN_DIR)


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


def export_to_html(
    orgfile, target_folder, theme, www_folder, verbose, **kwargs
) -> str:
    """
    call org-html-export-to-html on `orgfile`, gerenating html file in `target_folder` using `theme`

    >>> export_to_html('../tests/demo.org', '/tmp', 'darkfloat')

    test cmdline
    emacs --batch --chdir=/data/codes/hugchangelife/orgchange/themes/darkfloat --load export.el /home/pipz/org/design/web/posts/20211101_picture_language_matplotlib.org --eval '(progn (setq default-directory \"/home/pipz/codes/hugchangelife/posts\") (setq publish-directory \"/home/pipz/codes/hugchangelife\") (org-html-export-to-html))' --kill
    """
    eval_elisp = f"""
    (progn 
        (setq default-directory "{target_folder}") 
        (setq publish-directory "{www_folder}") 
        (setq categories "{kwargs.get('categories', '')}") 
        (setq prev-link "{kwargs.get('prev_link', '#')}") 
        (setq prev-title "{kwargs.get('prev_title', '')}") 
        (setq next-link "{kwargs.get('next_link', '#')}") 
        (setq next-title "{kwargs.get('next_title', '')}") 
        (setq github-issue-link "{kwargs.get('github_issue_link', '#')}") 
        (org-html-export-to-html)),
    """

    cmd = [
        "emacs",
        "--batch",
        f"--chdir={MAIN_DIR}/themes/{theme}",
        "--load",
        "export.el",
        orgfile,
        "--eval",
        eval_elisp,
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

    html_path = orgfile.replace(".org", ".html")
    target_path = os.path.join(target_folder, os.path.basename(html_path))
    if not os.path.exists(target_path):
        print(f'failed on\n{" ".join(cmd)}')
    return target_path


def is_valid_orgpath(path):
    """
    check if path is valid

    >>> is_valid_orgpath('/usr/bin/python')
    False
    >>> is_valid_orgpath('/tmp/a')
    False
    """

    return os.path.exists(path) and path.endswith(".org")


def extract_meta_from_index_org(orgfile, default_theme="darkfloat"):
    """
    extract metadata from index.org, return a dict
    get all child nodes from a level1 1 node with tag 'post'

    >>> extract_meta_from_index_org('tests/index.org')
    {'posts': [{'path': '/home/pipz/codes/orgpost/tests/demo.org', 'theme': 'darkfloat', 'categories': ['sample', 'test']}, {'path': '/home/pipz/codes/orgpost/tests/index.org', 'theme': 'darkfloat', 'categories': ['']}]}
    """

    org = orgparse.load(orgfile)
    theme = "THEME"
    categories = "CATEGORIES"
    posts = []
    base_dir = os.path.dirname(orgfile)
    for node in org:
        # ignore nodes with noexport tag
        if "noexport" in node.tags:
            continue
        if "post" in node.tags and node.level == 2:
            heading = node.get_heading(format="raw")
            path = get_path_from_orglink(heading)
            path = os.path.join(base_dir, path)
            title = get_title_from_orglink(heading)

            if is_valid_orgpath(path):
                posts.append(
                    {
                        "path": os.path.abspath(path),
                        "theme": node.get_property(theme, default_theme),
                        "categories": [
                            x.strip()
                            for x in node.get_property(categories, "").split(
                                ","
                            )
                        ],
                        "title": title,
                    }
                )

    return {"posts": posts}


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


def extract_links_from_html(path):
    with open(path, "r") as f:
        html_content = f.read()

    # Parse the HTML content using BeautifulSoup
    soup = BeautifulSoup(html_content, "html.parser")

    # Find all image tags in the HTML document
    img_tags = soup.find_all("img")

    # Extract the image URLs from the image tags using regular expressions
    img_urls = [img["src"] for img in img_tags if "src" in img.attrs]
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


def publish_single_file(
    target_file_path, publish_info, www_folder, verbose=False
):
    """
    publish a single org file:
    - generate  target_folder from www_folder and target_file_path
    - call export_to_html to generate html file
    - call extract_links_from_html to get all images file path
    - call rsync_copy to copy all images to www_folder
    """
    filepath = publish_info["path"]

    # e.g. ~/org/posts/
    src_folder = os.path.dirname(filepath)

    # e.g. /www/posts/
    target_folder = os.path.dirname(target_file_path)
    if not os.path.exists(target_folder):
        os.makedirs(target_folder)

    theme = publish_info.get("theme")
    export_to_html(
        filepath,
        target_folder,
        theme,
        www_folder,
        verbose,
        **publish_info.get("context", {}),
    )
    img_urls = extract_links_from_html(target_file_path)
    for img_url in img_urls:
        with change_dir(src_folder):
            # mv url from ~/org/posts/imgs/... to /www/posts/imgs/...
            rsync_copy(img_url, target_folder)

    return target_file_path


def generate_index_html(config, info, www_folder):
    """
    generate index.html from index.org
    """
    index = info["index_template"]
    env = Environment(loader=FileSystemLoader(os.path.dirname(index)))
    template = env.get_template(os.path.basename(index))
    data = {
        "year": datetime.datetime.now().year,
        "posts": info["posts"],
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


def normalize_path(path: str) -> str:
    """
    normalize path to absolute path
    """
    if path.startswith("~"):
        path = os.path.expanduser(path)
    return os.path.abspath(path)


def publish_via_index(config, verbose=False):
    """
    publish all valid posts mentioned in index.org
    """
    index_org = normalize_path(config["index_org"])
    www_folder = normalize_path(config.get("publish_folder", "/tmp"))
    prefixes = format_prefixes(config["org_prefixes"])

    meta = extract_meta_from_index_org(
        index_org, config.get("default_theme", "darkfloat")
    )

    meta["index_template"] = config["index_template"]
    meta["categories"] = defaultdict(list)

    if www_folder is None:
        www_folder = MAIN_DIR

    # first pass on all posts, generate html relative path
    for i, post_info in enumerate(meta["posts"]):
        # e.g. ./posts/a.org
        suffix = extract_suffix(post_info["path"], prefixes)

        # e.g. /www/posts/a.html
        html_path = os.path.join(www_folder, suffix).replace(".org", ".html")
        html_relative_path = extract_suffix_from_prefix(html_path, www_folder)

        meta["posts"][i]["html_relative_path"] = html_relative_path
        for category in post_info["categories"]:
            if category == "":
                continue
            meta["categories"][category].append(
                {
                    "html_relative_path": os.path.relpath(
                        html_relative_path, "categories"
                    ),
                    "title": post_info["title"],
                }
            )

    for i, post_info in enumerate(meta["posts"]):
        post_info["context"] = {}
        context = post_info.get("context")
        if i > 0:
            context["prev_link"] = os.path.join(
                "/",
                meta["posts"][i - 1]["html_relative_path"],
            )
            context["prev_title"] = meta["posts"][i - 1]["title"]
        if i < len(meta["posts"]) - 1:
            context["next_link"] = os.path.join(
                "/",
                meta["posts"][i + 1]["html_relative_path"],
            )
            context["next_title"] = meta["posts"][i + 1]["title"]
        if "site_repo" in config:
            site_repo = config["site_repo"]
            context["github_issue_link"] = os.path.join(
                site_repo, "issues/new"
            )
        context["categories"] = ",".join(post_info["categories"])
        html_path = os.path.join(www_folder, post_info["html_relative_path"])

        publish_single_file(html_path, post_info, www_folder, verbose)
        print("published to {}".format(html_path))

    generate_index_html(config, meta, www_folder)

    generate_category_html(config, meta, www_folder)


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
