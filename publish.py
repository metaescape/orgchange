import subprocess
import os
import sys
import shutil
import orgparse
import re
from bs4 import BeautifulSoup
import json
import argparse


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

current_file_dir = os.path.dirname(os.path.realpath(sys.argv[0]))
MAIN_DIR = os.path.dirname(current_file_dir)

DEFAULT_THEME = "darkfloat"
CONFIG_FILE = os.path.join(current_file_dir, "config.json")


def read_config():
    with open(CONFIG_FILE, "r") as f:
        return json.load(f)


config = {} if not os.path.exists(CONFIG_FILE) else read_config()


def export_to_html(orgfile, target_folder, theme) -> str:
    """
    call org-html-export-to-html on `orgfile`, gerenating html file in `target_folder` using `theme`

    >>> export_to_html('../tests/demo.org', '/tmp', 'darkfloat')
    """

    cmd = [
        "emacs",
        "--batch",
        f"--chdir={current_file_dir}/themes/{theme}",
        "--load",
        "export.el",
        orgfile,
        "--eval",
        f'(progn (setq default-directory "{target_folder}") (org-html-export-to-html))',
        "--kill",
    ]
    process = subprocess.Popen(
        cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE
    )
    output, error = process.communicate()

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


def extract_meta_from_index_org(orgfile):
    """
    extract metadata from index.org, return a dict
    get all child nodes from a level1 1 node with tag 'post'

    >>> extract_meta_from_index_org('tests/index.org')
    {'posts': [{'path': '/home/pipz/codes/orgpost/tests/demo.org', 'theme': 'darkfloat', 'categories': ['sample', 'test'], 'html_name': 'test_demo'}, {'path': '/home/pipz/codes/orgpost/tests/index.org', 'theme': 'darkfloat', 'categories': [''], 'html_name': 'index.html'}]}
    """

    org = orgparse.load(orgfile)
    theme = "THEME"
    html_name = "HTML_NAME"
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
            if is_valid_orgpath(path):
                posts.append(
                    {
                        "path": os.path.abspath(path),
                        "theme": node.get_property(theme, DEFAULT_THEME),
                        "categories": node.get_property(categories, "").split(
                            ","
                        ),
                        "html_name": node.get_property(
                            html_name,
                            os.path.basename(path).replace(".org", ".html"),
                        ),
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
    return RE_LINK.sub(
        lambda m: m.group("desc0") or m.group("link1"), raw_link
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
        prefix = os.path.expanduser(prefix)
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


def publish_single_file(prefixes, publish_info, www_folder):
    """
    publish a single org file:
    - get path suffix from filepath via prefix_folder
    - generate target_file_path and target_folder from www_folder and suffix
    - call export_to_html to generate html file
    - call extract_links_from_html to get all images file path
    - call rsync_copy to copy all images to www_folder
    - rename html file to html_name if html_name is specified in index org_file
    """
    filepath = publish_info["path"]
    suffix = extract_suffix(filepath, prefixes)

    target_file_path = os.path.join(www_folder, suffix).replace(
        ".org", ".html"
    )
    src_folder = os.path.dirname(filepath)
    target_folder = os.path.dirname(target_file_path)
    if not os.path.exists(target_folder):
        os.makedirs(target_folder)

    export_to_html(filepath, target_folder, publish_info["theme"])
    img_urls = extract_links_from_html(target_file_path)
    for img_url in img_urls:
        rsync_copy(os.path.join(src_folder, img_url), www_folder)

    # rename html file to html_name if html_name is specified in publish_info
    if "html_name" in publish_info:
        html_name = publish_info["html_name"]
        if not html_name.endswith(".html"):
            html_name += ".html"
        html_path = os.path.join(target_folder, html_name)
        shutil.move(target_file_path, html_path)
        return html_path
    return target_file_path


def publish_via_index(index_org, www_folder=None):
    """
    publish all valid posts mentioned in index.org
    """
    prefixes = format_prefixes(config["org_prefixes"])

    meta = extract_meta_from_index_org(index_org)
    if www_folder is None:
        www_folder = MAIN_DIR
    for post in meta["posts"]:
        path = publish_single_file(prefixes, post, www_folder)
        print("published to {}".format(path))


if __name__ == "__main__":
    import doctest

    # doctest.testmod()

    parser = argparse.ArgumentParser(description="Publish website")

    # Add arguments for index file and publish folder
    parser.add_argument(
        "index",
        metavar="index_file",
        nargs="?",
        type=str,
        default="tests/index.org",
        help="path to the index org file",
    )
    parser.add_argument(
        "publish",
        metavar="publish_folder",
        nargs="?",
        type=str,
        default="/tmp/www",
        help="path to the folder to publish the website",
    )

    # Parse the command-line arguments
    args = parser.parse_args()

    # Get the values of the index file and publish folder
    index_file = args.index
    publish_folder = args.publish
    publish_via_index(index_file, publish_folder)
