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
    extract_and_cache_time_vers_des,
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
    get_emacs_org_version,
)
import pickle
from feeds import generate_feed


ORG_CHANGE_DIR = os.path.dirname(os.path.realpath(sys.argv[0]))
os.chdir(ORG_CHANGE_DIR)
WWW = os.path.dirname(ORG_CHANGE_DIR)
CACHE_PATH = os.path.join(WWW, ".orgchange_cache")
global_cache = {}
if os.path.exists(CACHE_PATH):
    print_yellow(f"load cache from {CACHE_PATH}")
    with open(CACHE_PATH, "rb") as f:
        global_cache = pickle.load(f)


def is_project(post_info):
    return post_info.get("project", False)


from utils import sync_project


def publish_via_index(index_org, verbose=False, republish_all=False):
    """
    publish all valid posts mentioned in index.org
    """

    site_info = extract_site_info_from_index_org(
        index_org, republish_all=republish_all
    )

    last_index_modified = global_cache.get(index_org, None)
    index_time = os.path.getmtime(index_org)
    global_cache[index_org] = index_time

    if last_index_modified is not None:
        if index_time > last_index_modified:
            site_info["need_update"] = True

    # collect categories and publish
    for post_info in site_info["posts"]:
        # e.g. ./posts/a.org
        for category in post_info.get("categories", []):
            if category == "":
                continue
            # distinguish with categories in post_info
            site_info["categories_map"][category].append(post_info)
        if is_project(post_info):
            sync_project(post_info)
        elif post_info["need_update"]:
            publish_single_file(post_info, verbose)
            post_info["soup"] = get_soups([post_info["html_path_abs2sys"]])[0]
        else:
            print_green(f"no update, skip {post_info['html_path_abs2www']}")

    # https://emojicombos.com/deco-ascii-art
    line = """
     ✯¸.•´*¨`*•✿ Org Change ✿•*`¨*`•.¸✯
    """

    print(line)
    # get timestamp, merge toc, save draft list
    single_page_postprocessing(site_info)

    generate_feed(site_info)
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
    {'posts': [{'path': '/home/pipz/codes/orgpost/tests/demo.org', 'theme': 'darkfloat', 'categories': ['sample', 'test']},
               {'path': '/home/pipz/codes/orgpost/tests/index.org', 'theme': 'darkfloat', 'categories': ['']}]}
    """

    with change_dir(os.path.dirname(orgfile)):
        org = orgparse.load(orgfile)
        posts = []
        site_info = {"year": datetime.datetime.now().year, "abouts": []}
        i = 0

        for node in org:
            if "noexport" in node.tags:
                continue
            if "post" in node.tags and node.level == 1:
                site_info = update_site_info(node, site_info)
                assert (
                    site_info != {}
                ), f"please define site level config in python src block under {node.heading}"
            elif "post" in node.tags and node.level == 2:
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
            elif "about" in node.tags and node.level == 2:
                heading = node.get_heading(format="raw")
                body = node.get_body()
                site_info["abouts"].append((heading, body))

        site_info["posts"] = posts

    need_update_propagate(site_info)
    return site_info


def need_update_propagate(site_info):
    """
    if a post need to be updated, all its neighobr post which is not draft should be updated too
    """
    need_update_ids = []
    n = len(site_info["posts"]) - 1
    for i, post_info in enumerate(site_info["posts"]):
        if not post_info["need_update"] or post_info.get("draft", False):
            continue
        path, title = post_info["html_path_abs2www"], post_info["title"]
        if i > 0 and not site_info["posts"][i - 1].get("draft", False):
            prev_path = site_info["posts"][i - 1]["html_path_abs2sys"]
            prev_cache = global_cache.get(prev_path, {})
            if prev_cache.get("next", None) != (path, title):
                need_update_ids.append(i - 1)
        if i < n and not site_info["posts"][i + 1].get("draft", False):
            next_path = site_info["posts"][i + 1]["html_path_abs2sys"]
            next_cache = global_cache.get(next_path, {})
            if next_cache.get("prev", None) != (path, title):
                need_update_ids.append(i + 1)
    for i in need_update_ids:
        site_info["posts"][i]["need_update"] = True


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
    site_info["emacs"] = site_info.get("emacs", "emacs")
    if site_info["emacs"].startswith("~"):
        site_info["emacs"] = os.path.expanduser(site_info["emacs"])
    site_info["need_update"] = False
    # inverse map from id to html path
    site_info["id_map"] = {}
    # inverse map from theoritical org-export html path to real publish path
    site_info["html_map"] = {}
    site_info["categories_map"] = defaultdict(list)
    site_info["emacs_org_version"] = get_emacs_org_version(site_info["emacs"])
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
    html_folder_rel2publish = post_info.get("target_dir", "")
    heading = node.get_heading(format="raw")
    title = get_title_from_orglink(heading)
    is_force_update = title.startswith("-") or post_info.get("force", False)
    title = title.strip("-")

    org_path_abs2sys = normalize_path(get_path_from_orglink(heading))
    if not is_valid_orgpath(org_path_abs2sys):
        # if is folder
        if os.path.isdir(org_path_abs2sys):
            index = f"{org_path_abs2sys}/index.html"
            if os.path.exists(index):
                post_info["project"] = True
                print_yellow(
                    f"{org_path_abs2sys} is a project folder with index.html"
                )
                org_path_abs2sys = index
            else:
                print_yellow(
                    f"orgchange will publish all org files under this directory, without recursion"
                )
        else:

            print_red(f"invalid org path: {org_path_abs2sys}, skip")

            return None

    org_path_rel2prefix = extract_suffix(org_path_abs2sys, prefixes)

    html_path_rel2publish = org_path_rel2prefix.replace(".org", ".html")
    html_path_abs2sys = os.path.join(
        publish_folder, html_folder_rel2publish, html_path_rel2publish
    )

    multipage_index = post_info.get("multipage_index", False)
    html_path_theoritical = os.path.basename(html_path_abs2sys)
    if multipage_index:
        # e.g. ~/www/posts/book/index.html
        html_path_abs2sys = html_path_abs2sys.replace(".html", "/index.html")
    html_path_rel2www = extract_suffix_from_prefix(html_path_abs2sys, WWW)
    html_path_abs2www = "/" + html_path_rel2www

    # 是否强制更新

    is_modified = do_need_modified(
        post_info["theme"],
        org_path_abs2sys,
        html_path_abs2sys,
    )

    post_info["need_update"] = is_force_update or is_modified

    if post_info["emacs"].startswith("~"):
        post_info["emacs"] = os.path.expanduser(post_info["emacs"])
    post_info["emacs_org_version"] = get_emacs_org_version(post_info["emacs"])

    post_info.update(
        {
            "title": title,
            "html_path_theoritical": html_path_theoritical,
            "html_path_abs2sys": html_path_abs2sys,
            "html_path_abs2www": html_path_abs2www,
            "org_path_abs2sys": org_path_abs2sys,
            "html_path_rel2publish": html_path_rel2publish,
        }
    )
    post_info["html_map"][html_path_theoritical] = html_path_abs2www
    return post_info


def index_node_process(node, post_info):
    src_blocks = extract_code_blocks(node.get_body())
    for language, content in src_blocks:
        if language == "python":
            variable_setting = get_bindings_from_text(content["body"])
            # if value is a path starts with ~, expand it
            for key, value in variable_setting.items():
                if key == "org_prefixes":
                    variable_setting[key] = [
                        os.path.expanduser(p) for p in value
                    ]

            post_info.update(variable_setting)
        if language == "emacs-lisp":
            post_info["user_elisp"] += content["body"]

    return post_title_path_prepare(node, post_info)


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
) -> Union[None, List[BeautifulSoup]]:
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
            # if file is a folder , skip
            if os.path.isdir(file):
                continue
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
    emacs = post_info.get("emacs", "emacs")
    _export_to_html(emacs, theme_path, orgfile, elisp_code, verbose=verbose)
    if multipage_index:
        multipages_prepare(post_info, html_folder)
    print("published to {}".format(html_path_abs2sys))


def _export_to_html(emacs, theme_path, orgfile, elisp_code, verbose=False):
    """
    a wrapper of org-html-export-to-html
    """
    cmd = [
        f"{emacs}",
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
    def _sort_by_end_num(filename):
        # remove suffix
        basename = os.path.basename(filename)[: -len(".html")]

        return int(basename.split("_")[-1])

    html_folder_rel2www = extract_suffix_from_prefix(html_folder, WWW)
    html_folder_abs2www = "/" + html_folder_rel2www
    with change_dir(html_folder):
        html_files = glob.glob("*.html")
        html_files_without_index = [
            html for html in html_files if not html.endswith("index.html")
        ]
        html_files = ["index.html"] + sorted(
            html_files_without_index, key=_sort_by_end_num
        )
        soups = get_soups(html_files)

        titles = get_multipages_titles(soups)
        # add_article_footer(html_files, soups, titles)
        posts = []
        for file, soup, title in zip(html_files, soups, titles):
            if file.endswith("index.html"):
                # index.html is the first page of multipages
                sub_post_info = post_info
            else:
                sub_post_info = copy.copy(post_info)
                sub_post_info["title"] = str(title)

            sub_post_info["soup"] = soup

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


def collect_prev_next_and_generate(posts, cache):
    """
    if not draft, chain each post with prev and next post
    """
    for i, post_info in enumerate(posts):
        post_info["next"] = ("", "")
        post_info["prev"] = ("", "")
        path = post_info["html_path_abs2sys"]
        if not post_info.get("draft", False):
            if i < len(posts) - 1:
                post_info["next"] = (
                    posts[i + 1]["html_path_abs2www"],
                    posts[i + 1]["title"],
                )
                cache[path]["next"] = post_info["next"]
            if i > 0:
                post_info["prev"] = (
                    posts[i - 1]["html_path_abs2www"],
                    posts[i - 1]["title"],
                )
                cache[path]["prev"] = post_info["prev"]

        if post_info["need_update"] and not is_project(post_info):
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
            if not post.get("draft", False):
                all_visible_posts.extend(post["multipages"])
            else:
                all_draft_posts.extend(post["multipages"])
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


def save_draft_html_path_to_file(draft_posts):
    """
    save absolute html path of draft post in posts to a file
    .orgchange.draft
    """
    draft_html_paths = [
        post["html_path_abs2www"][1:]
        for post in draft_posts
        if post.get("categories", []) == []
    ]
    draft_file_path = os.path.join(WWW, ".orgchange.draft")
    with open(draft_file_path, "w") as f:
        f.write("\n".join(draft_html_paths))
    print_yellow(f"draft html paths saved at {draft_file_path}")

    # also save to ".git/info/exclude"
    git_exclude_path = os.path.join(WWW, ".git", "info", "exclude")
    with open(git_exclude_path, "w") as f:
        f.write("\n".join(draft_html_paths))
    print_yellow(f"add .orgchange.draft to git exclude")


def single_page_postprocessing(site_info):
    all_posts = separate_draft_posts(site_info)

    for post_info in all_posts:
        # 对所有 post 都提取时间信息（或者从缓存读取），用于显示在 post ，index list 和 category list 页面
        extract_and_cache_time_vers_des(post_info, cache=global_cache)
        if post_info["need_update"]:
            soup_decorate_per_html(post_info)

    merge_anthology_toc(site_info)
    collect_prev_next_and_generate(site_info["all_draft_posts"], global_cache)
    collect_prev_next_and_generate(
        site_info["all_visible_posts"], global_cache
    )

    with open(CACHE_PATH, "wb") as f:
        print_yellow(f"dump cache to {CACHE_PATH}")
        pickle.dump(global_cache, f)

    save_draft_html_path_to_file(site_info["all_draft_posts"])


def generate_post_page(post_info):
    """
    generate articles using jinja2 template
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

    emacs_scripts = post_info["soup"].find("head").find_all("script")
    if emacs_scripts:
        post_info["emacs_scripts"] = "\n".join(
            [str(script) for script in emacs_scripts]
        )

    org_main = post_info["soup"].find("div", {"id": "org-main"})

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

    # delete htmls that not in categories_map
    for file in os.listdir(os.path.join(publish_folder, "categories")):
        if (
            file.endswith(".html")
            and file.split(".")[0] not in site_info["categories_map"]
            and file != "index.html"
        ):
            os.remove(os.path.join(publish_folder, "categories", file))
            print_yellow(f"delete extra {file}")

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
