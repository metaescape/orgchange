import os
from pygments import highlight
from pygments.lexers import get_lexer_by_name
from pygments.formatters import HtmlFormatter
from bs4 import BeautifulSoup, NavigableString
import html
from utils import change_dir, print_green, rsync_copy, print_yellow, print_red
from datetime import datetime, timedelta
import traceback

# 防止 debug 时，soup 会被打印出来，导致 terminal 内容过多
BeautifulSoup.__repr__ = (
    lambda self: "use .decode() or .prettify() to view the content"
)

lisp_family = [
    "scheme",
    "lisp",
    "racket",
    "elisp",
    "emacs-lisp",
    "elisp",
]


def pygment_and_paren_match_all(soup, rainbow=True):
    for pre in list(soup.find_all("pre")):
        # if div has a <code> child
        if not pre.code:
            continue
        src = "demo"  # showed in the html <code>
        jupyter_python = False
        try:
            src = pre.code["class"][0]
            lang = src
            if src == "jupyter-python":
                jupyter_python = True
                lang = "python"
                src = "python"
            if src == "org":
                lang = "python"
            lexer = get_lexer_by_name(lang)
        except:
            continue

        formatter = HtmlFormatter()

        code = pre.get_text()
        escaped_code = html.escape(code)
        escaped_code = code

        highlighted_code = highlight(escaped_code, lexer, formatter)

        soup_code = BeautifulSoup(highlighted_code, "html.parser")
        if rainbow and (lang in lisp_family or jupyter_python):
            normalize_pre = normalize_span(soup_code.pre)
            parens = ["[]"] if jupyter_python else ["()"]
            if type(rainbow) == dict:
                parens = rainbow.get(src, parens)

            paren_matched_pre = paren_match(normalize_pre, parens)
            soup_code.pre.replace_with(paren_matched_pre)

        soup_code.pre["class"] = [src]
        pre.replace_with(soup_code)

    return soup


def paren_match(pre, parens=["[]", "()"]):
    """
    div is a <pre> tag, and we want to wrap parentheses in spans
    all children of div are <span> tags thanks to pygments
    """
    if type(parens) == str:
        parens = [parens]
    soup = BeautifulSoup("", "html.parser")

    paren_map = {p[0]: p[1] for p in parens}

    stack = []
    # open_index is the index of previous open parenthesis
    open_index_stack = []
    paren_stack = []

    for child in list(pre.children):
        # Check if both parentheses are in the same tag

        stack.append(child)
        if child.string in paren_map:
            paren_stack.append(child.string)
            # push the span tag onto the stack
            open_index_stack.append(len(stack) - 1)

        elif child.string in paren_map.values():
            if open_index_stack and paren_map[paren_stack[-1]] == child.string:
                paren_stack.pop()
                open_index = open_index_stack.pop()

                wrapper = soup.new_tag("span")
                wrapper["class"] = [
                    "paren",
                    f"plevel-{len(open_index_stack) + 1}",
                ]
                temp = stack[open_index:]
                for span in temp:
                    wrapper.append(span.extract())  # extract 会删除原来的标签

                # Insert the wrapper div into the original div
                pre.insert(open_index, wrapper)

                stack = stack[:open_index]
                stack.append(wrapper)

    return pre


def normalize_span(pre):
    soup = BeautifulSoup("", "html.parser")
    new_pre = soup.new_tag("pre")
    for child in list(pre.children):
        if isinstance(child, NavigableString):
            new_pre.append(child)
        elif not child.has_attr("class") or "p" not in child["class"]:
            new_pre.append(child)
        else:
            # 遍历 child 内容中的每个字符
            for char in child.string:
                new_span = soup.new_tag("span")  # 创建一个新的 span 标签
                if char in "(){}[]":
                    new_span["class"] = "p"
                new_span.string = char  # 将当前字符作为新 span 标签的内容
                new_pre.append(new_span)  # 将新 span 标签添加到 new_soup 中

    return new_pre


def new_soup(soup):
    return BeautifulSoup(str(soup), "html.parser")


def get_soups(html_files):
    soups = []
    for file in html_files:
        with open(file, "r") as f:
            soup = BeautifulSoup(f.read(), "html.parser")
            soups.append(soup)
    return soups


def merge_toc(posts):
    tocs = []

    # 第一次遍历：获取每个文件的目录
    for post_info in posts:
        title = post_info["title"]
        html_path = post_info["html_path_abs2www"]

        title_li_str = f'<li> <a href="{html_path}">{title}</a></li>'
        title_li = new_soup(title_li_str)

        local_toc = new_soup(title_li_str)

        soup = post_info["soup"] if "soup" in post_info else None
        if soup:
            toc = soup.find("div", {"id": "text-table-of-contents"})
            if toc:
                local_toc.append(new_soup(toc.ul))

        # print(local_toc.prettify(), title_li.prettify())
        tocs.append((local_toc, title_li))

    # second pass, merage each tocs
    for i, (local_toc, title_li) in enumerate(tocs):
        global_toc_ul_str = f"""
        <ul>
        </ul>    
        """

        global_toc_ul = BeautifulSoup(global_toc_ul_str, "html.parser")
        global_toc_ul.extend([new_soup(x[-1]) for x in tocs[:i]])
        global_toc_ul.append(local_toc)
        global_toc_ul.extend([new_soup(x[-1]) for x in tocs[i + 1 :]])
        posts[i]["global_toc"] = str(global_toc_ul)

    # for file, soup, global_toc in zip(html_files, soups, global_tocs):
    #     # print(global_toc)

    #     container = soup.find("div", {"class": "container"})
    #     old_global_toc = container.find("nav", {"id": "global-toc"})
    #     if old_global_toc:
    #         old_global_toc.replace_with(global_toc)
    #     else:
    #         container.insert(0, new_soup(global_toc))
    #     # 如果不用 new_soup,  global_toc 会被清空，为什么？


def insert_paginav(paginav, links, titles, i, cls="prev"):
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


def get_multipages_titles(soups):
    return [
        soup.find("section", {"class": "outline-2"}).find("h2").string
        for soup in soups
    ]


def add_article_footer(html_files, soups, titles=[]):
    """
    add article footer to each html file
    """
    n = len(html_files)
    for i in range(n):
        _, soup = html_files[i], soups[i]
        paginav = soup.find("nav", {"class": "paginav"}) if soup else None
        if paginav:
            insert_paginav(paginav, html_files, titles, i, cls="prev")
            insert_paginav(paginav, html_files, titles, i, cls="next")


def remove_user_prefix(path, prefixes):
    # Remove the .. and . parts directly

    return path


def mermaid_process(soup, theme):
    """
    check if there is a <code class="mermaid"> tag
    if exist add
      <script type="module">
    import mermaid from 'https://cdn.jsdelivr.net/npm/mermaid@10/dist/mermaid.esm.min.mjs';
    </script>
    before <body/> end
    """
    mermaid_code = soup.find("code", {"class": "mermaid"})
    if mermaid_code:
        # add <script> before body end
        script_tag = soup.new_tag("script", type="module")
        script_tag.string = (
            """
        import mermaid from 'https://cdn.jsdelivr.net/npm/mermaid@10/dist/mermaid.esm.min.mjs';
        mermaid.initialize({
        securityLevel: 'loose',
        theme: 
        """
            + f"'{theme}',"
            + "});"
        )

        return str(script_tag)
    return ""


def check_id(href, post_info):
    # if find "#ID-" in href, then split
    if href.find("#ID-") != -1:
        html, org_id = href.split("#ID-")
        if org_id in post_info["id_map"]:
            href = post_info["id_map"][org_id] + "#" + org_id
            return href
        if os.path.basename(html) in post_info["html_map"]:
            href = post_info["html_map"][os.path.basename(html)] + "#" + org_id
            return href
        return ""
    return href


def soup_decorate_per_html(post_info):
    """
    extract images from html soups for later rsync
    prune the cross reference links in href
    add code highlight with pygments
    """
    soup = post_info.get("soup", None)
    if soup is None:
        return
    link_replace, prefixes = (
        post_info.get("link_replace", {}),
        post_info["org_prefixes"],
    )

    # Find all image tags in the HTML document
    img_tags = soup.find_all("img")
    # Extract the image URLs from the image tags using regular expressions
    img_urls = [img["src"] for img in img_tags if "src" in img.attrs]

    # filter out the images with http prefix
    img_urls = [url for url in img_urls if not url.startswith("http")]

    # e.g. ~/org/posts/
    org_folder = os.path.dirname(post_info["org_path_abs2sys"])
    # e.g. /www/posts/
    html_folder = os.path.dirname(post_info["html_path_abs2sys"])
    publish_folder = post_info["publish_folder"]

    for img_url in img_urls:
        with change_dir(org_folder):
            # mv url from ~/org/posts/imgs/... to /www/posts/imgs/...
            rsync_copy(img_url, html_folder, root=publish_folder)

    for a in soup.find_all("a"):

        if not a.has_attr("href"):
            continue
        href = a["href"]

        if href.startswith("file:"):
            href = os.path.normpath(href)
            for src, target in link_replace.items():
                src = r"file:" + os.path.expanduser(src)
                href = href.replace(src, target)

            href = (
                href.replace("..", "")
                .replace("./", "")
                .replace(os.path.expanduser("~"), "local")
            )

        # if path is a http link, then skip, this is comment-like code
        elif href.startswith("http") or href.startswith("#"):
            pass

        href = check_id(href, post_info)

        # 删除 "#MissingReference"
        href = href.replace("#MissingReference", "")

        # 更新 href 属性
        a["href"] = href

    soup = self_apply(soup)
    post_info.get("")
    soup = pygment_and_paren_match_all(soup, post_info.get("rainbow", True))
    post_info["soup"] = soup

    post_info["mermaid_script"] = mermaid_process(
        soup, post_info.get("mermaid_theme", "neutral")
    )


def self_apply(soup):
    # find the h1 header with content self-apply
    h2 = soup.find("h2", string="self-apply")
    if h2 is not None:
        print_yellow("find self-apply python code, executing")
        # get the parent of h2
        parent = h2.parent
        python_blocks = parent.find_all("code", {"class": "python"})

        for block in python_blocks:
            # execute python code
            try:
                exec(block.string)
                print_yellow("self-apply success")
            except Exception as e:
                # print traceback

                print_red(f"self-apply failed with error: {e}")
                print_red(traceback.format_exc())
                pass
        # delete the h1 header
        parent.decompose()
    return soup


def strip_org_timestamp(timestamp: str):
    """
    if stamp is wrapped in brace , e.g. "[2024-01-27 六 13:05]"
    then strip the brace
    """
    timestamp = timestamp.strip()
    if timestamp.startswith("[") and timestamp.endswith("]"):
        return timestamp[1:-1]
    return timestamp


def extract_and_cache_time(
    post_info, stamp_id="created_timestamp", date_id="created", cache={}
):
    # 如果已经有用户在 post python post 里面设置了时间戳，那么就不需要再去解析 html 里面的时间戳
    # 但是如果用户没有设置时间戳，那么就需要解析 html 里面的时间戳
    # 如果没有 html 或者 html 里面没有时间戳，那么就读取 cache 里面的时间戳
    # 最后更新 cache 里面的时间戳
    html_path = post_info["html_path_abs2sys"]

    # if "orgchange" in post_info["title"]:
    #     breakpoint()
    if stamp_id in post_info:
        # extract date in  in "[date]"
        timestamp = strip_org_timestamp(post_info[stamp_id])
    else:
        soup = post_info.get("soup", None)
        html_id = stamp_id.replace("_", "-")
        timestamp = get_element_by_id(soup, html_id)

    if not timestamp:
        timestamp = (
            cache[html_path][stamp_id] if html_path in cache else "chaos"
        )
    if html_path not in cache:
        cache[html_path] = {}

    date = timestamp.split()[0] if timestamp else "chaos"

    post_info[stamp_id] = timestamp
    cache[html_path][stamp_id] = timestamp
    post_info[date_id] = date
    cache[html_path][date_id] = date


def extract_and_cache_emacs_org_version(post_info, cache={}):
    html_path = post_info["html_path_abs2sys"]
    soup = post_info.get("soup", None)
    version = get_element_by_id(soup, "emacs-org-version")
    post_info["emacs_org_version"] = version
    if html_path not in cache:
        cache[html_path] = {}
    cache[html_path]["emacs_org_version"] = version


def extract_and_cache_description(post_info, cache={}):
    html_path = post_info["html_path_abs2sys"]
    soup = post_info.get("soup", None)
    version = get_element_by_id(soup, "emacs-org-version")
    post_info["description"] = version
    if html_path not in cache:
        cache[html_path] = {}
    cache[html_path]["emacs_org_version"] = version


def get_element_by_id(soup, id):
    if soup is None:
        return None
    ele = soup.find("span", {"id": id})
    if ele is None:
        return None
    return ele.text.strip()


def extract_and_cache_time_vers_des(post_info, cache={}):
    """
    prioirity: user setting > html > cache
    """
    html_path = post_info["html_path_abs2sys"]
    if html_path not in cache and "soup" not in post_info:
        post_info["soup"] = get_soups([post_info["html_path_abs2sys"]])[0]

    extract_and_cache_time(post_info, "created_timestamp", "created", cache)
    extract_and_cache_time(
        post_info, "last_modify_timestamp", "last_modify", cache
    )
    extract_and_cache_emacs_org_version(post_info, cache)
    # extract_and_cache_description(post_info, cache)


# deprecated
def merge_single_pages_footer(site_info):
    visible_soups = []
    visible_htmls = []
    visible_titles = []
    for post_info in site_info["posts"]:
        # 只有可见文章才需要添加 footer, 那些不需要更新的也要加入，提供上下文
        if not post_info.get("draft", False):
            visible_soups.append(
                None if "soup" not in post_info else post_info["soup"]
            )
            visible_htmls.append(post_info["html_path_abs2www"])
            visible_titles.append(post_info["title"])

    add_article_footer(visible_htmls, visible_soups, visible_titles)
