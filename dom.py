import os
from pygments import highlight
from pygments.lexers import get_lexer_by_name
from pygments.formatters import HtmlFormatter
from bs4 import BeautifulSoup, NavigableString
import html

lisp_family = [
    "scheme",
    "lisp",
    "racket",
    "elisp",
    "emacs-lisp",
    "elisp",
]


def pygment_and_paren_match_all(soup, class_filters=[]):
    for pre in list(soup.find_all("pre")):
        # if div has a <code> child
        if not pre.code:
            continue
        lang = "example"
        jupyter_python = False
        try:
            lang = pre.code["class"][0]
            if lang == "jupyter-python":
                jupyter_python = True
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
        if lang in lisp_family or jupyter_python:
            normalize_pre = normalize_span(soup_code.pre)
            paren = "[]" if jupyter_python else "()"
            paren_matched_pre = paren_match(normalize_pre, paren=paren)
            soup_code.pre.replace_with(paren_matched_pre)

        soup_code.pre["class"] = [lang]
        pre.replace_with(soup_code)

    return soup


def paren_match(pre, paren="()"):
    """
    div is a <pre> tag, and we want to wrap parentheses in spans
    all children of div are <span> tags thanks to pygments
    """
    soup = BeautifulSoup("", "html.parser")

    stack = []
    # open_index is the index of previous open parenthesis
    open_index_stack = []

    for child in list(pre.children):
        # Check if both parentheses are in the same tag

        stack.append(child)
        # 不考虑 （）或 [] 情况
        # if child.string in [paren, paren+":"]:
        #     wrapper = soup.new_tag("span")
        #     wrapper["class"] = "paren"
        #     child.wrap(wrapper)
        if child.string == paren[0]:
            # push the span tag onto the stack
            open_index_stack.append(len(stack) - 1)

        elif child.string == paren[1]:
            if open_index_stack:
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


def _merge_toc(html_files, soups):
    # 创建一个字典，用于存储每个文件的table of contents元素

    tocs = []

    # 第一次遍历：获取每个文件的目录
    for file, soup in zip(html_files, soups):
        try:
            title = soup.find("h1", {"class": "title"}).contents[0]

        except AttributeError:
            print(file)
            raise AttributeError(
                f"\n--> ERROR: you may need to add a #+title in your org file: {os.path.abspath(file)}\n"
            )
        title_li_str = f'<li> <a href="{file}">{title}</a></li>'
        title_li = new_soup(title_li_str)

        local_toc = new_soup(title_li_str)

        toc = soup.find("div", {"id": "text-table-of-contents"})
        if toc:
            local_toc.append(new_soup(toc.ul))

        # print(local_toc.prettify(), title_li.prettify())
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
        paginav = soup.find("nav", {"class": "paginav"}) if soup else None
        if paginav:
            _insert_paginav(paginav, html_files, titles, i, cls="prev")
            _insert_paginav(paginav, html_files, titles, i, cls="next")
