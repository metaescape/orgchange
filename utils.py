from contextlib import contextmanager
import functools
import glob
import json
import os
import re
import shutil

# get path of current file
ORG_CHANGE_DIR = os.path.dirname(os.path.abspath(__file__))


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

utils_last_mod_time = {}


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


def check_modified_time_hook(file1, file2, f):
    """
    check if file1 is modified after file2, if so, call f(file1, file2)
    """
    if not os.path.exists(file2) or os.path.getmtime(file1) > os.path.getmtime(
        file2
    ):
        f(file1, file2)


def rsync_copy(file_suffix, target_folder):
    target_file = os.path.join(target_folder, file_suffix)

    target_folder = os.path.dirname(target_file)

    # Create the folder if it doesn't exist
    if not os.path.exists(target_folder):
        os.makedirs(target_folder)

    # Copy the source file to the target file only if it has been modified
    check_modified_time_hook(file_suffix, target_file, shutil.copy2)


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


def get_latest_timestamp(directory, ignore=None, includes=None):
    latest_timestamp = None

    for root, dirs, files in os.walk(directory):
        # 如果 ignore 参数不为 None，则将需要忽略的文件从 files 列表中移除
        if ignore is not None:
            files = [
                file
                for file in files
                if not any(
                    glob.fnmatch.fnmatch(file, pattern) for pattern in ignore
                )
            ]

        for file in files:
            file_path = os.path.join(root, file)
            timestamp = os.path.getmtime(file_path)

            if latest_timestamp is None or timestamp > latest_timestamp:
                latest_timestamp = timestamp

    # 如果 includes 参数不为 None，则检查额外要考虑的文件路径，并更新最新修改时间戳
    if includes is not None:
        for include in includes:
            timestamp = os.path.getmtime(include)

            if latest_timestamp is None or timestamp > latest_timestamp:
                latest_timestamp = timestamp

    return latest_timestamp


def print_green(text):
    green = "\033[32m"
    reset = "\033[0m"
    print(f"{green}{text}{reset}")


def get_timestamp_of_publish_utils(theme):
    theme_dir = os.path.join(ORG_CHANGE_DIR, "themes", theme)
    if utils_last_mod_time.get(theme, None) is None:
        last_mod = get_latest_timestamp(
            theme_dir,
            ignore=["style.css"],
            includes=[
                f"{ORG_CHANGE_DIR}/themes/general.el",
                f"{ORG_CHANGE_DIR}/publish.py",
                f"{ORG_CHANGE_DIR}/utils.py",
                f"{ORG_CHANGE_DIR}/dom.py",
            ],
        )
        utils_last_mod_time[theme] = last_mod
    return utils_last_mod_time[theme]


def get_timestamp_of_orgfile(orgfile):
    return os.path.getmtime(orgfile)


def do_need_modified(theme_dir, org_path, html_path):
    latest_timestamp = max(
        get_timestamp_of_publish_utils(theme_dir),
        get_timestamp_of_orgfile(org_path),
    )
    if (
        os.path.exists(html_path)
        and os.path.getmtime(html_path) > latest_timestamp
    ):
        print_green(f"html is newer than {org_path}, skip")
        return False
    return True


def cache(html="index.html"):
    def decorator(func):
        @functools.wraps(func)  # 保留被装饰函数的元数据
        def wrapper(*args, **kwargs):
            # 在被装饰函数执行前的操作
            orgfile, html_folder, theme = args[:3]
            if html:
                html_file = os.path.join(html_folder, f"{html}")
            else:
                html_file = os.path.join(
                    html_folder,
                    os.path.basename(orgfile).replace(".org", ".html"),
                )
            # if the timestamp of html_file is newer than orgfile, skip
            latest_timestamp = max(
                get_timestamp_of_publish_utils(theme),
                get_timestamp_of_orgfile(orgfile),
            )
            if (
                os.path.exists(html_file)
                and os.path.getmtime(html_file) > latest_timestamp
            ):
                print_green(f"html is newer than {orgfile}, skip")
                return

            # 执行被装饰函数
            result = func(*args, **kwargs)

            return result

        return wrapper

    return decorator


def get_bindings_from_text(args):
    exec(args)
    return {k: v for k, v in locals().items() if k != "args"}


# 定义一个函数，用于提取所有的代码块
def extract_code_blocks(text):
    pattern = r"\#\+begin_src *([\w\s:-]+)\n(.*?)\n *\#\+end_src"
    matches = re.findall(pattern, text, re.DOTALL)
    res = []
    for match in matches:
        language, attrs = unpack_language_header(match[0])
        src_body = match[1].strip()
        attrs["body"] = src_body
        res.append((language, attrs))

    return res


def unpack_language_header(language_header):
    language, *attrs = language_header.split(":")
    attr_map = {}
    for attr_pair in attrs:
        attr_k, attr_v = attr_pair.split(" ", 1)
        attr_map[attr_k] = attr_v

    return language, attr_map
