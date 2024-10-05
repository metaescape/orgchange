from utils import print_yellow, print_green
from datetime import datetime
from zoneinfo import ZoneInfo


def to_datetime(date_str):
    yyyy_m_d, week, time = date_str.split(" ")
    date_str = f"{yyyy_m_d} {time}"

    dt_naive = datetime.strptime(date_str, "%Y-%m-%d %H:%M")

    # 为 datetime 对象添加时区信息（北京时间是 UTC+8）
    dt_with_tz = dt_naive.replace(tzinfo=ZoneInfo("Asia/Shanghai"))
    return dt_with_tz


def get_feed_posts(site_info: dict):
    posts = site_info.get("posts", [])
    feed_num = site_info.get("feed_num", 10)
    updates = []
    nondrafts = []
    for post in posts:
        if post.get("draft", False):
            continue
        if post["need_update"]:
            updates.append(post)
        else:
            nondrafts.append(post)

    # sort from new to old
    nondrafts.sort(key=lambda x: x["last_modify_timestamp"], reverse=True)

    feed_posts = (updates + nondrafts)[:feed_num]

    return feed_posts[::-1]


def add_feed_entry(fg, site_url, feed_posts):
    for post in feed_posts:

        html_path_abs2www = post["html_path_abs2www"]
        post_id = html_path_abs2www.split(".")[0]

        post_title = post["title"]
        post_url = f"{site_url}/{html_path_abs2www}"
        last_modify_timestamp = post["last_modify_timestamp"]
        created_timestamp = post["created_timestamp"]
        description = post.get("description", "")
        last_modfiy_datetime = to_datetime(last_modify_timestamp)
        created_datetime = to_datetime(created_timestamp)
        categories = post.get("categories", [])
        category = [{"term": c} for c in categories]

        fe = fg.add_entry()

        fe.id(post_id)
        fe.title(post_title)
        fe.link(href=post_url)
        fe.description(description)
        fe.updated(last_modfiy_datetime)
        fe.published(created_datetime)
        fe.category(category)


def _generate_feed(site_info: dict, type="rss", feed_posts=[]):
    from feedgen.feed import FeedGenerator

    assert type in ["rss", "atom"], f"Invalid feed type: {type}"
    fg = FeedGenerator()
    site_title = site_info.get("site_name")
    site_url = site_info.get("site_url")
    feed_url = f"{site_url}/{type}.xml"
    language = site_info.get("html_lang")
    publish_folder = site_info.get("publish_folder")
    author_name = site_info.get("author_name")
    email = site_info.get("site_email")

    fg.title(site_title)
    fg.id(site_url)
    fg.link(href=site_url, rel="alternate")
    fg.subtitle(site_url)
    fg.link(href=feed_url, rel="self")
    fg.language(language)
    fg.author({"name": author_name, "email": email})

    add_feed_entry(fg, site_url, feed_posts)
    feed_file = f"{publish_folder}/{type}.xml"

    if type == "rss":
        # fg.rss_str(pretty=True)  # Get the RSS feed as string
        fg.rss_file(feed_file)  # Write the RSS feed to a file
    elif type == "atom":
        # fg.atom_str(pretty=True)
        fg.atom_file(feed_file)

    print_green(f"Feed generated: {feed_file}")


def generate_feed(site_info: dict):
    try:
        from feedgen.feed import FeedGenerator

        feed_types = site_info.get("feed_types", [])

        feed_posts = get_feed_posts(site_info)

        for feed_type in feed_types:
            _generate_feed(site_info, feed_type, feed_posts)
    except ImportError:
        print_yellow(
            "FeedGenerator is not installed. ignoring feed generation"
        )
