# 主题

本文介绍主题的结构设计，以 darkfloat 为例

目录结构如下：

```bash
themes/darkfloat/
├── export.el
├── header.html
├── head.html
├── index.html
├── postamble.html
├── preamble.html
└── style.css
```

核心在于 `export.el` , `darkfloat` 覆写了 `org-html-template`

postamble 和 preamble 的默认如下：

```emacs lisp
(setq org-html-preamble t)
(setq org-html-preamble-format '(("en" "<p> %t %a hello</p>")))
```

`org-html-preamble-format` 的列表第一个元素的第二个字段完全就是一个 html 文件，因此改成读取一个 html 文件会方便地多，比如可以灵活地添加 icon, 以下是 darkflot 里的设置方式

```emacs lisp
(setq org-html-postamble-format `(("en"
                                   ,(concat (format-time-string "<span> © %G</span>")
                                            (read-file-content "postamble.html")))))
```

因为 preamble/postamble 中既包含文章相关信息也包含全局信息，而 darkfloat 的策略是，用 preamble 作为文章信息展示栏，放在文章标题之下，文章目录之前, 可以包含发布日期和最新修改日期，也可以继续添加作者名称和 email 等，而 postamble 作为全局信息展示栏，包含 copyright，网站对应的 github 等，以全局 footer 的语义放在整个页面最底部.

可以看到， postamble 充当页面的 footer，与之对应的页面的 header 是导航栏，卸载 header.html 中。
preamble 充当文章的 header ，到目前位置没有与之对应的文章级别的 footer。后者可以展示的内容包括：

- 评论框. 由于本框架只生成静态页面，因此不支持评论,但可以通过别的方式来讨论，例如添加本文的 github 源码链接，让读者可以到 github 里评论。
- 与本文相关的其他文章链接，可以以上一篇或下一篇的方式显示，也可以随机展示。

为了实现以上两个功能，尤其是第二个功能，实际需要一个全局的文章列表信息，因为当前文章的上一篇和下一篇 blog 的标题和链接是在 index_org 中指定的. emacs org 导出机制无法获得这个信息。
为了添加全局信息，目前有两种思路：

- 通过修改已经 export 的 html 来把这个信息加入进去，用 beautiful soap 做一个解析和查找替换即可。这种方式增加了一个后处理（postprocess）阶段
- 直接在 `export.el` 的 `org-html-template` 加入 html 代码，然后在 python 中调 `export.el` 的时候，通过 setq 的方式把文章链接加入。

第二种方式避免了对已导出的 html 的后处理，因此选择这种方式。

添加 Categories 的方式，由于 org export 默认是不支持导出 filetags 的，filetags 的主要功能是方便本地 roam 搜索，因此可以是一些容易记住的关键词，而 blog 的 tags 或者说 categories 是为了展现不同视角的，是有展示页面的，因此功能上不太一样。这里完全采用解耦的方式，把 categories 都写在 index.org 的各个结点中。

## 切换主题

用 `localStorage` 保存一个全局的变量 theme，默认为空。
在紧随 `<body>` 之后的 `<script>` 区需要根据 theme
变量变换主题，这包括在 html 元素中添加 light 类名以及变更 highlightjs 主题，通过 `main.js` 中的 `setTheme` 来完成。由于在 body 刚加载的时候，切换主题的按钮还没有加载出来，因此这里不需要去调节按钮。
一个特殊情况是，在第一次打开页面的时候， theme 变量没有被设置，因为为空，那么默认用 dark。

在整个页面都加载完后, toggle 主题的按钮也出现了，因此可以在 `</body>` 前的 `<script>` 区中加入对按钮的监听以及根据主题对设置正确的按钮状态。通过 `main.js` 的 `themeBtnHandler` 函数来处理。
