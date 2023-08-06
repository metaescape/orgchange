# orgchange

`orgchange` 是为个人博客设计的 org mode 发布工具和主题集合。

页面展示： [hugchange](https://www.hugchange.life)

## 设计理念

- 与个人 emacs 配置完全解耦。也就是说，下载 orgchange 后，所有的 blog 发布需要的工具和依赖都在 orgchange 中包含和配置，不依赖自己的 emacs 配置。因为个人认为， blog 的个性化部分不是在 emacs 配置上，而是在文章内容和设计上。安装 orgchange 后，应该可以马上生成网页，并且该网页拥有灵活地修改页面样式的配置入口。

- 导出选项不应该被限制在 org 文件里。emacs org export 默认情况下会读取文件内的 header 选项( `#+` 开头的设置)，这导致 org 的内容和对 org 导出成 html 的控制选项都写在同一个文件中。然而个人倾向于在 org 只保留影响 emacs 交互的控制选项（比如 org-roam filetags, org-link 缩写等配置）

- 文学编程式的配置方法。对网站所有的期望都写在一个 org 或 json 文件中，配置的 scope 遵守 org 的层级结构。

- 通过 config.json 设置网站的全局属性，同时又可以在 index.org 中用 org property 的方式对每一篇导出的文章进行设置，尽可能避免在原始的 org 文件添加 blog 导出元属性。

- blog 发布目录独立于 org 文件目录，并且不对 org 文件的组织有固定要求。原生的 org-publish 只能指定某个目录作为网页发布目录，只能发布该目录下的 org 文件。orgchange 则可以将分散在不同位置的 org 文件都导入到同一个 web 目录下。

- 更现代的默认的导出选项： html5 标准

## 特色功能

- 支持单个文件按一级标题导出成多个 html, 每个 html 共享整个 org 文件的全局目录（类似 readthedocs 的全局目录）。
- 可以在 index_org 中为为每篇文章单独设置 tags（独立于 org 的 filetag），并且自动生成 tags 页面。
- 代码高亮：基于 pygments 进行代码高亮,从 https://pygments.org/styles/# 选择不同的代码主题
- 专为 lisp 系列语言提供 rainbow 括号高亮以及动态的括号内背景颜色高亮
- 支持 org-ref 和 org-cite 格式的参考文献 csl 格式导出，内置 acl 和 ieee 两种 csl 样式
- 支持在 index.org 的各个文章 heading 内用 python src block 的方式单独设置导出属性，例如 `draft`(只导出但不发布在 index.html 里) ，`link_replace`(替换 html 中本地文件链接) 等属性
- 如果有 mermaid src_bock, 导出的 html 中自动添加 mermaid js cdn 链接

## 依赖

需要提前安装 python 包

```bash
pip install orgparse jinja2 bs4 pygments
```

## 使用方法

假设个人网页发布的目录是 `~/mysite`, 那么首先把 orgchange 移动到该目录下, 路径为 `~/mysite/orgchange`. (后文出现的相对目录，都是相对 `~/mysite` 而言的)

执行以下命令即可发布 demo 页面

```bash
cd ~/mysite
python orgchange/publish.py --index orgchange/example/index.org --verbose --all
```

通过以下命令启动简单的 web server 进行预览

```bash
google-chrome --new-window http://localhost:1897/orgchange/example/www
python -m http.server 1897
```

可以参考
网站所有的发布配置都在一个 index.org 文件中。具体参考 [example/index.org](example/index.org)

## 局限

不处理 org 文件重命名：

- 例如假设有 a.org 想导出成 b.html， 这个功能涉及整个项目引用检索，因此没有实现.
