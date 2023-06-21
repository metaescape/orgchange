# orgpost

`orgpost` 是专为个人博客设计的 org mode 发布工具和主题集合。

页面展示： [hugchange](www.hugchange.life)

## 依赖

需要提前安装 python 包

```bash
pip install orgparse jinja2 bs4 pygments
```

## 使用方法

假设个人网页发布的目录是 `~/mysite`, 那么首先把 orgchange 移动到该目录下, 路径为 `~/mysite/orgchange`. (后文出现的相对目录，都是相对 `~/mysite` 而言的)

接着进入到 `~/mysite` 中, 配置好 `orgchange/config.json` 后执行 `python orgchange/publish.py`

完整的代码可能是（注释掉的代码是只需要在初始化时执行一次的）：

```bash
# pip install orgparse
cd ~/mysite
# git clone git@github.com:metaescape/orgchange.git
# cp config_example.json config.json
# edit config.json
python orgchange/publish.py
```

如果不修改 `config_example。json` 文件, 以上命令执行后会将 `example` 目录中的所有 org 文件导出到 `~/mysite/example/www` 子目录中，可以在 `~/mysite` 中启动本地 web server 来查看网页导出效果。

个人测试的时候，也可以执行： `python orgchange/publish.py --config config_example.json` 只导出 example 目录里的文件。

## `config.json` 解释

config_example.json 结构如下，发布前，需要把该文件复制或重命名为 `config.json` ,然后根据自己的实际情况修改其中的内容。

```json
{
  "org_prefixes": ["~/org/", "./example/"],
  "index_template": "./themes/darkfloat/index.html",
  "beian": "京ICP备2021000000号",
  "sitename": "Personal blog...",
  "default_theme": "darkfloat",
  "github_url": "https://github.com/metaescape",
  "github_name": "metaescape",
  "index_org": "./example/index.org",
  "publish_folder": "./example/www",
  "site_repo": "https://github.com/metaescape/hugchangelife/"
}
```

接下来是对各个关键词的解释:

- `org_prefixes`: 这是一个保存路径的列表，记录的是源 org 文件所在的目录名. 也就是说，在这些目录下的 org 文件才有可能被导出成 html。 由于是列表，这意味着可以从多个不同的目录下导出 org 到同一个发布目录下。
- `publish_folder`: 个人网站发布的主目录，org 文件导出成 html 后的去处。注意这个目录必须是 `~/mysite` 的子目录（理论上也可以是其他路径，例如 /tmp, 但这会导致页面寻找 css，js 等静态文件时出现混乱，导致样式失效, 因此如果要测试，可以指定 `~/mysite/test_www` 或 `./example/www` 格式的路径）
- `index_org`: 该 org 文件中记录需要发布的所有 blog 的文章的路径。也是相对 `orgchange`

  结合以上三个属性，用一个具体的例子来简单说明发布的流程：

  - 假定 `index_org` 指定为 `~/org/web/mysite.org`, 并且其中有两篇文章的链接：

  ```org
  [[file::../first_blog.org]]
  [[file::~/mysite/orgchange/example/demo.org]]
  ```

  注意第一个 org 文件路径是相对地址，第二个是绝对地址

  - 当执行 `python orgchange/publish.py` 后，orgpost 会把地址格式转成绝对路径，因此得到了两个待导出的 org 文件路径为:

  ```bash
  ~/org/web/first_blog.org
  ~/mysite/orgchange/example/demo.org
  ```

  每个一路径都会与 `org_prefixes` 记录的前缀列表进行匹配，越长的路径优先匹配，匹配到的前缀会用来对路径进行截取，得到以下两个相对路径：

  ```bash
  web/first_blog.org
  demo.org
  ```

  - 以上两个相对路径（转换成 html 后缀后）会与 `publish_folder`(样例中是 `"./example/www"`) 拼接，得到：

  ```bash
  ~/mysite/example/www/web/first_blog.html
  ~/mysite/example/www/demo.html
  ```

  这两个路径是最终发布的页面的路径。

- `index_template`: 主页的模板，在 `index_org` 中的文章列表会添加到该模板的指定位置中, 使用 jinja 渲染。
- `beian`, `sitename`, `github_url`, `github_name`: 这些是可选的，它属于网站的 meta 信息，一般放置在主页的 footer 中

## 设计原则

- 独立于个人 emacs 配置，也就是说，下载 orgchange 后，所有的 blog 发布需要的工具和依赖都在 orgchange 中包含和配置，不依赖自己的 emacs 配置。

- blog 发布目录独立于 org 文件目录，并且不对 org 文件的组织有固定要求。这意味着可以将分散在不同位置的 org 文件都导入到同一个 web 页面下。

- 更现代的默认导出设置： html5 标准，黑白主题切换

- 统一的全局配置，通过 config.json 设置网站的全局属性，同时又可以在 index.org 中用 org property 的方式对每一篇导出的文章进行设置，尽可能避免在原始的 org 文件添加 blog 导出元属性。

## 特色功能

- 支持单个文件按一级标题导出成多个 html, 每个 html 共享整个 org 文件的全局目录（类似 readthedocs 的全局目录）。
- 可以在 index_org 中为为每篇文章单独设置 tags（独立于 org 的 filetag），并且自动生成 tags 页面。
- 代码高亮：基于 pygments 进行代码高亮,从 https://pygments.org/styles/# 选择不同的代码主题
- 专为 lisp 系列语言提供 rainbow 括号高亮以及动态的括号内背景颜色高亮
- 支持 org-ref 和 org-cite 格式的参考文献 csl 格式导出，内置 acl 和 ieee 两种 csl 样式

## 局限

不处理 org 文件重命名：

- 例如假设有 a.org 想导出成 b.html， 这个功能涉及整个项目引用检索，因此没有实现.
- 由于代码执行的结果实际上会导出成成 pre.example tag, 它和 `#+begin_example` 导出结果是一样的，为了更好的控制源码和执行结果之间的距离，默认样式中把 example 的 margin-top 设置成负数，这会导致 `#+begin_example` 的导出结果边距很小，因此建议在 blog 的原始 blog 中不要使用 `#+begin_example` ， 可以用 dynamic ， verse,center 等其他 block
