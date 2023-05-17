# orgpost

`orgpost` 是个人使用的 org mode 的 blog 发布工具和主题集合。

## 使用方法

假设个人网页目录是 `~/mysite`, 那么首先把 orgchange 作为其子目录，也就是 `~/mysite/orgchange`

接着进入到 `~/mysite` 中, 配置好 `orgchange/config.json` 后执行 `python orgchange/publish.py`

需要提前安装 python 包

```bash
pip install orgparse jinja2 bs4
```

完整的代码可能是（注释掉的代码是只需要在初始化时执行一次的）：

```bash
# pip install orgparse
cd ~/mysite
# git clone git@github.com:metaescape/orgchange.git
# cp config_example.json config.json
# edit config.json
python orgchange/publish.py
```

如果不修改 config_example, 以上命令执行后会将 `example` 目录中的所有 org 文件导出到 `example/www` 子目录中，可以用启动本地 web server 来查看样例。

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

- `org_prefixes`: 这是一个保存路径的列表，记录的是源 org 文件所在的目录名. 也就是说，在这些目录下的 org 文件才有可能被导出成 html。 由于是列表，这意味着可以从多个不同的目录下导出 org 到同一个网页。
- `publish_folder`: 个人网站发布的主目录，org 文件导出成 html 后的去处。
- `index_org`: 这是一个 org 文件地址，该 org 文件中记录需要发布的所有 blog 的文章地址. 结合以上三个属性，用一个具体的例子来简单说明发布的流程：

  - 假定 `index_org` 指定为 `~/org/web/mysite.org`, 并且其中有两篇文章的链接：

  ```org
  [[file::../first_blog.org]]
  [[file::~/mysite/orgchange/tests/demo.org]]
  ```

  注意第一个 org 文件路径是相对地址，第二个是绝对地址(还可以支持 org id)

  - 当执行 `python orgchange/publish.py` 后，orgpost 会把地址格式转成绝对路径，因此得到了两个待导出的 org 文件路径为:

  ```bash
  ~/org/web/first_blog.org
  ~/mysite/orgchange/tests/demo.org
  ```

  每个一路径会与 `org_prefixes` 记录的前缀列表进行匹配，越长的路径优先
  匹配，匹配到的前缀会用来对路径进行截取，得到以下两个相对路径：

  ```bash
  web/first_blog.org
  demo.org
  ```

  - 这两个相对路径（转换成 html 后缀后）会与 `publish_folder` 拼接，得到：

  ```bash
  ~/mysite/web/first_blog.html
  ~/mysite/demo.html
  ```

  作为最终发布的页面的路径。

- `index_template`: 这是主页的模板，在 `index_org` 中的文章列表会添加到该模板的指定位置中，我将在 html 模板一节详细介绍如何编写模板（或者如何使用现有的模板）。
- `beian`, `sitename`, `github_url`, `github_name`: 这些是可选的，它属于网站的 meta 信息，一般放置在

## 原则

独立于个人 emacs 配置，也就是说，只需要下载 orgchange 后，所有的 blog 发布需要的工具和依赖都在 orgchange 中包含，而不需要依赖自己的 emacs 配置。emacs 配置和 blog 发布并不互相影响。

blog 发布目录独立于 org 文件目录，并且不对 org 文件的组织有固定要求

## 局限

不处理 org 文件重命名：

- 例如假设有 a.org 想导出成 b.html， 这个功能涉及整个项目引用检索，因此没有实现.
