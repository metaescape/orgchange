# orgpost

`orgpost` 是个人使用的 org mode 的 blog 发布工具和主题集合。

## 依赖

需要提前安装 python 包

```bash
pip install orgparse jinja2 bs4
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

如果不修改 `config_exampl`e, 以上命令执行后会将 `example` 目录中的所有 org 文件导出到 `~/mysite/example/www` 子目录中，可以在 `~/mysite` 中启动本地 web server 来查看网页导出效果。

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

## 原则

独立于个人 emacs 配置，也就是说，只需要下载 orgchange 后，所有的 blog 发布需要的工具和依赖都在 orgchange 中包含和配置，而不需要依赖自己的 emacs 配置。emacs 配置和 blog 发布配置并不互相影响。

blog 发布目录独立于 org 文件目录，并且不对 org 文件的组织有固定要求。这意味着可以将分散在不同位置的 org 文件都导入到同一个 web 页面下。

## 局限

不处理 org 文件重命名：

- 例如假设有 a.org 想导出成 b.html， 这个功能涉及整个项目引用检索，因此没有实现.
