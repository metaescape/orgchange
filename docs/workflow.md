# orgchage 导出流程

## 读取发布列表和文章发布选项

- `publish_via_index` 读取 index_org 文件，该文件中规定了要导出的的文章列表

  - `extract_meta_from_index_org` 是对 index_org 进行解析的函数，它扫描该文件中的 level1 和 level2 标题，这些标题应该是 org 链接的形式，每个标题可以通过设置 org-property 来定制该文章的导出选项。

    读取到原始信息后，还要给每个 header 新增几个关键路径：

    - `html_path`： org 文件转成 html 的最终发布路径，这是一个绝对路径, 执行 org-export 的时候需要
    - `html_relative_path` org 文件转成 html 后相对网页目录的相对路径，用于网站中页面之间的引用

  - `index_preprocess` 函数会在读取到所有待发布的文章后，对整个文章列表进行预处理，这包括新增一个 categories 字典，用于提取出文章的 tag/category 信息，比如
    ```
    meta["categories"]["emacs"] 中保存的是与 emacs 有关的文章的 `html_relative_path`
    ```

### 网站发布地址约定
