class LinkPreview {
  constructor(classname) {
    // 使用querySelectorAll获取所有匹配的元素
    this.triggerElements = Array.from(
      document.getElementsByClassName(classname)
    );

    if (this.triggerElements.length === 0) {
      return;
    }

    this.previewPopup = document.getElementById("radioLinkPopups");

    this.attachEventListeners();
  }

  attachEventListeners() {
    // 遍历所有触发元素并分别绑定事件
    this.triggerElements.forEach((element) => {
      element.addEventListener("mouseenter", (event) => {
        this.loadContentAndShow(event, element); // 传递事件对象和当前元素
      });

      element.addEventListener("mouseleave", (event) => {
        this.previewPopup.style.display = "none";
      });
    });
  }

  loadContentAndShow(event, element) {
    const targetId = element.getAttribute("href").substring(1);
    const targetElement = document.getElementById(targetId);
    const parentElement = targetElement.parentElement.parentElement;

    if (targetElement) {
      let content = parentElement.textContent;
      // get fist 100 characters, if it is more than 100 characters
      // ending with ...
      if (content.length > 1000) {
        content = content.substring(0, 1000) + "...";
      }
      this.previewPopup.innerHTML = content;
      this.previewPopup.style.display = "block";

      // 获取触发链接元素的位置信息
      const linkRect = element.getBoundingClientRect();

      // 设置预览窗口的位置
      // 例如，放置在链接的正下方，并稍微向右偏移
      this.previewPopup.style.left = `${linkRect.left + window.scrollX}px`;
      this.previewPopup.style.top = `${linkRect.bottom + window.scrollY}px`;
    }
  }
}

// 在页面上初始化 LinkPreview
document.addEventListener("DOMContentLoaded", () => {
  new LinkPreview("radioLinks");
});
