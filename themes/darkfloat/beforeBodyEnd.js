const global_toc = document.querySelector("#global-toc");
if (global_toc) {
  document.addEventListener("click", function (event) {
    if (event.target !== global_toc && !global_toc.contains(event.target)) {
      // 判断点击的元素是否是按钮元素或者按钮元素的子元素
      global_toc.style.transform = "translate(calc(-100% + 1.5rem), 0)";
      global_toc.style.maxHeight = "12vh";
    } else {
      global_toc.style.transform = "translate(0, 0)";
      global_toc.style.maxHeight = "50vh";
    }
  });
}

const toc = document.querySelector("#table-of-contents");
if (toc) {
  const h2 = toc.querySelector("h2");

  h2.addEventListener("click", function () {
    toc.classList.toggle("open");
  });

  var backTop = document.querySelector(".back-top");

  const obs = new IntersectionObserver(
    function (ents) {
      const ent = ents[0];

      if (ent.isIntersecting === false) {
        backTop.style.display = "block";
      }

      if (ent.isIntersecting === true) {
        backTop.style.display = "none";
      }
    },
    {
      // In the viewport
      root: null,
      threshold: 0,
      rootMargin: "-80px",
    }
  );
  obs.observe(toc);
}

class RadioLinkPreview {
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
        // clear previous content
        this.previewPopup.innerHTML = "";
        this.loadContentAndShow(event, element); // 传递事件对象和当前元素
      });

      element.addEventListener("mouseleave", (event) => {
        this.previewPopup.style.display = "none";
      });
    });
  }

  solveLinkedTargetElement(link) {
    let href = link.getAttribute("href");
    // get first character of href
    if (href[0] !== "#") {
      // this is a remote link, do nothing
      return;
    }
    // this is a local link, solve its context and preview it
    let targetId = link.getAttribute("href").substring(1);
    let targetElement = document.getElementById(targetId);
    let previewElement = targetElement;
    // check if targetElement is a <a id=xxx> tag
    if (targetElement.tagName === "A") {
      // this is a <a> tag, probably a radio link, so check its parent
      targetElement = targetElement.parentElement;
    }

    // check if targetElement is a <h_> tag
    if (targetElement.tagName[0] === "H") {
      // this is a <h_> tag, so it is heading, get its next sibling
      let textSection = targetElement.nextElementSibling;

      // find the first BLOCKQUOTE or <pre class="example"> block in the children of targetElement
      let children = textSection.children;
      for (let child of children) {
        if (child.tagName === "PRE" && child.className === "example") {
          return child;
        } else if (child.tagName === "BLOCKQUOTE") {
          return child;
        }
      }
    }
    return previewElement;
  }

  adjustPopupPosition(element, previewPopup) {
    const linkRect = element.getBoundingClientRect();

    // 基本定位在元素的右下角, absolute定位需要加上滚动距离
    let left = linkRect.left + window.scrollX;
    let top = linkRect.bottom + window.scrollY;

    // 检查是否会溢出屏幕右侧
    const rightEdge = window.innerWidth; // 非滚动宽度
    if (linkRect.left + previewPopup.offsetWidth > rightEdge) {
      // 如果溢出，向左调整位置
      left = linkRect.right + window.scrollX - previewPopup.offsetWidth; // 直接设置left为屏幕宽度减去弹窗宽度
    }

    // 检查是否会溢出屏幕底部
    const bottomEdge = window.innerHeight;
    if (linkRect.bottom + previewPopup.offsetHeight > bottomEdge) {
      // 如果溢出，向上调整位置
      top = linkRect.top + window.scrollY - previewPopup.offsetHeight; // 直接设置top为屏幕高度减去弹窗高度
    }

    // 应用调整后的位置
    previewPopup.style.left = `${left}px`;
    previewPopup.style.top = `${top}px`;
  }

  loadContentAndShow(event, element) {
    const previewElement = this.solveLinkedTargetElement(element);

    this.previewPopup.innerHTML = previewElement.innerHTML;
    this.previewPopup.style.display = "block";

    this.adjustPopupPosition(element, this.previewPopup);
  }
}

document.addEventListener("DOMContentLoaded", (event) => {
  const fringeOptions = [
    {
      content: "<span>目</span><span>录</span>",
      probability: 0.5,
      fontSize: "1.2rem",
    },
    {
      content: "<span>⢘</span>",
      probability: 0.3,
      fontSize: "1.5rem",
    },
    {
      content: "<span>𖡎</span><span>🀤</span>",
      probability: 0.2,
      fontSize: "1.5rem",
    },
  ];

  const fringe = document.getElementById("global-toc-fringe");
  if (fringe) {
    let randomNum = Math.random();
    let cumulativeProbability = 0;

    for (let option of fringeOptions) {
      cumulativeProbability += option.probability;
      if (randomNum < cumulativeProbability) {
        fringe.innerHTML = option.content;
        fringe.style.fontSize = option.fontSize;
        break;
      }
    }
  }

  const categoryLink = document.getElementById("header-category");
  if (categoryLink) {
    categoryLink.textContent = "...";
    categoryLink.style.fontSize = "1.6rem";
  }

  // highlight recent modified posts' last modified time
  const postListContainer = document.querySelector(".post-list");
  const currentDate = new Date();
  const oneMonthAgo = new Date(
    currentDate.getFullYear(),
    currentDate.getMonth() - 1,
    currentDate.getDate()
  );
  const twoWeeksAgo = new Date(
    currentDate.getFullYear(),
    currentDate.getMonth(),
    currentDate.getDate() - 14
  );
  if (postListContainer) {
    const lastModifiedElements =
      postListContainer.querySelectorAll(".index-last-modify");

    lastModifiedElements.forEach((element) => {
      const dateText = element.textContent;
      const date = new Date(dateText);
      date.setDate(date.getDate() - 1); // 将日期向前调整一天

      if (date > twoWeeksAgo && date <= currentDate) {
        element.style.fontWeight = "bold"; // 加粗显示
        element.style.textShadow = `0px 0px 0.5px var(--change-decorate8)`;
      }
    });
  }
  themeBtnHandler(localStorage.getItem("theme"));

  new RadioLinkPreview("radioLinks"); // 初始化LinkPreview
});
