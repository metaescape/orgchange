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
