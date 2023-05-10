/* Start of copy code */
// based on https://www.roboleary.net/2022/01/13/copy-code-to-clipboard-blog.html
const copyLabel = "Copy";

async function copyCode(block, button) {
  let code = block.querySelector("pre");
  let text = code.innerText;
  await navigator.clipboard.writeText(text);
  button.innerText = "Copied";
  setTimeout(() => {
    button.innerText = copyLabel;
  }, 500);
}

function addCopyCodeButtons() {
  if (!navigator.clipboard) return;
  let blocks = document.querySelectorAll(".org-src-container");
  blocks.forEach((block) => {
    let button = document.createElement("button");
    button.innerText = copyLabel;
    button.classList.add("copy-code");
    let details = block.closest("details");
    let summary = details && details.querySelector("summary");
    if (summary) {
      summary.appendChild(button);
    } else {
      block.appendChild(button);
    }
    button.addEventListener("click", async () => {
      await copyCode(block, button);
    });
    block.setAttribute("tabindex", 0);
  });
}
document.addEventListener("DOMContentLoaded", function (event) {
  addCopyCodeButtons();
});

window.onload = function () {
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
};

window.addEventListener("load", function () {
  const toggleSwitch = document.querySelector("#darkmode-toggle");
  const html = document.documentElement;
  let isLight = localStorage.getItem("isLight"); // 从本地存储中获取 isLight 变量
  let nextStyle, currentStyle;
  if (isLight) {
    html.classList.add("light");
    toggleSwitch.checked = true;
    [nextStyle, currentStyle] = isLight ? ["light", "dark"] : ["dark", "light"];
    document
      .querySelector(`link[title="${nextStyle}"]`)
      .removeAttribute("disabled");
    document
      .querySelector(`link[title="${currentStyle}"]`)
      .setAttribute("disabled", "disabled");
  }

  toggleSwitch.addEventListener("change", function () {
    [nextStyle, currentStyle] = this.checked
      ? ["light", "dark"]
      : ["dark", "light"];
    document
      .querySelector(`link[title="${nextStyle}"]`)
      .removeAttribute("disabled");
    document
      .querySelector(`link[title="${currentStyle}"]`)
      .setAttribute("disabled", "disabled");
    if (this.checked) {
      html.classList.add("light");
      localStorage.setItem("isLight", true); // 将 isLight 变量存储在本地存储中
    } else {
      html.classList.remove("light");
      localStorage.removeItem("isLight"); // 从本地存储中删除 isLight 变量
    }
  });
});
