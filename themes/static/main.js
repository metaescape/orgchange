/* Start of copy code */
// based on https://www.roboleary.net/2022/01/13/copy-code-to-clipboard-blog.html
const copyLabel = "⎘ ";
let rootStyle;

async function copyCode(block, button) {
  let pre = block.querySelector("pre");
  let text = pre.innerText;
  await navigator.clipboard.writeText(text);
  button.innerText = "copied";
  setTimeout(() => {
    button.innerText = copyLabel + " " + pre.className;
  }, 500);
}

function addCopyCodeButtons() {
  if (!navigator.clipboard) return;
  let blocks = document.querySelectorAll(".org-src-container");
  blocks.forEach((block) => {
    let button = document.createElement("button");
    pre = block.querySelector("pre");
    if (pre) {
      button.innerText = copyLabel + " " + pre.className;
    } else {
      button.innerText = copyLabel;
    }

    button.classList.add("copy-code");
    // let details = block.closest("details");
    // let summary = details && details.querySelector("summary");
    // if (summary) {
    //   summary.appendChild(button);
    // } else {
    block.appendChild(button);
    // }
    button.addEventListener("click", async () => {
      await copyCode(block, button);
    });
    block.setAttribute("tabindex", 0);
  });
}
document.addEventListener("DOMContentLoaded", function (event) {
  addCopyCodeButtons();
});

function setTheme(targetTheme) {
  targetTheme = targetTheme || "dark";
  let oppsiteTheme = targetTheme === "light" ? "dark" : "light";

  var codeThemeCss = document.getElementById("code-theme-css");
  if (targetTheme === "dark") {
    codeThemeCss.href = "/orgchange/themes/static/dark.css";
  } else {
    codeThemeCss.href = "/orgchange/themes/static/light.css";
  }

  const html = document.documentElement;
  if (targetTheme === "dark") {
    html.classList.remove("light");
  } else {
    html.classList.add("light");
  }
  localStorage.setItem("theme", targetTheme);
  rootStyle = getComputedStyle(document.documentElement);

  parenBackground = rootStyle.getPropertyValue("--paren-background1").trim();
}

function themeBtnHandler(targetTheme) {
  const toggleBtn = document.getElementById("theme-toggle");
  toggleBtn.addEventListener("change", () => {
    if (toggleBtn.checked) {
      setTheme("light");
    } else {
      setTheme("dark");
    }
  });
  toggleBtn.checked = targetTheme === "light";
}

let last_paren;
let current_paren;
let parenBackground;

document.addEventListener("DOMContentLoaded", function (e) {
  rootStyle = getComputedStyle(document.documentElement);
  parenBackground = rootStyle.getPropertyValue("--paren-background1").trim();
  let mainFontSize = rootStyle.getPropertyValue("----main-font-size").trim();

  // document.querySelector("#content").addEventListener("click", function (e) {
  //   let current_paren = e.target.closest(".paren");
  //   // clear last_pare style
  //   if (last_paren) {
  //     // last_paren.style.backgroundColor = preBackground;
  //     // last_paren.style.fontSize = mainFontSize;
  //     last_paren.style = "";
  //   }
  //   if (current_paren) {
  //     current_paren.style.backgroundColor = parenBackground;
  //     current_paren.style.fontSize = "1.8rem";
  //   }
  //   console.log(current_paren, last_paren);
  //   last_paren = current_paren;
  // });
  document
    .querySelector("#content")
    .addEventListener("mouseover", function (e) {
      current_paren = e.target.closest(".paren");
      if (current_paren) {
        current_paren.style.backgroundColor = parenBackground;
        current_paren.style.fontSize = "1.7rem";
      }
    });

  document.querySelector("#content").addEventListener("mouseout", function (e) {
    current_paren = e.target.closest(".paren");
    if (current_paren) {
      current_paren.style = "";
    }
  });

  // 检查是否存在 ID 为 'need-mathjax' 的元素
  var needMathJax = document.getElementById("need-mathjax");
  if (needMathJax) {
    // 如果存在，设置 MathJax 配置
    window.MathJax = {
      // ... MathJax 的配置 ...
      tex: {
        inlineMath: [
          ["$", "$"],
          ["\\(", "\\)"],
        ],
        displayMath: [
          ["$$", "$$"],
          ["\\[", "\\]"],
        ],
        processEscapes: true,
        tags: "ams",
      },
      svg: {
        fontCache: "global",
      },
      options: {
        options: {
          enableMenu: true, // 确保上下文菜单启用
        },
      },
      displayAlign: "center",
      displayIndent: "0em",
    };

    // 创建并插入 MathJax CDN 脚本标签
    var script = document.createElement("script");
    script.src = "/mathjax/tex-chtml.js";
    script.async = true;
    document.head.appendChild(script);
  }
});
