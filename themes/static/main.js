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

function setTheme(targetTheme) {
  targetTheme = targetTheme || "dark";
  let oppsiteTheme = targetTheme === "light" ? "dark" : "light";
  document
    .querySelector(`link[title="${targetTheme}"]`)
    .removeAttribute("disabled");
  document
    .querySelector(`link[title="${oppsiteTheme}"]`)
    .setAttribute("disabled", "disabled");
  const html = document.documentElement;
  if (targetTheme === "dark") {
    html.classList.remove("light");
  } else {
    html.classList.add("light");
  }
  localStorage.setItem("theme", targetTheme);
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

document.addEventListener("DOMContentLoaded", function (e) {
  let rootStyle = getComputedStyle(document.documentElement);

  let mainFontSize = rootStyle.getPropertyValue("----main-font-size").trim();

  document.querySelector("#content").addEventListener("click", function (e) {
    let parenBackground = rootStyle
      .getPropertyValue("--paren-background1")
      .trim();
    let current_paren = e.target.closest(".paren");
    // clear last_pare style
    if (last_paren) {
      // last_paren.style.backgroundColor = preBackground;
      // last_paren.style.fontSize = mainFontSize;
      last_paren.style = "";
    }
    if (current_paren) {
      current_paren.style.backgroundColor = parenBackground;
      current_paren.style.fontSize = "1.8rem";
    }
    console.log(current_paren, last_paren);
    last_paren = current_paren;
  });
});
