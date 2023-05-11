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
