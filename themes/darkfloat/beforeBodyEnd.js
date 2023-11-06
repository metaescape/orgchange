themeBtnHandler(localStorage.getItem("theme"));

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

document.addEventListener("DOMContentLoaded", (event) => {
  const fringeOptions = [
    {
      content: '<span>目</span><span">录</span>',
      probability: 0.5,
      fontSize: "1.2rem",
    },
    {
      content: '<span>𖡎</span><span">🀤</span>',
      probability: 0.3,
      fontSize: "1.5rem",
    },
  ];

  const fringe = document.getElementById("global-toc-fringe");

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
});
