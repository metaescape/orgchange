themeBtnHandler(localStorage.getItem("theme"));

const global_toc = document.querySelector("#global-toc");
if (global_toc) {
  document.addEventListener("click", function (event) {
    if (event.target !== global_toc && !global_toc.contains(event.target)) {
      // 判断点击的元素是否是按钮元素或者按钮元素的子元素
      global_toc.style.transform = "translate(-90%, 0)";
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
