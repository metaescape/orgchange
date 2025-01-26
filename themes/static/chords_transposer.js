/*!
 *  Chord Transposer
 * modified: 20240622 by orgchange
 * created: 20240222 by orgchange
 * based on http://codegavin.com/projects/transposer
 * support multiple tablature in one page
 * make currentKey to be a block local variable
 */

class Transposer {
  constructor(element, options = {}) {
    this.element = element;
    this.options = Object.assign({}, Transposer.defaults, options);

    this.keys = [
      { name: "Ab", value: 0, type: "F" },
      { name: "A", value: 1, type: "N" },
      { name: "A#", value: 2, type: "S" },
      { name: "Bb", value: 2, type: "F" },
      { name: "B", value: 3, type: "N" },
      { name: "C", value: 4, type: "N" },
      { name: "C#", value: 5, type: "S" },
      { name: "Db", value: 5, type: "F" },
      { name: "D", value: 6, type: "N" },
      { name: "D#", value: 7, type: "S" },
      { name: "Eb", value: 7, type: "F" },
      { name: "E", value: 8, type: "N" },
      { name: "F", value: 9, type: "N" },
      { name: "F#", value: 10, type: "S" },
      { name: "Gb", value: 10, type: "F" },
      { name: "G", value: 11, type: "N" },
      { name: "G#", value: 0, type: "S" },
    ];

    this.init();
  }

  static defaults = {
    chordRegex:
      /^[A-G][b\#]?(2|4|5|6|7|9|11|13|6\/9|7\-5|7\-9|7\#5|7\#9|7\+5|7\+9|b5|#5|#9|7b5|7b9|7sus2|7sus4|add2|add4|add9|aug|dim|dim7|m\/maj7|m6|m7|m7b5|m9|m11|m13|maj7|maj9|maj11|maj13|M7|M9|M11|M13|mb5|m|sus|sus2|sus4)*(\/[A-G][b\#]*)*$/,
    chordReplaceRegex:
      /([A-G][b\#]?(2|4|5|6|7|9|11|13|6\/9|7\-5|7\-9|7\#5|7\#9|7\+5|7\+9|b5|#5|#9|7b5|7b9|7sus2|7sus4|add2|add4|add9|aug|dim|dim7|m\/maj7|m6|m7|m7b5|m9|m11|m13|maj7|maj9|maj11|maj13|M7|M9|M11|M13|mb5|m|sus|sus2|sus4)*)/g,
  };

  init() {
    const startKey = this.element.dataset.key || this.options.key;
    if (!startKey) {
      throw new Error("Starting key not defined.");
    }

    this.currentKey = this.getKeyByName(startKey);

    // Build transpose links
    const keysHtml = document.createElement("div");
    keysHtml.className = "transpose-keys";
    keysHtml.innerHTML = this.keys
      .map(
        (key) =>
          `<a href="#" class="${
            this.currentKey.name === key.name ? "selected" : ""
          }">${key.name}</a>`
      )
      .join("");

    keysHtml.addEventListener("click", (e) => {
      e.preventDefault();
      if (e.target.tagName === "A") {
        this.transposeSong(e.target.textContent);
        keysHtml
          .querySelectorAll("a")
          .forEach((a) => a.classList.remove("selected"));
        e.target.classList.add("selected");
      }
    });

    if (
      !this.element.previousElementSibling ||
      !this.element.previousElementSibling.classList.contains("transpose-keys")
    ) {
      this.element.insertAdjacentElement("beforebegin", keysHtml);
    }

    let html_lines = this.element.textContent.split(/\r\n|\n/g).map((line) => {
      return `<span>${
        this.isChordLine(line) ? this.wrapChords(line) : line
      }</span>`;
    });

    const node_lines = html_lines.map((line) => htmlStringToDomNode(line));
    const wrappedNode_lines = node_lines.map((line) => wrapTextNodes(line));

    this.element.innerHTML = "";
    // create a Fragment
    const fragment = document.createDocumentFragment();
    wrappedNode_lines.forEach((line, index) => {
      fragment.appendChild(line);

      // 如果不是最后一行，添加一个换行符节点
      if (index < wrappedNode_lines.length - 1) {
        fragment.appendChild(document.createTextNode("\n"));
      }
    });

    this.element.appendChild(fragment);
  }

  getKeyByName(name) {
    if (name.endsWith("m")) {
      name = name.slice(0, -1);
    }
    return this.keys.find((key) => key.name === name);
  }

  getChordRoot(input) {
    return input.length > 1 &&
      (input.charAt(1) === "b" || input.charAt(1) === "#")
      ? input.substr(0, 2)
      : input.substr(0, 1);
  }

  getNewKey(oldKey, delta, targetKey) {
    let keyValue = this.getKeyByName(oldKey).value + delta;
    keyValue = (keyValue + 12) % 12;

    const isFlatOrSharp = [0, 2, 5, 7, 10].includes(keyValue);

    if (isFlatOrSharp) {
      const targetType = this.getChordType(targetKey.name);
      return (
        this.keys.find(
          (key) => key.value === keyValue && key.type === targetType
        ) ||
        this.keys.find(
          (key) => key.value === keyValue && key.type !== targetType
        )
      );
    } else {
      return this.keys.find((key) => key.value === keyValue);
    }
  }

  getChordType(key) {
    const lastChar = key.slice(-1);
    if (lastChar === "b") return "F";
    if (lastChar === "#") return "S";
    return "N";
  }

  getDelta(oldIndex, newIndex) {
    return newIndex - oldIndex;
  }

  transposeSong(newKeyName) {
    const currentKey = this.currentKey;
    const newKey = this.getKeyByName(newKeyName);

    if (currentKey.name === newKey.name) return;

    const delta = this.getDelta(currentKey.value, newKey.value);
    // get the whitespace offset of all span.c

    this.element.querySelectorAll("span.chord").forEach((el) => {
      this.transposeChord(el, delta, newKey);
    });

    this.element.dataset.key = newKey.name;
    this.currentKey = newKey;
  }

  transposeChord(el, delta, targetKey) {
    const oldChord = el.textContent;
    const oldChordRoot = this.getChordRoot(oldChord);
    const newChordRoot = this.getNewKey(oldChordRoot, delta, targetKey);
    const newChord = newChordRoot.name + oldChord.substr(oldChordRoot.length);

    if (oldChord.length < newChord.length) {
      // delete the next withespace Sibling
      const sib = el.nextSibling;
      if (sib && sib.classList.contains("char-en")) {
        sib.remove();
      }
    } else if (oldChord.length > newChord.length) {
      // insert a whitespace Sibling
      const sib = document.createElement("span");
      sib.textContent = " ";
      sib.className = "char-en";
      el.insertAdjacentElement("afterend", sib);
    }

    // wrap newChord

    const wrappedChord = newChord
      .split("")
      .map((char) => `<span class='char-en'>${char}</span>`)
      .join("");
    el.innerHTML = wrappedChord;
  }

  isChordLine(input) {
    return input
      .split(" ")
      .every((token) => !token.trim() || this.options.chordRegex.test(token));
  }

  wrapChords(input) {
    return input.replace(
      this.options.chordReplaceRegex,
      "<span class='chord'>$1</span>"
    );
  }

  wrapChar(str) {
    const re = /[\u4e00-\u9fa5]/gi;
    return str.replace(re, function (match) {
      return `<span class="char-cn">${match}</span>`;
    });
  }
}

function htmlStringToDomNode(htmlString) {
  const tempDiv = document.createElement("div");

  tempDiv.innerHTML = htmlString;

  return tempDiv.firstChild;
}

//https://www.shymean.com/article/%E8%AE%B0%E4%B8%80%E6%AC%A1%E5%89%8D%E7%AB%AF%E6%96%87%E6%9C%AC%E5%AF%B9%E9%BD%90%E7%9A%84%E9%97%AE%E9%A2%98

function getTextSize(parent, ch = "a") {
  const span = document.createElement("span");
  const result = {};
  result.width = span.offsetWidth;
  result.height = span.offsetHeight;
  span.style.visibility = "hidden";
  // span.style.fontSize = fontSize
  // span.style.fontFamily = fontFamily
  span.style.display = "inline-block";
  parent.appendChild(span); // 使用继承自父元素的字体样式
  if (typeof span.textContent !== "undefined") {
    span.textContent = ch;
  } else {
    span.innerText = ch;
  }
  let width = parseFloat(window.getComputedStyle(span).width) - result.width;
  let height = parseFloat(window.getComputedStyle(span).height) - result.height;
  result.width = (width * 4) / 5;
  result.height = (height * 4) / 5;
  parent.removeChild(span);
  return result;
}

function alignAndTransposer(chord_block) {
  new Transposer(chord_block, { key: "C" });

  let preTextSize = getTextSize(chord_block);

  //not work
  // chord_block.setAttribute("style", `--char-width:${preTextSize.width}px`);
  // chord_block.setAttribute("style", `--char-height:${preTextSize.height}px`);

  chord_block.style.setProperty("--char-width", `${preTextSize.width}px`);
  chord_block.style.setProperty("--char-height", `${preTextSize.height}px`);
}

document.addEventListener(
  "DOMContentLoaded",

  function () {
    // 查找所有的 h2 元素
    const headers = document.querySelectorAll("h2");

    headers.forEach((header) => {
      // 为每个 h2 元素添加点击事件监听器
      header.addEventListener("click", function (e) {
        // 找到下一个 .outline-text-2 元素
        const nextOutlineText2 = header.nextElementSibling;

        if (
          !nextOutlineText2 ||
          !nextOutlineText2.classList.contains("outline-text-2")
        ) {
          return;
        }
        let displayStyle = nextOutlineText2.style.display;
        if (displayStyle === "" || displayStyle === "none") {
          nextOutlineText2.style.display = "block";
          if (!nextOutlineText2.classList.contains("transposed")) {
            nextOutlineText2.classList.add("transposed");

            const preBlock = nextOutlineText2.querySelector("pre");
            if (preBlock) {
              // 执行 expand 函数
              alignAndTransposer(preBlock);
            }
          }
        } else {
          nextOutlineText2.style.display = "none";
        }
      });
    });
  }
);

function isChinese(char) {
  return /[\u4e00-\u9fa5]/.test(char);
}

function wrapTextNodes(node) {
  // 1) basecase the node is a text node
  if (node.nodeType === Node.TEXT_NODE) {
    const text = node.textContent;
    const text_array = [];

    for (let char of text) {
      const charSpan = document.createElement("span");
      charSpan.className = isChinese(char) ? "char-cn" : "char-en";
      charSpan.textContent = char;
      text_array.push(charSpan);
    }
    return text_array;

    // 2) recursive case the node is an element node
  } else if (node.nodeType === Node.ELEMENT_NODE) {
    // create a node with the same tag and attributes

    const children = Array.from(node.childNodes);
    // clear childNodes
    while (node.firstChild) {
      node.removeChild(node.lastChild);
    }

    for (let child of children) {
      let childWrapped = wrapTextNodes(child);

      if (childWrapped instanceof Array) {
        for (childNode of childWrapped) {
          node.appendChild(childNode);
        }
      } else {
        node.appendChild(childWrapped);
      }
    }
    return node;
  }
}

// 创建一个包含中文和英文的元素
// let testElement = document.createElement("div");
// const text = "<span>Fm7</span>   <span>Em</span> 测试 中文";
// testElement.innerHTML = text;
// console.log(Array.from(testElement.childNodes));
// console.log(wrapTextNodes(testElement));
