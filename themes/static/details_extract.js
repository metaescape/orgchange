// get all children of org-main
const org_main = document.getElementById("org-main");
const preamble = document.getElementById("preamble");

// create btn
let btn = document.createElement("button");
// set type and id
btn.setAttribute("type", "button");
btn.setAttribute("id", "toggle-details");
btn.innerText = "原文";
let state = "main";

preamble.append(btn);
const allDetails = document.querySelectorAll("details");

const plist = [];

allDetails.forEach((details) => {
  const firstLevelPs = details.querySelectorAll(":scope > p");

  firstLevelPs.forEach((p) => {
    plist.push(p.cloneNode(true));
  });
});

let org_main_details;

btn.addEventListener("click", () => {
  // replace org-main with org-main-details
  if (state === "main") {
    // wrap all_detainls in a div
    org_main_details = document.createElement("div", { id: "org-main" });
    // copy the btn to org_main_details
    org_main_details.append(...plist);
    org_main.replaceWith(org_main_details);
    state = "details";
    btn.innerText = "译文";
  } else {
    org_main_details.replaceWith(org_main);
    state = "main";
    btn.innerText = "原文";
  }
});
