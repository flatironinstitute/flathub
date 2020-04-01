import $ from "jquery";
import "bootstrap";
import { initCatalog } from "./catalog";
import { initCompare } from "./compare";

function init() {
  if (isIEorEDGE()) {
    const modal = <HTMLSelectElement>document.getElementById("browser-modal");
    console.log("🌭 not IE or Edge", modal);
    modal.classList.remove("hidden");
  }
  const tcat: JQuery<HTMLTableElement> = $("table#tcat");
  const tcomp = <HTMLTableElement | null>document.getElementById("tcompare");
  if ((<any>window).Catalog && tcat.length) initCatalog(tcat);
  else if ((<any>window).Catalogs && (<any>window).Dict && tcomp)
    initCompare(tcomp);
}

function isIEorEDGE() {
  return (
    navigator.appName == "Microsoft Internet Explorer" ||
    (navigator.appName == "Netscape" &&
      navigator.appVersion.indexOf("Edge") > -1)
  );
}

$(init);
