import $ from "jquery";
import "bootstrap";
import "datatables.net";
import "datatables.net-dt";
import { progressVue } from "./common";
import { initCatalog } from "./catalog";
import { initCompare } from "./compare";

function init() {
  if (isIEorEDGE()) {
    const modal = <HTMLSelectElement>document.getElementById("browser-modal");
    modal.classList.remove("hidden");
  }
  progressVue.$mount("#progress");
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
