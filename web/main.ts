import $ from "jquery";
import { initCatalog } from "./catalog";
import { initCompare } from "./compare";

function init() {
  const tcat: JQuery<HTMLTableElement> = $('table#tcat');
  const tcomp = <HTMLTableElement|null>document.getElementById('tcompare');
  if ((<any>window).Catalog && tcat.length)
    initCatalog(tcat);
  else if ((<any>window).Catalogs && (<any>window).Dict && tcomp)
    initCompare(tcomp);
}

$(init);
