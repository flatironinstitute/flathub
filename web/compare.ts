"use strict";

import $ from "jquery";
import { assert, Dict, Field, Catalog } from "./types";

/* compare */
declare const Catalogs: Dict<Catalog>;
declare const Dict: Field[];
const Dict_map: Dict<Field> = {};
const Compare: Array<Catalog> = [];
var TComp: HTMLTableElement;

function catalog_dict(c: Catalog, d: string): Field|undefined {
  return c.fields.find((f: Field) => f.dict === d);
}

function tr_cell(r: HTMLTableRowElement, i: number): HTMLTableCellElement {
  let n = i - r.cells.length + 1;
  if (n <= 0)
    return r.cells[i];
  while (--n)
    r.insertCell();
  return r.insertCell();
}

function add_compare(idx: number, f: Field) {
  const cat = Compare[idx];
  const d = f.dict && Dict_map[f.dict];
  const id = d ? "dict-"+d.name : cat.name+"-"+f.name;
  let r = <HTMLTableRowElement|null>document.getElementById("comp-"+id);
  if (!r) {
    r = TComp.tBodies[0].insertRow();
    r.id = "comp-"+id;
    const h = r.insertCell();
    h.textContent = (d || f).title;
  }

  for (idx = 0; idx < Compare.length; idx++) {
    let c = Compare[idx];
    let cf: Field|undefined;
    if (c === cat)
      cf = f;
    else if (d)
      cf = catalog_dict(c, d.name);
    if (!cf)
      continue;
    tr_cell(r, idx+1);
  }
}

(<any>window).selectCat = function selectCat(event: Event) {
  const sel = <HTMLSelectElement>event.target;
  const tsel = <HTMLTableCellElement>sel.parentElement;
  const idx = tsel.cellIndex-1;
  const rsel = <HTMLTableRowElement>tsel.parentElement;
  const cat = Catalogs[sel.value];
  if (cat) {
    Compare[idx] = cat;
    if (idx+2 == rsel.cells.length)
      rsel.insertCell().appendChild(sel.cloneNode(true));
    const rtitle = <HTMLTableRowElement>document.getElementById('tr-title');
    tr_cell(rtitle, idx+1).textContent = cat.title;

    for (let f of cat.fields) {
      if (f.top) {
        add_compare(idx, f);
      }
    }
  } else if (idx+2 < rsel.cells.length) {
    Compare.splice(idx, 1);
    for (let r of <any>TComp.rows as HTMLTableRowElement[])
      if (r.cells.length > idx+1)
        r.deleteCell(idx+1);
    /* TODO: remove unnecessary fields */
  }
}

export function initCompare(table: HTMLTableElement) {
  TComp = table;
  for (let c in Catalogs)
    Catalogs[c].name = c;
  for (let f of Dict)
    Dict_map[f.name] = f;
}

