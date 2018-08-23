"use strict";

import $ from "jquery";
import { assert, Dict, Field, Catalog, Aggr, AggrTerms, AggrStats, CatalogResponse, fill_select_terms, field_option } from "./types";

const Max_compare = 4;

declare const Catalogs: Dict<Catalog>;
declare const Dict: Field[];
const Dict_map: Dict<Field> = {};

const Compare: Array<Catalog> = [];
var TComp: HTMLTableElement;
const Aggs: Dict<Dict<Aggr>> = {};
const Aggs_req: Dict<Dict<(a: Aggr) => void>> = {};

function catalog_dict(c: Catalog, d: string): Field|undefined {
  return c.fields.find((f: Field) => f.dict === d);
}

var Aggs_timeout: number|undefined;
function update_aggs() {
  Aggs_timeout = undefined;
  for (let c in Aggs_req) {
    const q = Aggs_req[c];
    const l = Object.keys(q);
    if (!l)
      continue;
    $.ajax({
      method: 'GET',
      url: '/' + c + '/catalog',
      data: {
        limit: 0,
        aggs: l.join(' ')
      }
    }).then((res: CatalogResponse) => {
      const a = res.aggregations;
      if (!a)
        return;
      for (let f in a) {
        if (q[f]) {
          q[f](a[f]);
          delete q[f];
        }
      }
    });
  }
}

function request_aggs(cat: string, f: string, call: (a: Aggr) => void) {
  let ca = Aggs[cat];
  if (!ca)
    ca = Aggs[cat] = {};
  if (ca[f])
    return call(ca[f]);
  let q = Aggs_req[cat];
  if (!q)
    q = Aggs_req[cat] = {};
  const c = q[f];
  q[f] = (a) => {
    if (c)
      c(a);
    else
      ca[f] = a;
    call(a);
  };
  if (!Aggs_timeout)
    Aggs_timeout = setTimeout(update_aggs);
}

function tr_cell(r: HTMLTableRowElement, i: number): HTMLTableCellElement {
  let n = i - r.cells.length + 1;
  if (n <= 0)
    return r.cells[i];
  while (--n)
    r.insertCell();
  return r.insertCell();
}

function field_id(cat: Catalog, f: Field) {
  return f.dict ? "dict."+f.dict : cat.name+"."+f.name;
}

function set_field(idx: number, src: Catalog, f: Field) {
  const c = Compare[idx];
  let cf: Field|undefined;
  if (c === src)
    cf = f;
  else if (f.dict)
    cf = catalog_dict(c, f.dict);
  if (!cf)
    return;
  f = cf;
  const id = field_id(src, f);
  let r = <HTMLTableRowElement>document.getElementById("comp-"+id);
  const cell = tr_cell(r, idx+1);
  while (cell.firstChild)
    cell.removeChild(cell.firstChild);

  if (f.terms) {
    const select = document.createElement('select');
    select.name = c.name+'.'+f.name;
    select.disabled = true;
    cell.appendChild(select);
    request_aggs(c.name, f.name, (a: AggrTerms<string>) => {
      fill_select_terms(select, f, a);
    });
  } else {
  }
}

function add_compare(idx: number, f: Field, exists: boolean = true) {
  const cat = Compare[idx];
  const id = field_id(cat, f);
  let r = <HTMLTableRowElement|null>document.getElementById("comp-"+id);
  if (r)
    return set_field(idx, cat, f);
  else if (exists)
    return;
  r = TComp.tBodies[0].insertRow();
  r.id = "comp-"+id;
  const h = r.insertCell();
  const d = f.dict && Dict_map[f.dict];
  h.textContent = (d || f).title;

  for (let idx = 0; idx < Compare.length; idx++)
    set_field(idx, cat, f);
}

function update_addf() {
  const sel = <HTMLSelectElement>document.getElementById('addf');
  while (sel.lastChild)
    sel.removeChild(sel.lastChild);
  sel.add(document.createElement('option'));

  const dict: Dict<boolean> = {};
  const cats: Dict<boolean> = {};

  const dg = document.createElement('optgroup');
  dg.label = "Common fields";
  sel.add(dg);

  const add = (id: string, g: HTMLElement, f: Field) => {
    if (document.getElementById('comp-'+id))
      return;
    const o = g.appendChild(field_option(f));
    o.value = id;
  };

  for (let c of Compare) {
    if (cats[c.name])
      continue;
    const g = document.createElement('optgroup');
    g.label = c.title;
    cats[c.name] = true;

    for (let f of c.fields) {
      if (f.dict)
        dict[f.dict] = true;
      else 
        add(c.name+'.'+f.name, g, f);
    }
    sel.add(g);
  }

  for (let f of Dict) {
    if (dict[f.name])
      add('dict.'+f.name, dg, f);
  }
}

(<any>window).addField = function addField() {
  const sel = <HTMLSelectElement>document.getElementById('addf');
  const v = sel.value;
  if (!v)
    return;
  const [c,f] = v.split('.', 2);
};

function selectCat(sel: HTMLSelectElement) {
  const tsel = <HTMLTableCellElement>sel.parentElement;
  const idx = tsel.cellIndex-1;
  const rsel = <HTMLTableRowElement>tsel.parentElement;
  const cat = Catalogs[sel.value];
  if (cat) {
    Compare[idx] = cat;
    if (idx+2 == rsel.cells.length && idx <= Max_compare)
      rsel.insertCell().appendChild(sel.cloneNode(true));
    const rtitle = <HTMLTableRowElement>document.getElementById('tr-title');
    tr_cell(rtitle, idx+1).textContent = cat.title;

    for (let f of cat.fields)
      add_compare(idx, f, !f.top);
  } else if (idx+2 < rsel.cells.length) {
    Compare.splice(idx, 1);
    for (let r of <any>TComp.rows as HTMLTableRowElement[])
      if (r.cells.length > idx+1)
        r.deleteCell(idx+1);
    /* TODO: remove unnecessary fields */
  }
  update_addf();
}
(<any>window).selectCat = selectCat;

export function initCompare(table: HTMLTableElement) {
  TComp = table;
  for (let c in Catalogs)
    Catalogs[c].name = c;
  for (let f of Dict)
    Dict_map[f.name] = f;

  const sels = document.getElementsByName("selcat");
  for (let sel of <any>sels as HTMLSelectElement[])
    if (sel.value)
      selectCat(sel);

  update_addf();
}

