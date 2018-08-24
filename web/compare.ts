"use strict";

import $ from "jquery";
import { assert, Dict, Field, Catalog, Aggr, AggrTerms, AggrStats, CatalogResponse, fill_select_terms, field_option } from "./types";

const Max_compare = 4;

declare const Catalogs: Dict<Catalog>;
declare const Dict: Field[];
const Dict_map: Dict<Field> = {};

const Fields: Array<CField> = []
const Compares: Array<Compare> = [];
var TComp: HTMLTableElement;

const Aggs: Dict<Dict<Aggr>> = {};
const Aggs_req: Dict<Dict<(a: Aggr) => void>> = {};

var Aggs_timeout: number|undefined;
function update_aggs() {
  Aggs_timeout = undefined;
  for (let c in Aggs_req) {
    const q = Aggs_req[c];
    let a = Aggs[c];
    const call = (f: string) => {
      q[f](a[f]);
      delete q[f];
    };
    if (!a)
      a = Aggs[c] = {};
    const l = [];
    for (let f in q) {
      if (a[f])
        call(f);
      else
        l.push(f);
    }
    if (!l.length)
      continue;
    $.ajax({
      method: 'GET',
      url: '/' + c + '/catalog',
      data: {
        limit: 0,
        aggs: l.join(' ')
      }
    }).then((res: CatalogResponse) => {
      const r = res.aggregations;
      if (!r)
        return;
      for (let f in r) {
        a[f] = r[f];
        if (q[f])
          call(f);
      }
    });
  }
}

function request_aggs(cat: string, f: string, call: (a: Aggr) => void) {
  let q = Aggs_req[cat];
  if (!q)
    q = Aggs_req[cat] = {};
  const c = q[f];
  q[f] = c ? (a) => {
    c(a);
    call(a);
  } : call;
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

class CField {
  public cat: string|null
  public field: Field

  constructor(cat: Catalog|string|null, field: Field|string) {
    if (!cat) { }
    else if (typeof cat === 'object')
      cat = cat.name;
    else if (cat === 'dict')
      cat = null;

    if (typeof field === 'string')
      field = cat ?
        assert(Catalogs[cat].fields.find(f => f.name === field)) :
        Dict_map[field];
    if (field.dict) {
      cat = null;
      field = Dict_map[field.dict];
    }
    assert(!field.dict);

    this.cat = cat;
    this.field = field;
  }

  get id(): string {
    return (this.cat||'dict')+'.'+this.field.name;
  }

  toString(): string {
    return this.id;
  }

  equals(that: CField): boolean {
    return this.cat === that.cat && this.field.name === that.field.name;
  }

  private _tr?: HTMLTableRowElement|null
  get tr(): HTMLTableRowElement|null {
    if (this._tr === undefined)
      this._tr = <HTMLTableRowElement|null>document.getElementById('cf-'+this.id);
    return this._tr;
  }

  get idx(): number {
    let r = this.tr;
    return r ? r.sectionRowIndex : -1;
  }

  onCatalog(catalog: Catalog): Field|undefined {
    if (catalog.name === this.cat)
      return this.field;
    if (!this.cat)
      return catalog.fields.find((f: Field) => f.dict === this.field.name);
    return undefined;
  }

  add() {
    assert(!this.tr);
    let r = this._tr = TComp.tBodies[0].insertRow();
    Fields[this.idx] = this;
    r.id = 'cf-'+this.id;

    const h = r.insertCell();
    h.textContent = this.field.title;
    let rm = document.createElement('button');
    rm.className = 'remove';
    rm.innerHTML = '&times;';
    rm.onclick = this.remove.bind(this);
    h.insertBefore(rm, h.firstChild);

    for (let idx = 0; idx < Compares.length; idx++)
      Compares[idx].fillField(this);

    update_addf();
  }

  remove() {
    let r = this.tr;
    if (r)
      (r.parentNode as HTMLTableSectionElement).removeChild(r);
    let idx = this.idx;
    if (idx >= 0)
      Fields.splice(idx, 1);

    update_addf();
  }
}

class Compare {
  public filters: Array<Filter> = []

  constructor(public catalog: Catalog, idx: number) {
    Compares[idx] = this;
    const rtitle = <HTMLTableRowElement>document.getElementById('tr-title');
    tr_cell(rtitle, this.col).textContent = this.catalog.title;

    for (let f of this.catalog.fields) {
      this.fillField(new CField(this.catalog, f), f.top);
    }
  }

  get idx(): number {
    return Compares.indexOf(this);
  }

  get col(): number {
    return this.idx+1;
  }

  remove() {
    let col = this.col;
    Compares.splice(this.idx, 1);
    for (let r of <any>TComp.rows as HTMLTableRowElement[])
      if (r.cells.length > col)
        r.deleteCell(col);
    /* TODO: remove unnecessary fields */
  }

  fillField(cf: CField, add: boolean = false): boolean {
    let r = cf.tr;
    if (!r) {
      if (add) {
        cf.add();
        return true; /* presumably */
      }
      return false;
    }
    const cell = tr_cell(r, this.col);
    const f = cf.onCatalog(this.catalog);
    while (cell.firstChild)
      cell.removeChild(cell.firstChild);
    if (!f)
      return false;

    let filt: Filter
    if (f.terms)
      filt = new SelectFilter(this.catalog, f, cell);
    else
      filt = new NumericFilter(this.catalog, f, cell);
    this.filters[cf.idx] = filt;
    return true;
  }
}

abstract class Filter {
  constructor(public catalog: Catalog, public field: Field, public cell: HTMLTableCellElement) {
    request_aggs(catalog.name, field.name, this.fillAggs.bind(this));
  }

  protected abstract fillAggs(aggs: Aggr): void;
}

class SelectFilter extends Filter {
  select: HTMLSelectElement

  constructor(catalog: Catalog, field: Field, cell: HTMLTableCellElement) {
    super(catalog, field, cell);

    this.select = document.createElement('select');
    this.select.disabled = true;
    cell.appendChild(this.select);
  }

  protected fillAggs(a: AggrTerms<string>) {
    fill_select_terms(this.select, this.field, a);
  }
}

class NumericFilter extends Filter {
  lb: HTMLInputElement
  ub: HTMLInputElement

  private makeBound(w: boolean): HTMLInputElement {
    const b = <HTMLInputElement>document.createElement('input');
    b.title = (w?"Upper":"Lower")+" bound for " + this.field.title + " values"
    b.type = "number";
    b.step = this.field.base == "i" ? <any>1 : "any";
    b.disabled = true;
    return b;
  }

  constructor(catalog: Catalog, field: Field, cell: HTMLTableCellElement) {
    super(catalog, field, cell);

    this.lb = this.makeBound(false);
    this.ub = this.makeBound(true);
    cell.innerHTML = ' &ndash; ';
    cell.insertBefore(this.lb, cell.firstChild);
    cell.appendChild(this.ub);
  }

  protected fillAggs(a: AggrStats) {
    this.lb.defaultValue = this.lb.value = this.lb.min = this.ub.min = <any>a.min;
    this.ub.defaultValue = this.ub.value = this.lb.max = this.ub.max = <any>a.max;
    this.lb.disabled = this.ub.disabled = false;
  }
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

  const add = (cf: CField, g: HTMLElement) => {
    if (cf.tr)
      return;
    const o = g.appendChild(field_option(cf.field));
    o.value = cf.id;
  };

  for (let c of Compares) {
    const cat = c.catalog;
    if (cats[cat.name])
      continue;
    cats[cat.name] = true;
    const g = document.createElement('optgroup');
    g.label = cat.title;

    for (let f of cat.fields) {
      if (f.dict)
        dict[f.dict] = true;
      else 
        add(new CField(cat, f), g);
    }
    sel.add(g);
  }

  for (let f of Dict) {
    if (dict[f.name])
      add(new CField(null, f), dg);
  }
}

(<any>window).addField = function addField() {
  const sel = <HTMLSelectElement>document.getElementById('addf');
  const v = sel.value;
  if (!v)
    return;
  sel.value = '';
  const [c,n] = v.split('.', 2);
  let cf = new CField(c, n);
  if (!cf.tr) /* shouldn't exist */
    cf.add();
};

function selectCat(sel: HTMLSelectElement) {
  const tsel = <HTMLTableCellElement>sel.parentElement;
  const idx = tsel.cellIndex-1;
  const rsel = <HTMLTableRowElement>tsel.parentElement;
  const cat = Catalogs[sel.value];
  if (cat)
    new Compare(cat, idx);
  else if (Compares[idx])
    Compares[idx].remove();
  let n = rsel.cells.length-1;
  if (Compares.length >= n && n < Max_compare)
    rsel.insertCell().appendChild(sel.cloneNode(true));
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

