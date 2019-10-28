"use strict";

import $ from "jquery";
import Highcharts from "highcharts";
import { assert, Dict, Field, Catalog, Aggr, AggrTerms, AggrStats, CatalogResponse, fill_select_terms, field_option, field_title, toggle_log, axis_title, render_funct, histogram_options } from "./common";

const Max_compare = 4;
const Colors = ['#0008', '#f008', '#0f08', '#00f8'];

declare const Catalogs: Dict<Catalog>;
declare const Dict: Field[];
const Dict_map: Dict<Field> = {};

const Fields: Array<CField> = []
const Compares: Array<Compare> = [];
var TComp: HTMLTableElement;

const Aggs: Dict<Dict<Aggr>> = {};
const Aggs_req: Dict<Dict<(a: Aggr) => void>> = {};

var CompField: undefined|CField;
var Histogram: Highcharts.ChartObject|undefined;

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
    return;
  }

  add() {
    assert(!this.tr);
    let r = this._tr = TComp.tBodies[0].insertRow();
    Fields[this.idx] = this;
    r.id = 'cf-'+this.id;

    r.insertCell().appendChild(field_title(this.field, this.remove.bind(this)));

    for (let idx = 0; idx < Compares.length; idx++)
      Compares[idx].fillField(this);

    update_fields();
  }

  remove() {
    let r = this.tr;
    if (r && r.parentNode)
      (r.parentNode as HTMLTableSectionElement).removeChild(r);
    let idx = this.idx;
    if (idx >= 0)
      Fields.splice(idx, 1);

    update_fields();
    update_comp();
  }

  addOption(g: HTMLElement) {
    const o = g.appendChild(field_option(this.field));
    o.value = this.id;
  }
}

var Compare_unique = 1;

class Compare {
  public filters: Array<Filter> = []
  private unique: string;
  private pending: boolean = false;

  constructor(public catalog: Catalog, idx: number) {
    this.unique = String(Compare_unique++);
    Compares[idx] = this;

    for (let f of this.catalog.fields) {
      this.fillField(new CField(this.catalog, f), f.top);
    }

    update_comp(this);
  }

  get idx(): number {
    return Compares.indexOf(this);
  }

  get col(): number {
    return this.idx+1;
  }

  remove() {
    /* remove unnecessary fields */
    const old = Fields.filter((f: CField, i: number) =>
      Compares.every(c => !c.filters[i]));
    for (let f of old)
      f.remove();

    const series = Histogram && Histogram.get(this.unique);
    if (series)
      series.remove();
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
      filt = new SelectFilter(this, f, cell);
    else
      filt = new NumericFilter(this, f, cell);
    this.filters[cf.idx] = filt;
    return true;
  }

  private query(): Dict<string> {
    const filt: Dict<string> = {};
    for (let f of this.filters) {
      if (!f)
        continue;
      const q = f.query();
      if (!q)
        continue;
      filt[f.field.name] = q;
    }
    return filt;
  }

  updateResults(r: HTMLTableRowElement, ra: HTMLTableRowElement): boolean {
    const cell = tr_cell(r, this.col);
    const acell = tr_cell(ra, this.col);
    let f = CompField && CompField.onCatalog(this.catalog);
    while (cell.lastChild)
      cell.removeChild(cell.lastChild);
    while (acell.lastChild)
      acell.removeChild(acell.lastChild);
    if (!f)
      return false;
    if (this.pending)
      return true;
    this.pending = true;
    const n = f.name;
    const render = render_funct(f);
    const scale = f.scale || 1;

    const q = this.query();
    q.limit = <any>0;
    q.aggs = n;
    if (Histogram)
      q.hist = n+':128';
    $.ajax({
      method: 'GET',
      url: '/' + this.catalog.name + '/catalog',
      data: q
    }).then((res: CatalogResponse) => {
      this.pending = false;
      const r = res.aggregations;
      if (!r)
        return;
      let a = r[n] as AggrStats;
      cell.appendChild(document.createTextNode(render(a.min*scale) + ' \u2013 ' + render(a.max*scale)));
      cell.appendChild(document.createElement('br'));
      cell.appendChild(document.createElement('em')).appendChild(document.createTextNode('\u03bc'));
      cell.appendChild(document.createTextNode(' = ' + render(a.avg*scale)));

      const l = document.createElement('a');
      delete q.limit;
      delete q.aggs;
      delete q.hist;
      l.href = '/' + this.catalog.name + '?' + $.param(q);
      l.appendChild(document.createTextNode("Count: " + res.hits.total.toString()));
      acell.appendChild(l);

      if (r.hist && Histogram) {
        const wid = res.histsize[n]*scale;
        const data = (<AggrTerms<number>>r.hist).buckets.map(x => [x.key * scale, x.doc_count / res.hits.total] as [number, number]);
        const opts = {
          id: this.unique,
          index: this.idx,
          type: 'column',
          name: this.catalog.name,
          data: data,
          pointInterval: wid,
          pointRange: wid,
          color: Colors[this.idx]
        };
        const series = <Highcharts.SeriesObject|undefined>Histogram.get(this.unique);
        if (series)
          series.update(opts);
        else
          Histogram.addSeries(opts);
      }
    }, () => {
      this.pending = false;
    });
    return true;
  }
}

abstract class Filter {
  constructor(public compare: Compare, public field: Field, public cell: HTMLTableCellElement) {
    request_aggs(compare.catalog.name, field.name, this.fillAggs.bind(this));
  }

  protected abstract fillAggs(aggs: Aggr): void;

  change() {
    update_comp(this.compare);
  }

  abstract query(): string|undefined
}

class SelectFilter extends Filter {
  select: HTMLSelectElement

  constructor(compare: Compare, field: Field, cell: HTMLTableCellElement) {
    super(compare, field, cell);

    this.select = document.createElement('select');
    this.select.disabled = true;
    this.select.onchange = this.change.bind(this);
    cell.appendChild(this.select);
  }

  protected fillAggs(a: AggrTerms<string>) {
    fill_select_terms(this.select, this.field, a);
  }

  query(): string|undefined {
    const val = this.select.value;
    if (val)
      return val;
  }
}

class NumericFilter extends Filter {
  lb: HTMLInputElement
  ub: HTMLInputElement
  private scale: number

  private makeBound(w: boolean): HTMLInputElement {
    const b = <HTMLInputElement>document.createElement('input');
    b.title = (w?"Upper":"Lower")+" bound for " + this.field.title + " values"
    b.type = "number";
    b.step = this.field.base == "i" ? <any>1 : "any";
    b.disabled = true;
    b.onchange = this.change.bind(this);
    return b;
  }

  constructor(compare: Compare, field: Field, cell: HTMLTableCellElement) {
    super(compare, field, cell);

    this.scale = field.scale || 1;
    this.lb = this.makeBound(false);
    this.ub = this.makeBound(true);
    cell.appendChild(this.lb);
    cell.appendChild(document.createTextNode(' \u2013 '));
    cell.appendChild(this.ub);
  }

  protected fillAggs(a: AggrStats) {
    this.lb.defaultValue = this.lb.value = this.lb.min = this.ub.min = <any>(a.min*this.scale);
    this.ub.defaultValue = this.ub.value = this.lb.max = this.ub.max = <any>(a.max*this.scale);
    this.lb.disabled = this.ub.disabled = false;
  }

  query(): string {
    const lbv = this.lb.valueAsNumber/this.scale;
    const ubv = this.ub.valueAsNumber/this.scale;
    if (lbv == ubv)
      return <any>lbv;
    else
      return lbv+','+ubv;
  }
}

function update_fields() {
  const asel = <HTMLSelectElement>document.getElementById('addf');
  const csel = <HTMLSelectElement>document.getElementById('compf');
  while (asel.lastChild)
    asel.removeChild(asel.lastChild);
  while (csel.lastChild)
    csel.removeChild(csel.lastChild);
  const aopt = document.createElement('option');
  aopt.value = '';
  aopt.text = 'Add filter...';
  asel.add(aopt);
  const copt = document.createElement('option');
  copt.value = '';
  copt.text = 'Compare field...';
  csel.add(copt);

  const add = (g: HTMLElement, cf: CField) => {
    if (cf.tr)
      return;
    cf.addOption(g);
  };
  const addc = (cf: CField) => {
    if (!cf.field.terms)
      cf.addOption(csel);
  };

  const cats: Dict<boolean> = {};
  for (let c of Compares)
    cats[c.catalog.name] = true;

  let cl = Object.keys(cats);
  if (cl.length == 1) {
    /* only one catalog -- just use its fields */
    let cat = Catalogs[cl[0]];
    for (let f of cat.fields) {
      let cf = new CField(cat, f);
      add(asel, cf);
      addc(cf);
    }
  } else {
    const dict: Dict<number> = {};
    const dg = document.createElement('optgroup');
    dg.label = "Standard fields";
    asel.add(dg);

    for (let c of cl) {
      const cat = Catalogs[c];
      const g = document.createElement('optgroup');
      g.label = cat.title;

      for (let f of cat.fields) {
        if (f.dict)
          dict[f.dict] = (dict[f.dict] || 0)+1;
        else
          add(g, new CField(cat, f));
      }
      asel.add(g);
    }

    for (let f of Dict) {
      if (dict[f.name]) {
        let cf = new CField(null, f);
        add(dg, cf);
        if (dict[f.name] == cl.length)
          addc(cf);
      }
    }
  }
  if (CompField) {
    csel.value = CompField.id;
    if (csel.value !== CompField.id)
      update_comp();
  }
}

function selected_field(sel: HTMLSelectElement): CField|undefined {
  const v = sel.value;
  if (!v)
    return;
  const [c,n] = v.split('.', 2);
  return new CField(c, n);
}

(<any>window).addField = function addField() {
  const sel = <HTMLSelectElement>document.getElementById('addf');
  const cf = selected_field(sel);
  sel.value = '';
  if (cf && !cf.tr) /* shouldn't exist */
    cf.add();
};

function update_comp(...comps: Compare[]) {
  if (!comps.length)
    comps = Compares;
  const sel = <HTMLSelectElement>document.getElementById('compf');
  CompField = selected_field(sel);
  const r = <HTMLTableRowElement>document.getElementById('tr-comp');
  const ra = <HTMLTableRowElement>document.getElementById('tr-add');
  let valid = false;
  for (let c of comps) {
    if (c.updateResults(r, ra))
      valid = true;
  }
  if (Histogram) {
    if (CompField)
      Histogram.update({
        xAxis: {
          title: axis_title(CompField.field)
        }
      });
    else
      histogramToggle();
  }
  else if (comps === Compares) {
    const histTog = <HTMLButtonElement>document.getElementById('hist-tog');
    histTog.disabled = !valid;
  }
};
(<any>window).compField = update_comp;

function selectCat(sel: HTMLSelectElement) {
  const tsel = <HTMLTableCellElement>sel.parentElement;
  const idx = tsel.cellIndex-1;
  const rsel = <HTMLTableRowElement>tsel.parentElement;
  const cat = Catalogs[sel.value];
  const cur = Compares[idx];
  if (cur && !cat) {
    let col = cur.col;
    Compares.splice(cur.idx, 1);
    for (let r of <any>TComp.rows as HTMLTableRowElement[])
      if (r.cells.length > col)
        r.deleteCell(col);
  }
  if (cat)
    new Compare(cat, idx);
  if (cur)
    cur.remove();
  let n = rsel.cells.length-1;
  if (Compares.length >= n && n < Max_compare)
    rsel.insertCell().appendChild(sel.cloneNode(true));
  update_fields();
}
(<any>window).selectCat = selectCat;

function histogramToggle() {
  if (Histogram) {
    Histogram.destroy();
    Histogram = undefined;
    $('#dhist').hide();
    $('#hist').empty();
    return;
  }
  if (!CompField)
    return;
  $('#dhist').show();
  const opts = histogram_options(CompField.field);
  (<Highcharts.LegendOptions>opts.legend).enabled = true;
  (<Highcharts.AxisTitle>(<Highcharts.AxisOptions>opts.yAxis).title).text = 'Fraction';
  Histogram = Highcharts.chart('hist', opts);
  update_comp();
}
(<any>window).histogramComp = histogramToggle;

function toggleLog() {
  if (Histogram)
    toggle_log(Histogram);
};

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

  update_fields();
  (<any>window).toggleLog = toggleLog;
}
