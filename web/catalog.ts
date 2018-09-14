"use strict";

import $ from "jquery";
import "datatables.net";
import DataTablesScroller from "datatables.net-scroller";
import Highcharts from "highcharts";
import { assert, Dict, Field, Catalog, AggrStats, AggrTerms, Aggr, CatalogResponse, fill_select_terms, field_option } from "./types";

System.import('highcharts/modules/heatmap.js').then(m => m(Highcharts));

DataTablesScroller();

var TCat: DataTables.Api;
declare const Catalog: Catalog;
declare const Query: {offset:number, limit:number, sort:{field:string,asc:boolean}[], fields:string[], filter:{field:string,value:string|{lb:string,ub:string}}[], aggs:string[], hist:string|null};
const Fields_idx: Dict<number> = {};
const Filters: Array<Filter> = [];
var Sample: number = 1;
var Seed: undefined|number = 0;
var Update_aggs: number = -1;
var Histogram: undefined|NumericFilter;
const Histogram_bins = 100;
var Histogram_chart: Highcharts.ChartObject | undefined;
var Heatmap_chart: Highcharts.ChartObject | undefined; 
var Histogram_bin_width = 0;
var Last_fields: string[] = [];

var url_dict = {};

var Download_query: Dict<string> = {};
function set_download(query: Dict<string> = Download_query) {
  delete query.limit;
  delete query.offset;
  delete query.aggs;
  delete query.hist;
  const q = '?' + $.param(Download_query = query);
  const h = $('#download').html('download as ');
  for (let f of Catalog.bulk) {
    const a = document.createElement('a');
    h.append(a);
    a.id = 'download.' + f;
    a.href = '/' + Catalog.name + '/' + f + q;
    a.appendChild(document.createTextNode(f));
    h.append(document.createTextNode(' '));
  }
}

function histogram(agg: AggrTerms<number>) {
  const hist = Histogram;
  if (!hist)
    return;
  const field = hist.field;
  const points = agg.buckets.map(d => { return {x:d.key,y:d.doc_count}; });
  points.push({x:points[points.length-1].x+Histogram_bin_width,y:0});
  const render = render_funct(hist.field);
  $('#dhist').show();
  Histogram_chart = Highcharts.chart('hist', {
    chart: {
      events: {
        selection: function (event: Highcharts.ChartSelectionEvent) {
          let left = event.xAxis[0].min;
          let right = event.xAxis[0].max;
          if (left == null || right == null || !(left < right)) {
            /* select one bucket? ignore? */
            return;
          }
          left  = Histogram_bin_width*Math.floor(left/Histogram_bin_width);
          right = Histogram_bin_width*Math.ceil(right/Histogram_bin_width);
          hist.setRange(left, right);
          hist.change();
          return false; // Don't zoom
        }
      },
      zoomType: 'x',
      animation: false
    },
    legend: {
      enabled: false
    },
    title: {
      text: null
    },
    credits: {
      enabled: false
    },
    xAxis: {
      type: 'linear',
      title: {
        // useHTML: true, // not enough for mathjax
        text: field.title + (field.units ? ' [' + field.units + ']' : '')
      },
      gridLineWidth: 1,
      min: hist.lbv,
      max: hist.ubv+Histogram_bin_width
    },
    yAxis: {
      type: 'linear',
      title: {
        text: 'Count'
      },
      min: 0
    },
    tooltip: {
      animation: false,
      formatter: function (this: {x: number, y: number}): string {
        return ('[' + render(this.x) + ',' + render(this.x+Histogram_bin_width) + '): ' + this.y + '\n(drag to filter)');
      }
    },
    plotOptions: {
      area: {
        step: 'left',
        fillOpacity: 0.5,
        animation: { duration: 0 },
        marker: {
          radius: 0
        },
        states: {
          hover: {
            enabled: false
          }
        }
      }
    },
    series: [{
      type: 'area',
      data: points,
      color: '#555555',
    }]
  });
}
function heatmapChart() {
  console.log('heatmap');
  let fake_data = generateData();
  Heatmap_chart = Highcharts.chart('hist', {
    chart: {
      inverted: true
    },
    yAxis: {
      title: {
        text: null
      },
      minPadding: 0,
      maxPadding: 0,
      startOnTick: false,
      endOnTick: false
    },

    colorAxis: {
      stops: [
        [0.0, '00000'],
        [0.25, '#3060cf'],
        [0.5, '#fffbbc'],
        [0.9, '#c4463a']
      ],
    },
    series: [{
      type: 'heatmap',
      data: fake_data,
      borderWidth: 0
    }]
  });
}

(<any>window).heatmap = function heatmap() {
  console.log('chart');
  heatmapChart();
}

function generateData() {
  let data = [];
  for (let i = 0; i < 100; i++) {
    for (let j = 0; j <100; j++) { 
      data.push([i, j, Math.floor(Math.random() * Math.floor(100))]);
  }
  }
  return data;
}

(<any>window).toggleLog = function toggleLog() {
  if (!Histogram_chart)
    return;
  const axis = Histogram_chart.yAxis[0];
  if ((<any>axis).userOptions.type !== 'linear') {
    axis.update({
      min: 0,
      type: 'linear'
    });
  }
  else {
    axis.update({
      min: 0.1, 
      type: 'logarithmic'
    });
  }
}

/* elasticsearch max_result_window */
const displayLimit = 10000;

function ajax(data: any, callback: ((data: any) => void), opts: any) {
  const query: Dict<string> = {
    sort: data.order.map((o: any) => {
      return (o.dir == "asc" ? '' : '-') + data.columns[o.column].data;
    }).join(' ')
  };

  let aggs = Filters;
  if (Update_aggs >= 0)
    aggs = aggs.slice(Update_aggs);
  else
    Update_aggs = Filters.length;
  for (let fi = 0; fi < Update_aggs; fi++) {
    const filt = Filters[fi];
    const q = filt.query();
    if (q != null)
      query[filt.name] = q;
  }
  if (Sample < 1) {
    query.sample = <any>Sample;
    if (Seed != undefined)
      query.sample += '@' + Seed;
  }

  query.offset = data.start;
  query.limit = data.length;
  Last_fields = TCat.columns(':visible').dataSrc().toArray();
  query.fields = Last_fields.join(' ');
  if (aggs)
    query.aggs = aggs.map((filt) => filt.name).join(' ');
  if (Histogram) {
    const wid = Histogram.ubv - Histogram.lbv;
    if (wid && wid > 0) {
      Histogram_bin_width = wid/Histogram_bins;
      if (Histogram.field.base === "i")
        Histogram_bin_width = Math.ceil(Histogram_bin_width);
      query.hist = Histogram.name + ':' + Histogram_bin_width;
    }
  }
  $('td.loading').show();
  $.ajax({
    method: 'GET',
    url: '/' + Catalog.name + '/catalog',
    data: query
  }).then((res: CatalogResponse) => {
    $('td.loading').hide();
    Catalog.count = Math.max(Catalog.count || 0, res.hits.total);
    const settings = (<any>TCat.settings())[0];
    settings.oLanguage.sInfo = "Showing _START_ to _END_ of " + settings.fnFormatNumber(res.hits.total);
    callback({
      draw: data.draw,
      recordsTotal: Catalog.count,
      recordsFiltered: Math.min(res.hits.total, displayLimit),
      data: res.hits.hits
    });
    for (let filt of aggs)
      filt.update_aggs((res.aggregations as Dict<Aggr>)[filt.name]);
    Update_aggs = Filters.length;
    if (res.aggregations && res.aggregations.hist)
      histogram(res.aggregations.hist as AggrTerms<number>);

    delete query.aggs;
    delete query.hist;
    url_update(query);
    delete query.limit;
    delete query.offset;
    set_download(query);
  }, (xhr, msg, err) => {
    callback({
      draw: data.draw,
      data: [],
      error: msg + ": " + err
    });
  });
  py_text();
}

function add_filt_row(name: string, ...nodes: Array<JQuery.htmlString | JQuery.TypeOrArray<JQuery.Node | JQuery<JQuery.Node>>>) {
  const id = 'filt-' + name;
  let tr = <HTMLTableRowElement | null>document.getElementById(id);
  if (tr) return;
  const tab = <HTMLTableElement>document.getElementById('filt');
  tr = document.createElement('tr');
  tr.id = id;
  if (tab.lastChild)
    $(tr).insertBefore(<HTMLTableRowElement>tab.lastChild);
  else
    $(tr).appendTo(tab);
  for (let node of nodes) {
    const td = $(document.createElement('td')).appendTo(tr);
    td.append(node);
  }
}

function add_sample() {
  const samp = <HTMLInputElement>document.createElement('input');
  samp.name = "sample";
  samp.type = "number";
  samp.step = "any";
  samp.min = <any>0;
  samp.max = <any>1;
  samp.value = <any>Sample;
  samp.title = "Probability (0,1] with which to include each item"

  const seed = <HTMLInputElement>document.createElement('input');
  seed.name = "seed";
  seed.type = "number";
  seed.step = <any>1;
  seed.min = <any>0;
  seed.value = <any>Seed;
  seed.disabled = Sample >= 1;
  seed.title = "Random seed to generate sample selection"

  samp.onchange = seed.onchange = function () {
    Sample = samp.valueAsNumber;
    if (!isFinite(Sample))
      Sample = 1;
    if (seed.disabled = Sample >= 1)
      seed.value = '';
    Seed = seed.valueAsNumber;
    if (!isFinite(Seed))
      Seed = 0;
    TCat.draw();
  };

  add_filt_row('sample', 'random sample',
    $('<span>').append('fraction ').append(samp),
    $('<span>').append('seed ').append(seed));
}

abstract class Filter {
  name: string
  protected tcol: DataTables.ColumnMethods
  private label: JQuery<HTMLSpanElement>

  constructor(public field: Field) {
    this.name = this.field.name
    this.tcol = TCat.column(this.name + ':name');
    // columnVisible(this.name, true);
    this.label = $(document.createElement('span'));
    this.label.append($('<button class="remove">&times;</button>')
      .on('click', this.remove.bind(this)),
      this.field.title);
  }

  protected add(...nodes: Array<JQuery.TypeOrArray<JQuery.Node | JQuery<JQuery.Node>>>) {
    add_filt_row(this.field.name, this.label, ...nodes);
    Filters.push(this);
  }

  abstract update_aggs(aggs: Aggr): void;

  protected change(search: any, vis: boolean) {
    const i = Filters.indexOf(this);
    if (i >= 0 && Update_aggs > i)
      Update_aggs = i + 1;
    columnVisible(this.name, vis);
    this.tcol.draw();
  }

  protected remove() {
    if (!TCat) return;
    const i = Filters.indexOf(this);
    if (i < 0) return;
    Filters.splice(i, 1);
    $('tr#filt-' + this.name).remove();
    Update_aggs = i;
    columnVisible(this.name, true);
    this.tcol.draw();
  }

  abstract query(): string | undefined
  abstract pyQuery(): string | undefined
}

class SelectFilter extends Filter {
  select: HTMLSelectElement
  private value?: string

  constructor(field: Field) {
    super(field);

    this.select = document.createElement('select');
    this.select.name = this.field.name;
    this.select.disabled = true;
    this.select.onchange = this.change.bind(this);
    this.add(this.select);
  }

  update_aggs(aggs: AggrTerms<string>) {
    while (this.select.lastChild)
      this.select.removeChild(this.select.lastChild);
    fill_select_terms(this.select, this.field, aggs);
    if (this.value != null) {
      this.select.value = this.value;
      delete this.value;
    }
  }

  change() {
    const val = this.select.value;
    super.change(val, !val);
  }

  setValue(val: string) {
    if (this.select.disabled)
      this.value = val;
    else
      this.select.value = val;
  }

  query(): string | undefined {
    const val = this.select.value;
    if (val)
      return val;
  }

  pyQuery(): string | undefined {
    const val = this.select.value;
    if (val)
      return JSON.stringify(val);
  }
}

class NumericFilter extends Filter {
  lb: HTMLInputElement
  ub: HTMLInputElement
  private avg: HTMLSpanElement

  private makeBound(w: boolean): HTMLInputElement {
    const b = <HTMLInputElement>document.createElement('input');
    b.name = this.name + "." + (w ? "u" : "l") + "b";
    b.title = (w ? "Upper" : "Lower") + " bound for " + this.field.title + " values"
    b.type = "number";
    b.step = this.field.base == "i" ? <any>1 : "any";
    b.disabled = true;
    b.onchange = this.change.bind(this);
    return b;
  }

  constructor(field: Field) {
    super(field);
    this.lb = this.makeBound(false);
    this.ub = this.makeBound(true);
    this.avg = document.createElement('span');
    this.avg.innerHTML = "<em>loading...</em>";
    this.add(
      $('<span>').append(this.lb).append(' &ndash; ').append(this.ub),
      $('<span><em>&mu;</em> = </span>').append(this.avg),
      $('<button>histogram</button>').on('click', this.histogram.bind(this)),
      $('<button>reset</button>').on('click', this.reset.bind(this))
    );
  }

  get lbv(): number {
    return this.lb.valueAsNumber;
  }

  get ubv(): number {
    return this.ub.valueAsNumber;
  }

  update_aggs(aggs: AggrStats) {
    this.lb.defaultValue = this.lb.min = this.ub.min = <any>aggs.min;
    this.ub.defaultValue = this.lb.max = this.ub.max = <any>aggs.max;
    this.lb.disabled = this.ub.disabled = false;
    this.avg.textContent = <any>aggs.avg;
  }

  change() {
    super.change(this.lbv + " TO " + this.ubv, this.lbv != this.ubv);
  }

  query(): string {
    const lbv = this.lbv;
    const ubv = this.ubv;
    if (lbv == ubv)
      return <any>lbv;
    else
      return lbv + ' ' + ubv;
  }

  pyQuery(): string {
    const lbv = this.lbv;
    const ubv = this.ubv;
    if (lbv == ubv)
      return JSON.stringify(lbv);
    else
      return '(' + JSON.stringify(lbv) + ', ' + JSON.stringify(ubv) + ')';
  }

  private histogram() {
    if (Histogram !== this) {
      Histogram = this;
      this.tcol.draw(false);
    }
    else {
      Histogram = undefined;
      $('#dhist').hide();
    }
  }

  private reset() {
    this.lb.value = this.lb.defaultValue;
    this.ub.value = this.ub.defaultValue;
    this.change();
  }

  protected remove() {
    super.remove();
    if (Histogram === this) {
      Histogram = undefined;
      $('#dhist').hide();
    }
  }

  setRange(lbv: number, ubv: number) {
    this.lb.valueAsNumber = lbv;
    this.ub.valueAsNumber = ubv;
  }
}

function add_filter(idx: number): Filter | undefined {
  const field = Catalog.fields[idx];
  if (!TCat || !field)
    return;
  let filt = Filters.find((f) => f.field.name === field.name);
  if (filt)
    return filt;
  if (field.terms)
    return new SelectFilter(field);
  return new NumericFilter(field);
}

function colvisNames(box: HTMLInputElement): string[] {
  const l: string[] = [];
  for (let k of <any>box.classList as string[]) {
    l.push(k.substr(7));
  }
  return l;
}

function colvisUpdate(box: HTMLInputElement, vis: boolean) {
  const v: boolean[] = (<any>TCat.columns(colvisNames(box).map(n => n + ":name")).visible() as JQuery<boolean>).toArray();
  // vis = v.shift();
  box.checked = vis;
  box.indeterminate = v.some(x => x !== vis);
}

function columnVisible(name: string, vis: boolean) {
  TCat.column(name + ":name").visible(vis);
  for (let b of <any>document.getElementsByClassName('colvis-' + name) as Element[])
    colvisUpdate(<HTMLInputElement>b, vis);
  if (vis && Last_fields.indexOf(name) < 0)
    TCat.draw();
}

(<any>window).colvisSet = function colvisSet(event: Event) {
  const box = <HTMLInputElement>event.target;
  if (!box.indeterminate)
    for (let n of colvisNames(box))
      columnVisible(n, box.checked);
}

function py_text() {
  const cat = Catalog.name;
  let st = "import fi_astrosims.client\n" + cat + " = fi_astrosims.client.Simulation(" + JSON.stringify(cat) + ")\nq = fi_astrosims.client.Query(" + cat;
  for (let i = 0; i < Filters.length; i++) {
    const q = Filters[i].pyQuery();
    if (q != null)
      st += ", " + Filters[i].name + ' = ' + q;
  }
  if (Sample < 1) {
    st += ", sample = " + Sample;
    if (Seed != undefined)
      st += ", seed = " + Seed;
  }
  st += ')\ndat = q.numpy()';
  (<HTMLPreElement>document.getElementById('code-py')).textContent = st;
}

function url_update(query: Dict<string>) {
  history.replaceState({}, '', location.origin + location.pathname + '?' + $.param(query));
}

function render_funct(field: Field): (data: any) => string {
  if (field.base === 'f')
    return (data) => data != undefined ? parseFloat(data).toPrecision(8) : data;
  if (field.enum) {
    const e: string[] = field.enum;
    return (data) => data in e ? e[data] : data;
  }
  return (data) => data;
}

(<any>window).toggleDisplay = function toggleDisplay(ele: string) {
  $('#'+ele).toggle();
}

export function initCatalog(table: JQuery<HTMLTableElement>) {
  for (let i = 0; i < Catalog.fields.length; i++)
    Fields_idx[Catalog.fields[i].name] = i;
  const topts: DataTables.Settings = {
    serverSide: true,
    ajax: ajax,
    deferLoading: 1,
    scrollX: true,
    pageLength: 25,
    processing: true,
    language: {
      emptyTable: "",
      zeroRecords: ""
    },
    dom: 'i<"#download">rtlp',
    deferRender: true,
    pagingType: 'simple',
    columns: Catalog.fields.map((c) => {
      return {
        name: c.name,
        className: c.base === 'f' || c.base === 'i' ? 'dt-body-right' : 'dt-body-left',
        render: render_funct(c)
      };
    })
  };
  if ((<any>window).Query) {
    if (Query.sample != null)
      Sample = Query.sample;
    if (Query.seed != null)
      Seed = Query.seed;
    if (Query.offset)
      topts.displayStart = Query.offset
    if (Query.limit)
      topts.pageLength = Query.limit;
    if (Query.sort)
      topts.order = Query.sort.map((o) => {
        return [Fields_idx[o.field], o.asc ? 'asc' : 'desc'];
      });
    if (Query.fields && Query.fields.length && topts.columns)
      for (let c of topts.columns)
        c.visible = Query.fields.indexOf(<string>c.name) >= 0; // FIXME: colvis
  }
  TCat = table.DataTable(topts);
  /* for debugging: */
  (<any>window).TCat = TCat;
  const addfilt = <HTMLSelectElement>document.createElement('select');
  addfilt.appendChild(document.createElement('option'));
  add_filt_row('', addfilt, 'Select field to filter');
  add_sample();
  for (let i = 0; i < Catalog.fields.length; i++) {
    const f = Catalog.fields[i];
    const opt = field_option(f);
    opt.value = <any>i;
    addfilt.appendChild(opt);
    if (f.top)
      add_filter(i);
  }
  addfilt.onchange = function () {
    add_filter(<any>addfilt.value);
    TCat.draw(false);
  };
  if ((<any>window).Query && Query.filter) {
    for (let f of Query.filter) {
      const fi = Fields_idx[f.field];
      if (fi == null)
        continue;
      const filt = add_filter(fi);
      if (filt instanceof NumericFilter && typeof (f.value) === 'object')
        filt.setRange(f.value.lb, f.value.ub);
      if (filt instanceof SelectFilter && typeof (f.value) !== 'object')
        filt.setValue(f.value);
    }
  }
  TCat.draw();
}