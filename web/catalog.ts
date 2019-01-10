"use strict";

import $ from "jquery";
import Datatables from "datatables.net";
import Highcharts from "highcharts";
import Highcharts_heatmap from "highcharts/modules/heatmap";
import { assert, Dict, Field, Catalog, AggrStats, AggrTerms, Aggr, CatalogResponse, fill_select_terms, field_option, field_title, toggle_log, axis_title, render_funct, histogram_options } from "./common";

Datatables(window, $);
Highcharts_heatmap(Highcharts);

var TCat: DataTables.Api;
declare const Catalog: Catalog;
declare const Query: {offset:number, limit:number, sort:{field:string,asc:boolean}[], fields:string[], filter:{field:string,value:string|{lb:number,ub:number}}[], sample:number, seed:number|undefined, aggs:string[], hist:string|null};
const Fields_idx: Dict<number> = {};
const Filters: Array<Filter> = [];
var Sample: number = 1;
var Seed: undefined|number;
var Update_aggs: number = -1;
var Histogram: undefined|NumericFilter;
var Heatmap: undefined|Field;
var Histogram_chart: Highcharts.ChartObject | undefined;
var Hist_chart_x: Highcharts.ChartObject | undefined;
var Hist_chart_y: Highcharts.ChartObject | undefined;
var Last_fields: string[] = [];

function set_download(query: Dict<string>) {
  const q = '?' + $.param(query);
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

function histogramRemove() {
  if (Histogram_chart)
    Histogram_chart.destroy();
  Histogram_chart = undefined;
  Histogram = undefined;
  Heatmap = undefined;
  (<HTMLSelectElement>document.getElementById('histsel')).value = '';
  $('#dhist').hide();
  $('#hist').empty();
  $('#dhist_table').hide();
  $('#hist_x').empty();
  $('#hist_y').empty();
  $('#scatter_xy').empty();
  $('#plotspace').hide();
}

function zoomRange(f: NumericFilter, wid: number, axis: Highcharts.AxisOptions) {
  f.setRange(wid*Math.floor((<number>axis.min)/wid),
             wid*Math.ceil( (<number>axis.max)/wid));
  f.change();
}

function histDraw(hist: NumericFilter, data: number[][], size: number[], field: Field, inv: boolean, xLab: boolean) {
  const f = field;
  const opts: Highcharts.Options = histogram_options(f);
  const wid = (inv) ? size[1] : size[0];
  const chart_type = (inv) ? "bar" : "column"; 
  if (inv) {
    (<Highcharts.AxisOptions>opts.xAxis).reversed = false;
  }
  else { // the resizing issue for the y axis plot
    (<Highcharts.AxisOptions>opts.xAxis).min = hist.lbv;
    (<Highcharts.AxisOptions>opts.xAxis).max = hist.ubv + wid;
  }
  (<Highcharts.ChartOptions>opts.chart).events = {
    selection: function (event: Highcharts.ChartSelectionEvent) {
      event.preventDefault();
      zoomRange(hist, wid, event.xAxis[0]);
      return false; // Don't zoom
    }
  };
  (<Highcharts.TooltipOptions>opts.tooltip).footerFormat = 'drag to filter';
  opts.series = [<Highcharts.ColumnChartSeriesOptions>{
    showInLegend: true,
    type: chart_type,
    data: data,
    pointInterval: wid,
    pointRange: wid,
    color: '#0008'
  }];
  if (!xLab) {
    (<Highcharts.AxisOptions>opts.xAxis).title = false;
    (<Highcharts.AxisOptions>opts.xAxis).opposite = true;
  }
  return (opts);
}

function histogramDraw(hist: NumericFilter, heatmap: undefined | Field, agg: AggrTerms<number>, size: number[]) {
  $('#plotspace').show();
  const field = hist.field;
  const data: number[][] = [];
  const hist_x_data: number[][] = [];
  const hist_y_data: number[][] = [];
  let hist_y_dict = [];
  for (let x of agg.buckets) {
    if (heatmap && x.hist)
      for (let y of x.hist.buckets) {
        data.push([x.key, y.key, y.doc_count]);
        if (y.key in hist_y_dict) 
          hist_y_dict[y.key] = hist_y_dict[y.key] + y.doc_count;
        else 
          hist_y_dict[y.key] = y.doc_count;
      }
      hist_x_data.push([x.key, x.doc_count]);
  }
  for (var key in hist_y_dict) {
    hist_y_data.push([parseFloat(key), hist_y_dict[key]]);
  }
  hist_y_data.sort(function (a, b) {
    if (a[0] == b[0]) return 0;
    return a[0] < b[0] ? -1 : 1;
  });
  if (data.length <= 1 && hist_x_data.length <= 1)
    return histogramRemove();
  const opts: Highcharts.Options = histogram_options(field);
  if (heatmap) {
    $('#dhist').hide();
    $('#hist').empty();
    const wid = size.map(s => s/2);
    for (let d of data) {
      for (let i = 0; i < wid.length; i++)
        d[i] += wid[i];
    }
    (<Highcharts.ChartOptions>opts.chart).zoomType = 'xy';
    (<Highcharts.ChartOptions>opts.chart).events = {
      selection: function (event: Highcharts.ChartSelectionEvent) {
        event.preventDefault();
        zoomRange(hist, wid[0], event.xAxis[0]);
        const hm = add_filter(Fields_idx[heatmap.name]);
        if (hm instanceof NumericFilter) {
          zoomRange(hm, wid[1], event.yAxis[0]);
        }
        return false; // Don't zoom
      }
    };
    const renderx = render_funct(hist.field);
    const rendery = render_funct(heatmap);
    (<Highcharts.TooltipOptions>opts.tooltip).formatter = function (this: {point: {x: number, y: number, value: number}}): string {
      const p = this.point;
      return '[' + renderx(p.x-wid[0]) + ',' + renderx(p.x+wid[0]) + ') & ' + 
             '[' + rendery(p.y-wid[1]) + ',' + rendery(p.y+wid[1]) + '): ' +
             p.value;
    };
    opts.colorAxis = <Highcharts.ColorAxisOptions>opts.yAxis;
    opts.colorAxis.minColor = '#ffffff';
    opts.colorAxis.reversed = false;
   // (<Highcharts.LegendOptions>opts.legend).enabled = true;
    opts.xAxis = {
      min: hist.lbv,
      max: hist.ubv + size[0],
      gridLineWidth: 1
    },
    opts.yAxis = {
      type: 'linear',
      title: axis_title(heatmap)
    };
    opts.series = [<Highcharts.HeatMapSeriesOptions>{
      type: 'heatmap',
      data: data,
      colsize: size[0],
      rowsize: size[1]
    }];
    Histogram_chart = Highcharts.chart('scatter_xy', opts);
    Hist_chart_x = Highcharts.chart('hist_x', histDraw(hist, hist_x_data, size, field, false, false));
    // fix resizing issue -> hm only adds the filter when the query is called so can't resize based on the filter
    Hist_chart_y = Highcharts.chart('hist_y', histDraw(hist, hist_y_data, size, heatmap, true, false));
    $('#dhist_table').show();
  } else {
    $('#dhist_table').hide();
    $('#hist_x').empty();
    $('#hist_y').empty();
    $("scatter_xy").empty();
    Histogram_chart = Highcharts.chart('hist', histDraw(hist, hist_x_data, size, field, false, true));
    $('#dhist').show();
  }
}

function toggleLog() {
  if (Histogram_chart)
    toggle_log(Histogram_chart);
};

(<any>window).toggleLogHeat = function toggleLogHeat() {
  if (Hist_chart_x)
    toggle_log(Hist_chart_x);
  if (Hist_chart_y)
    toggle_log(Hist_chart_y);
  toggleLog();
};

(<any>window).histogramSelect = function histogramSelect() {
  const sel = <HTMLSelectElement>document.getElementById('histsel');
  Heatmap = Catalog.fields[Fields_idx[sel.value]];
  update(false);
};

/* elasticsearch max_result_window */
const DisplayLimit = 10000;
var Update: boolean|number = false;
var Update_paging: boolean = false;

function update(paging: boolean = true) {
  if (Update)
    return;
  if (paging)
    Update_paging = paging;
  Update = setTimeout(() => {
    Update = true;
    TCat.draw(Update_paging);
    Update_paging = false;
  });
}

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
  const histogram = Histogram;
  const heatmap = Histogram && Heatmap;
  if (histogram) {
    if (heatmap)
      query.hist = histogram.name+':16 ' + heatmap.name+':16';
    else
      query.hist = histogram.name+':128';
  }
  $.ajax({
    method: 'GET',
    url: '/' + Catalog.name + '/catalog',
    data: query
  }).then((res: CatalogResponse) => {
    Update = false;
    Catalog.count = Math.max(Catalog.count || 0, res.hits.total);
    const settings = (<any>TCat.settings())[0];
    settings.oLanguage.sInfo = "Showing _START_ to _END_ of " + settings.fnFormatNumber(res.hits.total);
    callback({
      draw: data.draw,
      recordsTotal: Catalog.count,
      recordsFiltered: Math.min(res.hits.total, DisplayLimit),
      data: res.hits.hits
    });
    for (let filt of aggs)
      filt.update_aggs((res.aggregations as Dict<Aggr>)[filt.name]);
    Update_aggs = Filters.length;
    if (histogram && res.aggregations && res.aggregations.hist)
      histogramDraw(histogram, heatmap, res.aggregations.hist as AggrTerms<number>, res.histsize);

    delete query.aggs;
    delete query.hist;
   // url_update(query);
    delete query.limit;
    delete query.offset;
    set_download(query);
    py_text(query);
  }, (xhr, msg, err) => {
    Update = false;
    callback({
      draw: data.draw,
      data: [],
      error: msg + ": " + err
    });
  });
}

function add_filt_row(name: string, ...nodes: Array<JQuery.htmlString | JQuery.TypeOrArray<JQuery.Node | JQuery<JQuery.Node>>>) {
  const id = 'filt-'+name;
  let tr = <HTMLTableRowElement|null>document.getElementById(id);
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
      Seed = undefined;
    update();
  };

  add_filt_row('sample', 'random sample',
    $('<span>').append('fraction ').append(samp),
    $('<span>').append('seed ').append(seed));
}

abstract class Filter {
  protected tcol: DataTables.ColumnMethods
  private label: HTMLSpanElement

  constructor(public field: Field) {
    this.tcol = TCat.column(this.name+':name');
    this.label = field_title(this.field, this.remove.bind(this));
  }

  get name(): string {
    return this.field.name;
  }

  protected add(...nodes: Array<JQuery.TypeOrArray<JQuery.Node | JQuery<JQuery.Node>>>) {
    add_filt_row(this.field.name, this.label, ...nodes);
    Filters.push(this);
  }

  abstract update_aggs(aggs: Aggr): void;

  protected change(search: any, vis: boolean) {
    const i = Filters.indexOf(this);
    if (i >= 0 && Update_aggs > i)
      Update_aggs = i+1;
    columnVisible(this.name, vis);
    update();
  }

  protected remove() {
    if (!TCat) return;
    const i = Filters.indexOf(this);
    if (i < 0) return;
    Filters.splice(i, 1);
    $('tr#filt-'+this.name).remove();
    Update_aggs = i;
    columnVisible(this.name, true);
    update();
  }

  abstract query(): string|undefined
  abstract pyQuery(): string|undefined
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

  query(): string|undefined {
    const val = this.select.value;
    if (val)
      return val;
  }

  pyQuery(): string|undefined {
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
    b.name = this.name+"."+(w?"u":"l")+"b";
    b.title = (w?"Upper":"Lower")+" bound for " + this.field.title + " values"
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
    if (this.lb.disabled)
      this.lb.disabled = false;
    else
      this.lb.value = <any>aggs.min;
    if (this.ub.disabled)
      this.ub.disabled = false;
    else
      this.ub.value = <any>aggs.max;
    this.avg.textContent = render_funct(this.field)(aggs.avg);
  }

  change() {
    super.change(this.lbv+" TO "+this.ubv, this.lbv!=this.ubv);
  }

  query(): string {
    const lbv = this.lbv;
    const ubv =  this.ubv;
    if (lbv == ubv)
      return <any>lbv;
    else
      return lbv+' '+ubv;
  }

  pyQuery(): string {
    const lbv = this.lbv;
    const ubv = this.ubv;
    if (lbv == ubv)
      return JSON.stringify(lbv);
    else
      return '(' + JSON.stringify(lbv) + ', ' + JSON.stringify(ubv) + ')';
  }

  private histogramRemove(): boolean {
    if (Histogram !== this)
      return false;
    histogramRemove();
    return true;
  }

  private histogram() {
    if (!this.histogramRemove()) {
      Histogram = this;
      update(false);
    }
  }

  private reset() {
    this.lb.value = this.lb.defaultValue;
    this.ub.value = this.ub.defaultValue;
    this.change();
  }

  protected remove() {
    this.histogramRemove();
    super.remove();
  }

  setRange(lbv: number, ubv: number) {
    this.lb.valueAsNumber = lbv;
    this.ub.valueAsNumber = ubv;
  }
}

function add_filter(idx: number): Filter|undefined {
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
    if (k.startsWith('colvis-'))
      l.push(k.substr(7));
  }
  return l;
}

function colvisUpdate(box: HTMLInputElement, vis?: boolean) {
  const v: boolean[] = (<any>TCat.columns(colvisNames(box).map(n => n+":name")).visible() as JQuery<boolean>).toArray();
  if (vis == null)
    vis = v.shift() || false;
  box.checked = vis;
  box.indeterminate = v.some(x => x !== vis);
}

function columnVisible(name: string, vis: boolean) {
  TCat.column(name+":name").visible(vis);
  for (let b of <any>document.getElementsByClassName('colvis-'+name) as Element[])
    colvisUpdate(<HTMLInputElement>b, vis);
  if (vis && Last_fields.indexOf(name) < 0)
    update(false);
}

(<any>window).colvisSet = function colvisSet(event: Event) {
  const box = <HTMLInputElement>event.target;
  if (!box.indeterminate)
    for (let n of colvisNames(box))
      columnVisible(n, box.checked);
}

function py_text(query: Dict<string>) {
  const cat = Catalog.name;
  let st = "import fi_astrosims.client\n"
    + cat + " = fi_astrosims.client.Simulation(" + JSON.stringify(cat) + ", host = " + JSON.stringify(location.origin) + ")\n"
    + "q = " + cat + ".query(fields = " + JSON.stringify(query.fields.split(' '));
  for (let i = 0; i < Filters.length; i++) {
    const q = Filters[i].pyQuery();
    if (q != null)
      st += ", " + Filters[i].name + ' = ' + q;
  }
  if (query.sort)
    st += ", sort = " + JSON.stringify(query.sort.split(' '));
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

export function initCatalog(table: JQuery<HTMLTableElement>) {
  for (let i = 0; i < Catalog.fields.length; i++)
    Fields_idx[Catalog.fields[i].name] = i;
  const topts: DataTables.Settings = {
    serverSide: true,
    ajax: ajax,
    deferLoading: 1,
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
    if (Query.fields && Query.fields.length && topts.columns) {
      for (let c of topts.columns)
        c.visible = Query.fields.indexOf(<string>c.name) >= 0;
    }
  }
  TCat = table.DataTable(topts);
  /* for debugging: */
  (<any>window).TCat = TCat;
  const addfilt = <HTMLSelectElement>document.createElement('select');
  const aopt = document.createElement('option');
  aopt.value = '';
  aopt.text = 'Add filter...';
  addfilt.appendChild(aopt);
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
    if (add_filter(<any>addfilt.value))
      update(false);
  };
  if ((<any>window).Query && Query.filter) {
    for (let f of Query.filter) {
      const fi = Fields_idx[f.field];
      if (fi == null)
        continue;
      const filt = add_filter(fi);
      if (filt instanceof NumericFilter && typeof(f.value) === 'object')
        filt.setRange(f.value.lb, f.value.ub);
      if (filt instanceof SelectFilter && typeof(f.value) !== 'object')
        filt.setValue(f.value);
    }
  }
  for (let b of <any>document.getElementsByClassName('colvis') as HTMLInputElement[])
    colvisUpdate(b);
  update();
  (<any>window).toggleLog = toggleLog;
}
