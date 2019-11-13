"use strict";

import $ from "jquery";
import Datatables from "datatables.net";
import Highcharts from "highcharts";
import Highcharts_more from "highcharts/highcharts-more";
import Highcharts_heatmap from "highcharts/modules/heatmap";
import {
  assert,
  Dict,
  Field,
  Catalog,
  AggrStats,
  AggrTerms,
  Aggr,
  CatalogResponse,
  fill_select_terms,
  field_option,
  field_title,
  toggle_log,
  axis_title,
  render_funct,
  histogram_options,
  updateMathJax
} from "./common";

Datatables(window, $);
Highcharts_more(Highcharts);
Highcharts_heatmap(Highcharts);

var TCat: DataTables.Api;
declare const Catalog: Catalog;
declare const Query: {
  offset: number;
  limit: number;
  sort: { field: string; asc: boolean }[];
  fields: string[];
  filter: { field: string; value: string | { lb: number; ub: number } }[];
  sample: number;
  seed: number | undefined;
  aggs: string[];
  hist: string | null;
};
const Fields_idx: Dict<number> = {};
const Filters: Array<Filter> = [];
var Sample: number = 1;
var Seed: undefined | number;
var Update_aggs: number = -1;
var Histogram: undefined | NumericFilter;
var Heatmap: undefined | Field;
var Histcond: boolean = false;
var Histogram_chart: Highcharts.ChartObject | undefined;
var Last_fields: string[] = [];
var Last_query: undefined | Dict<string>;

function set_download(query: Dict<string>) {
  const q = "?" + $.param(query);
  const h = $("#download").html("download as ");
  for (let f of Catalog.bulk) {
    const a = document.createElement("a");
    h.append(a);
    a.id = "download." + f;
    a.className = "button button-primary";
    a.href = "/" + Catalog.name + "/" + f + q;
    a.appendChild(document.createTextNode(f));
    h.append(document.createTextNode(" "));
  }
  py_text(query);
}

function histogramRemove() {
  if (Histogram_chart) Histogram_chart.destroy();
  Histogram_chart = undefined;
  Histogram = undefined;
  Heatmap = undefined;
  $("#dhist").hide();
  $("#hist").empty();
}

function zoomRange(
  f: NumericFilter,
  wid: number,
  axis: Highcharts.AxisOptions
) {
  f.setRange(
    wid * Math.floor(<number>axis.min / wid),
    wid * Math.ceil(<number>axis.max / wid)
  );
  f.change();
}

function histogramDraw(
  hist: NumericFilter,
  heatmap: undefined | Field,
  cond: boolean,
  agg: AggrTerms<number>,
  size: Dict<number>
) {
  const field = hist.field;
  const data: number[][] = [];
  let xwid = size[field.name] / 2;
  let ywid = heatmap ? size[heatmap.name] / 2 : NaN;
  let cmax = 0;
  for (let x of agg.buckets) {
    if (heatmap) {
      if (cond && x.pct) {
        data.push([
          x.key + xwid,
          x.pct.values["0.0"],
          x.pct.values["25.0"],
          x.pct.values["50.0"],
          x.pct.values["75.0"],
          x.pct.values["100.0"],
          x.doc_count
        ]);
        if (x.doc_count > cmax) cmax = x.doc_count;
      } else if (x.hist)
        for (let y of x.hist.buckets)
          data.push([x.key + xwid, y.key + ywid, y.doc_count]);
    } else data.push([x.key, x.doc_count]);
  }
  if (data.length <= 1) return histogramRemove();

  const opts: Highcharts.Options = histogram_options(field);
  if (heatmap && cond) {
    const renderx = render_funct(hist.field);
    (<Highcharts.TooltipOptions>opts.tooltip).formatter = function(
      this: Highcharts.PointObject
    ): string {
      return (
        "[" +
        renderx(this.x - xwid) +
        "," +
        renderx(this.x + xwid) +
        "): " +
        this.y
      );
    };
    opts.colorAxis = <Highcharts.ColorAxisOptions>opts.yAxis;
    opts.colorAxis.minColor = "#bbbbbb";
    opts.colorAxis.reversed = false;
    opts.colorAxis.max = cmax;
    opts.yAxis = {
      type: "linear",
      title: axis_title(heatmap)
    };
    opts.series = [
      <Highcharts.BoxPlotChartSeriesOptions>{
        type: "boxplot",
        data: data,
        keys: ["x", "low", "q1", "median", "q3", "high", "c"],
        colorKey: "c"
      },
      <Highcharts.LineChartSeriesOptions>{
        type: "line",
        data: data,
        keys: ["x", "min", "q1", "y", "q3", "max", "c"],
        colorKey: "c"
      }
    ];
  } else if (heatmap) {
    (<Highcharts.ChartOptions>opts.chart).zoomType = "xy";
    (<Highcharts.ChartOptions>opts.chart).events = {
      selection: function(event: Highcharts.ChartSelectionEvent) {
        event.preventDefault();
        zoomRange(hist, xwid, event.xAxis[0]);
        const hm = add_filter(Fields_idx[heatmap.name]);
        if (hm instanceof NumericFilter) zoomRange(hm, ywid, event.yAxis[0]);
        return false; // Don't zoom
      }
    };
    const renderx = render_funct(hist.field);
    const rendery = render_funct(heatmap);
    (<Highcharts.TooltipOptions>opts.tooltip).formatter = function(this: {
      point: { x: number; y: number; value: number };
    }): string {
      const p = this.point;
      return (
        "[" +
        renderx(p.x - xwid) +
        "," +
        renderx(p.x + xwid) +
        ") & " +
        "[" +
        rendery(p.y - ywid) +
        "," +
        rendery(p.y + ywid) +
        "): " +
        p.value
      );
    };
    opts.colorAxis = <Highcharts.ColorAxisOptions>opts.yAxis;
    opts.colorAxis.minColor = "#ffffff";
    opts.colorAxis.reversed = false;
    (<Highcharts.LegendOptions>opts.legend).enabled = true;
    opts.yAxis = {
      type: "linear",
      title: axis_title(heatmap)
    };
    opts.series = [
      <Highcharts.HeatMapSeriesOptions>{
        type: "heatmap",
        data: data,
        colsize: 2 * xwid,
        rowsize: 2 * ywid
      }
    ];
  } else {
    const wid = 2 * xwid;
    (<Highcharts.ChartOptions>opts.chart).events = {
      selection: function(event: Highcharts.ChartSelectionEvent) {
        event.preventDefault();
        zoomRange(hist, wid, event.xAxis[0]);
        return false; // Don't zoom
      }
    };
    (<Highcharts.TooltipOptions>opts.tooltip).footerFormat = "drag to filter";
    (<Highcharts.AxisOptions>opts.xAxis).min = hist.lbv;
    (<Highcharts.AxisOptions>opts.xAxis).max = hist.ubv + wid;
    opts.series = [
      <Highcharts.ColumnChartSeriesOptions>{
        showInLegend: true,
        type: "column",
        data: data,
        pointInterval: wid,
        pointRange: wid,
        color: "#0008"
      }
    ];
  }
  $("#dhist").show();
  Histogram_chart = Highcharts.chart("hist", opts);
}

function toggleLog() {
  if (Histogram_chart) toggle_log(Histogram_chart);
}

(<any>window).histogramShow = function histogramShow(axis: "x" | "y" | "c") {
  if (axis == "x") Heatmap = undefined;
  else {
    const sely = <HTMLSelectElement>document.getElementById("histsel-y");
    Heatmap = Catalog.fields[Fields_idx[sely.value]];
    Histcond = axis == "c";
  }
  const selx = <HTMLSelectElement>document.getElementById("histsel-x");
  const filt = add_filter(Fields_idx[selx.value]);
  if (filt instanceof NumericFilter) Histogram = filt;
  else histogramRemove();
  update(false);
};

/* elasticsearch max_result_window */
const DisplayLimit = 10000;
var Update: boolean | number = false;
var Update_paging: boolean = false;

function update(paging: boolean = true) {
  if (Update) return;
  if (paging) Update_paging = paging;
  Update = setTimeout(() => {
    Update = true;
    TCat.draw(Update_paging);
    Update_paging = false;
  });
  const modal = <HTMLSelectElement>document.getElementById("progress-modal");
  modal.classList.remove("hidden");
}

function ajax(data: any, callback: (data: any) => void, opts: any) {
  const query: Dict<string> = {
    sort: data.order
      .map((o: any) => {
        return (o.dir == "asc" ? "" : "-") + data.columns[o.column].data;
      })
      .join(" ")
  };

  const modal = <HTMLSelectElement>document.getElementById("progress-modal");
  modal.classList.remove("hidden");

  let aggs = Filters;
  if (Update_aggs >= 0) aggs = aggs.slice(Update_aggs);
  else Update_aggs = Filters.length;
  for (let fi = 0; fi < Update_aggs; fi++) {
    const filt = Filters[fi];
    const q = filt.query();
    if (q != null) query[filt.name] = q;
  }
  if (Sample < 1) {
    query.sample = <any>Sample;
    if (Seed != undefined) query.sample += "@" + Seed;
  }

  query.offset = data.start;
  query.limit = data.length;
  Last_fields = TCat.columns(":visible")
    .dataSrc()
    .toArray();
  query.fields = Last_fields.join(" ");
  if (aggs) query.aggs = aggs.map(filt => filt.name).join(" ");
  const histogram = Histogram;
  const heatmap = Histogram && Heatmap;
  const histcond = Histcond;
  if (histogram) {
    if (heatmap) {
      if (histcond) query.hist = histogram.name + ":64 " + heatmap.name;
      else query.hist = histogram.name + ":16 " + heatmap.name + ":16";
    } else query.hist = histogram.name + ":128";
  }
  $.ajax({
    method: "GET",
    url: "/" + Catalog.name + "/catalog",
    data: query
  }).then(
    (res: CatalogResponse) => {
      const modal = <HTMLSelectElement>(
        document.getElementById("progress-modal")
      );
      modal.classList.add("hidden");
      Update = false;
      Catalog.count = Math.max(Catalog.count || 0, res.hits.total);
      const settings = (<any>TCat.settings())[0];
      settings.oLanguage.sInfo =
        "Showing _START_ to _END_ of " +
        settings.fnFormatNumber(res.hits.total);
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
        histogramDraw(
          histogram,
          heatmap,
          histcond,
          res.aggregations.hist as AggrTerms<number>,
          res.histsize
        );
      delete query.aggs;
      delete query.hist;
      url_update(query);
      delete query.limit;
      delete query.offset;
      set_download((Last_query = query));
    },
    (xhr, msg, err) => {
      const modal = <HTMLSelectElement>(
        document.getElementById("progress-modal")
      );
      modal.classList.add("hidden");
      Update = false;
      callback({
        draw: data.draw,
        data: [],
        error: msg + ": " + err
      });
    }
  );
}

function add_filt_row(
  name: string,
  isTop: boolean | undefined,
  ...nodes: Array<
    JQuery.htmlString | JQuery.TypeOrArray<JQuery.Node | JQuery<JQuery.Node>>
  >
) {
  const id = "filt-" + name;
  let tr = <HTMLTableRowElement | null>document.getElementById(id);
  if (tr) return;
  const tab = <HTMLTableElement>document.getElementById("filt");
  tr = document.createElement("div");
  tr.id = id;
  tr.classList.add("alert", "fade", "show", "row", "filter-row");
  if (!name.length) {
    tr.classList.add("alert-secondary");
  } else if (isTop) {
    tr.classList.add("alert-info");
  } else {
    tr.classList.add("alert-warning");
  }
  if (tab.lastChild) $(tr).insertBefore(<HTMLTableRowElement>tab.lastChild);
  else $(tr).appendTo(tab);
  for (let node of nodes) {
    const td = $(document.createElement("div")).appendTo(tr);
    td.append(node);
  }
}

(<any>window).sampleChange = function sampleChange() {
  const samp = <HTMLInputElement>document.getElementById("sample");
  const seed = <HTMLInputElement>document.getElementById("seed");
  Sample = samp.valueAsNumber;
  if (!isFinite(Sample)) Sample = 1;
  if ((seed.disabled = Sample >= 1)) seed.value = "";
  Seed = seed.valueAsNumber;
  if (!isFinite(Seed)) Seed = undefined;
  update();
};

abstract class Filter {
  protected tcol: DataTables.ColumnMethods;
  private label: HTMLSpanElement;

  constructor(public field: Field) {
    this.tcol = TCat.column(this.name + ":name");
    this.label = field_title(this.field, this.remove.bind(this));
  }

  get name(): string {
    return this.field.name;
  }

  protected add(
    ...nodes: Array<JQuery.TypeOrArray<JQuery.Node | JQuery<JQuery.Node>>>
  ) {
    add_filt_row(this.field.name, this.field.top, this.label, ...nodes);
    Filters.push(this);
  }

  abstract update_aggs(aggs: Aggr): void;

  protected change(search: any, vis: boolean) {
    const i = Filters.indexOf(this);
    if (i >= 0 && Update_aggs > i) Update_aggs = i + 1;
    columnVisible(this.name, vis);
    update();
  }

  protected remove() {
    if (!TCat) return;
    const i = Filters.indexOf(this);
    if (i < 0) return;
    Filters.splice(i, 1);
    $("div#filt-" + this.name).remove();
    Update_aggs = i;
    columnVisible(this.name, true);
    update();
  }

  abstract query(): string | undefined;
  abstract pyQuery(): string | undefined;
}

class SelectFilter extends Filter {
  select: HTMLSelectElement;
  private value?: string;

  constructor(field: Field) {
    super(field);

    this.select = document.createElement("select");
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
    if (this.select.disabled) this.value = val;
    else this.select.value = val;
  }

  query(): string | undefined {
    const val = this.select.value;
    if (val) return val;
  }

  pyQuery(): string | undefined {
    const val = this.select.value;
    if (val) return JSON.stringify(val);
  }
}

class NumericFilter extends Filter {
  lb: HTMLInputElement;
  ub: HTMLInputElement;
  private avg: HTMLSpanElement;

  private makeBound(w: boolean): HTMLInputElement {
    const b = <HTMLInputElement>document.createElement("input");
    b.name = this.name + "." + (w ? "u" : "l") + "b";
    b.title =
      (w ? "Upper" : "Lower") + " bound for " + this.field.title + " values";
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
    this.avg = document.createElement("span");
    this.avg.innerHTML = "<em>loading...</em>";
    this.add(
      $("<span>")
        .append(this.lb)
        .append(" &ndash; ")
        .append(this.ub),
      $("<span><em>&mu;</em> = </span>").append(this.avg),
      $("<button>reset</button>").on("click", this.reset.bind(this))
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
    if (this.lb.disabled) this.lb.disabled = false;
    else this.lb.value = <any>aggs.min;
    if (this.ub.disabled) this.ub.disabled = false;
    else this.ub.value = <any>aggs.max;
    this.avg.textContent = render_funct(this.field)(aggs.avg);
  }

  change() {
    super.change(this.lbv + " TO " + this.ubv, this.lbv != this.ubv);
  }

  query(): string {
    const lbv = this.lbv;
    const ubv = this.ubv;
    if (lbv == ubv) return <any>lbv;
    else return lbv + " " + ubv;
  }

  pyQuery(): string {
    const lbv = this.lbv;
    const ubv = this.ubv;
    if (lbv == ubv) return JSON.stringify(lbv);
    else return "(" + JSON.stringify(lbv) + ", " + JSON.stringify(ubv) + ")";
  }

  private histogramRemove(): boolean {
    if (Histogram !== this) return false;
    histogramRemove();
    return true;
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

function add_filter(idx: number): Filter | undefined {
  const field = Catalog.fields[idx];
  if (!TCat || !field) return;
  let filt = Filters.find(f => f.field.name === field.name);
  if (filt) return filt;
  if (field.terms) return new SelectFilter(field);
  return new NumericFilter(field);
}

function colvisNames(box: HTMLInputElement): string[] {
  const l: string[] = [];
  for (let k of (<any>box.classList) as string[]) {
    if (k.startsWith("colvis-")) l.push(k.substr(7));
  }
  return l;
}

function colvisUpdate(box: HTMLInputElement, vis?: boolean) {
  const v: boolean[] = ((<any>(
    TCat.columns(colvisNames(box).map(n => n + ":name")).visible()
  )) as JQuery<boolean>).toArray();
  if (vis == null) vis = v.shift() || false;
  box.checked = vis;
  box.indeterminate = v.some(x => x !== vis);
}

function columnVisible(name: string, vis: boolean) {
  TCat.column(name + ":name").visible(vis);
  for (let b of (<any>(
    document.getElementsByClassName("colvis-" + name)
  )) as Element[])
    colvisUpdate(<HTMLInputElement>b, vis);
  if (vis && Last_fields.indexOf(name) < 0) {
    update(false);
    updateMathJax();
  }
}

(<any>window).colvisSet = function colvisSet(event: Event) {
  const box = <HTMLInputElement>event.target;
  if (!box.indeterminate)
    for (let n of colvisNames(box)) columnVisible(n, box.checked);
  if (Last_query) {
    Last_query.fields = TCat.columns(":visible")
      .dataSrc()
      .toArray()
      .join(" ");
    set_download(Last_query);
  }
};

function py_text(query: Dict<string>) {
  const cat = Catalog.name;
  let st =
    "import fi_astrosims.client\n" +
    cat +
    " = fi_astrosims.client.Simulation(" +
    JSON.stringify(cat) +
    ", host = " +
    JSON.stringify(location.origin) +
    ")\n" +
    "q = " +
    cat +
    ".query(fields = " +
    JSON.stringify(query.fields.split(" "));
  for (let i = 0; i < Filters.length; i++) {
    const q = Filters[i].pyQuery();
    if (q != null) st += ", " + Filters[i].name + " = " + q;
  }
  if (query.sort) st += ", sort = " + JSON.stringify(query.sort.split(" "));
  if (Sample < 1) {
    st += ", sample = " + Sample;
    if (Seed != undefined) st += ", seed = " + Seed;
  }
  st += ")\ndat = q.numpy()";
  (<HTMLPreElement>document.getElementById("code-py")).textContent = st;
}

function url_update(query: Dict<string>) {
  history.replaceState(
    {},
    "",
    location.origin + location.pathname + "?" + $.param(query)
  );
}

export function initCatalog(table: JQuery<HTMLTableElement>) {
  for (let i = 0; i < Catalog.fields.length; i++)
    Fields_idx[Catalog.fields[i].name] = i;
  const topts: DataTables.Settings = {
    serverSide: true,
    ajax: ajax,
    deferLoading: 1,
    pageLength: 25,
    processing: false,
    language: {
      emptyTable: "",
      zeroRecords: ""
    },
    dom: 'i<"#download">rtlp',
    deferRender: true,
    pagingType: "simple",
    columns: Catalog.fields.map(c => {
      return {
        name: c.name,
        className:
          c.base === "f" || c.base === "i" ? "dt-body-right" : "dt-body-left",
        render: render_funct(c)
      };
    })
  };
  if (Catalog.sort) topts.order = Catalog.sort.map(o => [o, "asc"]);
  if ((<any>window).Query) {
    if (Query.sample != null) Sample = Query.sample;
    if (Query.seed != null) Seed = Query.seed;
    if (Query.offset) topts.displayStart = Query.offset;
    if (Query.limit) topts.pageLength = Query.limit;
    if (Query.sort)
      topts.order = Query.sort.map(o => {
        return [Fields_idx[o.field], o.asc ? "asc" : "desc"];
      });
    if (Query.fields && Query.fields.length && topts.columns) {
      for (let c of topts.columns)
        c.visible = Query.fields.indexOf(<string>c.name) >= 0;
    }
  }
  TCat = table.DataTable(topts);
  /* for debugging: */
  (<any>window).TCat = TCat;
  const addfilt = <HTMLSelectElement>document.createElement("select");
  const aopt = document.createElement("option");
  aopt.value = "";
  aopt.text = "Add filter...";
  addfilt.appendChild(aopt);
  // TODO: Add conditional styling to indicate not selected yet row (alert-something)
  add_filt_row("", false, addfilt, "Select field to filter");
  for (let i = 0; i < Catalog.fields.length; i++) {
    const f = Catalog.fields[i];
    const opt = field_option(f);
    opt.value = <any>i;
    addfilt.appendChild(opt);
    if (f.top) add_filter(i);
  }
  addfilt.onchange = function() {
    if (add_filter(<any>addfilt.value)) update(false);
  };
  if ((<any>window).Query && Query.filter) {
    for (let f of Query.filter) {
      const fi = Fields_idx[f.field];
      if (fi == null) continue;
      const filt = add_filter(fi);
      if (filt instanceof NumericFilter && typeof f.value === "object")
        filt.setRange(f.value.lb, f.value.ub);
      if (filt instanceof SelectFilter && typeof f.value !== "object")
        filt.setValue(f.value);
    }
  }
  for (let b of (<any>(
    document.getElementsByClassName("colvis")
  )) as HTMLInputElement[])
    colvisUpdate(b);
  update();
  (<any>window).toggleLog = toggleLog;
}
