"use strict";

import Vue from "vue";
import $ from "jquery";
import Datatables from "datatables.net";
import Highcharts from "highcharts";
import Highcharts_more from "highcharts/highcharts-more";
import Highcharts_heatmap from "highcharts/modules/heatmap";
import Highcharts_export_data from "highcharts/modules/export-data";
import {
  assert,
  Dict,
  Field,
  Catalog,
  AggrStats,
  AggrTerms,
  Aggr,
  CatalogResponse,
  toggle_log,
  axis_title,
  render_funct,
  histogram_options,
  updateMathJax,
} from "./common";

Datatables(window, $);
Highcharts_more(Highcharts);
Highcharts_heatmap(Highcharts);
Highcharts_export_data(Highcharts);

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
var Heatmap: undefined | NumericFilter;
var Histcond: boolean = false;
var Histogram_chart: Highcharts.Chart | undefined;
var Last_fields: string[] = [];
var Last_query: undefined | Dict<string>;
var Show_data: boolean = true;

const downloadVue = new Vue({
  data: {
    query: "",
  },
});

function set_download(query: Dict<string>) {
  downloadVue.query = "?" + $.param(query);
  py_text(query);
}

const plotVue = new Vue({
  data: {
    type: "x",
    xfilter: Histogram,
    yfilter: Heatmap,
    log: false,
  },
  methods: {
    // Count log toggle (aka replot)
    toggle_log: function () {
      if (Histogram_chart) toggle_log(Histogram_chart, this.log);
    },
    // Reload
    go: function () {
      histogramShow(<any>this.type);
    },
    reset: function () {
      let selx = <HTMLSelectElement>document.getElementById("histsel-x");
      if (selx) {
        selx.selectedIndex = 0;
      }
      let sely = <HTMLSelectElement>document.getElementById("histsel-y");
      if (sely) {
        sely.selectedIndex = 0;
      }
      histogramRemove();
    },
    tooltip: function () {
      console.log("hover");
    },
  },
});

function histogramRemove() {
  if (Histogram_chart) Histogram_chart.destroy();
  Histogram_chart = undefined;
  Histogram = undefined;
  Heatmap = undefined;
  $("#histlabel").remove();
  $("#hist").empty();
}

function zoomRange(
  f: NumericFilter,
  wid: number,
  axis: Highcharts.AxisOptions
) {
  /* FIXME histLog may have been updated since plot? */
  const unlog = f.histLog ? Math.exp : (x: number) => x;
  f.setRange(
    unlog(wid ? wid * Math.floor(<number>axis.min / wid) : <number>axis.min),
    unlog(wid ? wid * Math.ceil(<number>axis.max / wid) : <number>axis.max)
  );
  f.change();
}

function histogramDraw(
  hist: NumericFilter,
  heatmap: undefined | NumericFilter,
  cond: boolean,
  agg: AggrTerms<number>,
  size: Dict<number>
) {
  const histLabel = document.getElementById("histlabel");
  const field = hist.field;
  const data: number[][] = [];
  let xwid = size[field.name] / 2;
  let ywid = heatmap ? size[heatmap.name] / 2 : NaN;
  let cmax = 0;
  const key = (x: { key: any }) => parseFloat(x.key);
  for (let x of agg.buckets) {
    if (heatmap) {
      if (cond && x.pct) {
        if (x.doc_count)
          data.push([
            key(x) + xwid,
            x.pct.values["0.0"],
            x.pct.values["25.0"],
            x.pct.values["50.0"],
            x.pct.values["75.0"],
            x.pct.values["100.0"],
            x.doc_count,
          ]);
        if (x.doc_count > cmax) cmax = x.doc_count;
      } else if (x.hist)
        for (let y of x.hist.buckets)
          if (y.doc_count)
            data.push([key(x) + xwid, key(y) + ywid, y.doc_count]);
    } else if (x.doc_count) data.push([key(x), x.doc_count]);
  }
  if (data.length <= 1) {
    histogramRemove();
    $("#hist").text("No data for histogram");
    return;
  }
  if (!histLabel) {
    $("#hist").before(
      `<p id="histlabel">Click and drag to zoom into the figure. Filters in the right column reflect plot selections.</p>`
    );
  }

  const opts: Highcharts.Options = histogram_options(
    field,
    hist.histLog,
    plotVue.log
  );
  const renderx = render_funct(hist.field, hist.histLog);
  if (heatmap) {
    opts.colorAxis = <Highcharts.ColorAxisOptions>opts.yAxis;
    opts.colorAxis.reversed = false;
    opts.yAxis = histogram_options(
      heatmap.field,
      heatmap.histLog,
      plotVue.log
    ).xAxis;
  }
  if (heatmap && !cond) {
    (<Highcharts.ChartOptions>opts.chart).zoomType = "xy";
    (<Highcharts.ChartOptions>opts.chart).events = {
      selection: function (event: Highcharts.ChartSelectionContextObject) {
        event.preventDefault();
        zoomRange(hist, xwid, event.xAxis[0]);
        const i = Update_aggs;
        zoomRange(heatmap, ywid, event.yAxis[0]);
        /* make sure both filters apply, in case y happens to be above x */
        if (Update_aggs < i) Update_aggs = i;
        return false; // Don't zoom
      },
    };
    const rendery = render_funct(heatmap.field, heatmap.histLog);
    (<Highcharts.TooltipOptions>opts.tooltip).formatter = function (
      this: Highcharts.TooltipFormatterContextObject
    ): string {
      const p = this.point;
      return (
        (xwid
          ? "[" + renderx(p.x - xwid) + "," + renderx(p.x + xwid) + ")"
          : renderx(p.x)) +
        " & " +
        (ywid
          ? "[" +
            rendery(<number>p.y - ywid) +
            "," +
            rendery(<number>p.y + ywid) +
            ")"
          : rendery(p.y)) +
        ": " +
        (<any>p).value
      );
    };
    (<Highcharts.ColorAxisOptions>opts.colorAxis).minColor = "#ffffff";
    (<Highcharts.LegendOptions>opts.legend).enabled = true;
    opts.series = [
      <Highcharts.SeriesHeatmapOptions>{
        type: "heatmap",
        data: data,
        colsize: 2 * xwid,
        rowsize: 2 * ywid,
      },
    ];
  } else {
    const wid = 2 * xwid;
    (<Highcharts.ChartOptions>opts.chart).events = {
      selection: function (event: Highcharts.ChartSelectionContextObject) {
        event.preventDefault();
        zoomRange(hist, wid, event.xAxis[0]);
        return false; // Don't zoom
      },
    };
    (<Highcharts.TooltipOptions>opts.tooltip).footerFormat = "drag to filter";
    (<Highcharts.AxisOptions>opts.xAxis).min = hist.histLog
      ? Math.log(hist.lbv)
      : hist.lbv;
    (<Highcharts.AxisOptions>opts.xAxis).max = hist.histLog
      ? Math.log(hist.ubv + wid)
      : hist.ubv + wid;
    if (heatmap) {
      /* condmedian */
      (<Highcharts.TooltipOptions>opts.tooltip).formatter = function (
        this: Highcharts.TooltipFormatterContextObject
      ): string {
        return (
          (xwid
            ? "[" + renderx(this.x - xwid) + "," + renderx(this.x + xwid) + ")"
            : renderx(this.x)) +
          ": " +
          this.y
        );
      };
      (<Highcharts.ColorAxisOptions>opts.colorAxis).minColor = "#bbbbbb";
      (<Highcharts.ColorAxisOptions>opts.colorAxis).max = cmax;
      opts.series = [
        <Highcharts.SeriesBoxplotOptions>{
          type: "boxplot",
          data: data,
          keys: ["x", "low", "q1", "median", "q3", "high", "c"],
          colorKey: "c",
        },
        <Highcharts.SeriesLineOptions>{
          type: "line",
          data: data,
          keys: ["x", "min", "q1", "y", "q3", "max", "c"],
          colorKey: "c",
        },
      ];
    } else {
      /* histogram */
      opts.series = [
        <Highcharts.SeriesColumnOptions>{
          showInLegend: true,
          type: "column",
          data: data,
          pointInterval: wid,
          pointRange: wid,
          color: "#0008",
        },
      ];
    }
  }
  Histogram_chart = Highcharts.chart("hist", opts);
  plotVue.$forceUpdate();
}

function histogramShow(axis: "x" | "y" | "c") {
  const selx = <HTMLSelectElement>document.getElementById("histsel-x");
  const filt = addFilter(selx.value);
  if (filt instanceof NumericFilter) {
    Histogram = filt;
    Heatmap = undefined;
    if (axis != "x") {
      const sely = <HTMLSelectElement>document.getElementById("histsel-y");
      const heat = addFilter(sely.value);
      if (heat instanceof NumericFilter) {
        Heatmap = heat;
        if ((Histcond = axis == "c")) Heatmap.histLog = false;
      }
    }
  } else histogramRemove();
  plotVue.xfilter = Histogram;
  plotVue.yfilter = Heatmap;
  update(false);
}

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

function visibleFields(): string[] {
  if (Show_data) {
    return TCat.columns(":visible").dataSrc().toArray();
  } else {
    const cols = TCat.columns();
    const cvis = (<any>cols.visible()).toArray();
    return cols
      .dataSrc()
      .toArray()
      .filter((n, i) => cvis[i]);
  }
}

function ajax(data: any, callback: (data: any) => void, opts: any) {
  const query: Dict<string> = {
    sort: data.order
      .map((o: any) => {
        return (o.dir == "asc" ? "" : "-") + data.columns[o.column].data;
      })
      .join(" "),
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
  query.limit = Show_data ? data.length : 0;
  Last_fields = visibleFields();
  query.fields = Show_data ? Last_fields.join(" ") : "";
  if (aggs) query.aggs = aggs.map((filt) => filt.name).join(" ");
  const histogram = Histogram;
  const heatmap = Histogram && Heatmap;
  const histcond = Histcond;
  if (histogram) {
    if (heatmap) {
      if (histcond) query.hist = histogram.histQuery(64) + " " + heatmap.name;
      else query.hist = histogram.histQuery(16) + " " + heatmap.histQuery(16);
    } else query.hist = histogram.histQuery(128);
  }
  $.ajax({
    method: "GET",
    url: "/" + Catalog.name + "/catalog",
    data: query,
  }).then(
    (res: CatalogResponse) => {
      const modal = <HTMLSelectElement>(
        document.getElementById("progress-modal")
      );
      modal.classList.add("hidden");
      Update = false;
      $("#error").hide();
      Catalog.count = Math.max(Catalog.count || 0, res.hits.total);
      const settings = (<any>TCat.settings())[0];
      settings.oLanguage.sInfo =
        "Showing _START_ to _END_ of " +
        settings.fnFormatNumber(res.hits.total);
      $("#info").text(
        settings.fnFormatNumber(res.hits.total) +
          " Results (Filtered from " +
          settings.fnFormatNumber(Catalog.count) +
          ")"
      );
      callback({
        draw: data.draw,
        recordsTotal: Catalog.count,
        recordsFiltered: Math.min(res.hits.total, DisplayLimit),
        data: res.hits.hits,
      });
      for (let filt of aggs)
        filt.update_aggs((res.aggregations as Dict<Aggr>)[filt.name]);
      Update_aggs = Filters.length;
      if (histogram) {
        if (res.aggregations && res.aggregations.hist)
          histogramDraw(
            histogram,
            heatmap,
            histcond,
            res.aggregations.hist as AggrTerms<number>,
            res.histsize || {}
          );
        else $("#hist").text("No data for histogram");
      }
      if (!Show_data) query.fields = Last_fields.join(" ");
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
      $("#error")
        .text(msg + ": " + err)
        .show();
      callback({
        draw: data.draw,
        data: [],
        error: msg + ": " + err,
      });
    }
  );
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
  aggs: Aggr | {} = {};
  render: (val: any) => string;

  constructor(public field: Field) {
    this.tcol = TCat.column(this.name + ":name");
    this.render = render_funct(this.field);
    this.add();
  }

  get name(): string {
    return this.field.name;
  }

  get addopt(): HTMLOptionElement {
    return <HTMLOptionElement>(
      document.getElementById("addfilt-" + this.field.name)
    );
  }

  protected add() {
    this.addopt.disabled = true;
    Filters.push(this);
  }

  update_aggs(aggs: Aggr): void {
    this.aggs = aggs;
  }

  protected change(search: any, vis: boolean) {
    const i = Filters.indexOf(this);
    /* we want this filter to take effect, so only update later ones */
    if (i >= 0) Update_aggs = i + 1;
    columnVisible(this.name, vis);
    update();
  }

  remove() {
    if (!TCat) return;
    const i = Filters.indexOf(this);
    if (i < 0) return;
    Filters.splice(i, 1);
    Update_aggs = i;
    columnVisible(this.name, true);
    this.addopt.disabled = false;
    update();
  }

  abstract query(): string | undefined;
  abstract pyQuery(): string | undefined;
}

class SelectFilter extends Filter {
  value: string = "";

  update_aggs(aggs: AggrTerms<string>) {
    super.update_aggs(aggs);
  }

  change() {
    const val = this.value;
    super.change(val, !val);
  }

  setValue(val: string) {
    this.value = val;
  }

  query(): string | undefined {
    const val = this.value;
    if (val) return val;
  }

  pyQuery(): string | undefined {
    const val = this.value;
    if (val) return JSON.stringify(val);
  }
}

class NumericFilter extends Filter {
  lbv: number;
  ubv: number;
  histLog: boolean = false;

  update_aggs(aggs: AggrStats) {
    super.update_aggs(aggs);
    this.lbv = aggs.min;
    this.ubv = aggs.max;
  }

  change() {
    super.change(this.lbv + " TO " + this.ubv, this.lbv != this.ubv);
  }

  query(): string {
    const lbv = isFinite(this.lbv) ? this.lbv.toString() : "";
    const ubv = isFinite(this.ubv) ? this.ubv.toString() : "";
    if (lbv == ubv) return lbv;
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

  reset() {
    this.setRange((<AggrStats>this.aggs).min, (<AggrStats>this.aggs).max);
    this.change();
  }

  remove() {
    this.histogramRemove();
    super.remove();
  }

  setRange(lbv: number, ubv: number) {
    this.lbv = lbv;
    this.ubv = ubv;
    if (!(this.lbv > 0)) this.histLog = false;
    /* vue isn't updating when called from highcharts: */
    filterVue.$forceUpdate();
    filterTab.$forceUpdate();
  }

  histQuery(n: number): string {
    return this.field.name + (this.histLog ? ":log" : ":") + n.toString();
  }
}

const filterVue = new Vue({
  data: {
    filters: Filters,
  },
});

const filterTab = new Vue({
  data: { filters: Filters },
});

function addFilter(fi: string | Field): Filter | undefined {
  const field = typeof fi === "string" ? Catalog.fields[Fields_idx[fi]] : fi;
  if (!TCat || !field) return;
  let filt = Filters.find((f) => f.field.name === field.name);
  if (filt) return filt;
  if (field.terms) return new SelectFilter(field);
  return new NumericFilter(field);
}

(<any>window).addFilter = function (fn: string) {
  if (addFilter(fn)) update(false);
};

function colvisNames(box: HTMLInputElement): string[] {
  const l: string[] = [];
  for (let k of (<any>box.classList) as string[]) {
    if (k.startsWith("colvis-")) l.push(k.substr(7));
  }
  return l;
}

function colvisUpdate(box: HTMLInputElement, vis?: boolean) {
  const v: boolean[] = ((<any>(
    TCat.columns(colvisNames(box).map((n) => n + ":name")).visible()
  )) as JQuery<boolean>).toArray();
  if (vis == null) vis = v.shift() || false;
  box.checked = vis;
  box.indeterminate = v.some((x) => x !== vis);
}

function columnVisible(name: string, vis: boolean) {
  TCat.column(name + ":name").visible(vis);
  for (let b of (<any>(
    document.getElementsByClassName("colvis-" + name)
  )) as Element[])
    colvisUpdate(<HTMLInputElement>b, vis);
  if (Show_data && vis && Last_fields.indexOf(name) < 0) {
    update(false);
    updateMathJax();
  }
  for (let opt of (<any>(
    document.getElementsByClassName("sel-" + name)
  )) as HTMLOptionElement[]) {
    opt.style.display = vis ? "" : "none";
  }
}

(<any>window).colvisSet = function colvisSet(event: Event) {
  const box = <HTMLInputElement>event.target;
  if (!box.indeterminate)
    for (let n of colvisNames(box)) columnVisible(n, box.checked);
  if (Last_query) {
    Last_query.fields = visibleFields().join(" ");
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
  const url = new URL(location.href);
  for (let v in query) {
    url.searchParams.set(v, query[v]);
  }
  history.replaceState({}, "", url.href);
}

function toggleShowData(show?: boolean) {
  Show_data = show === undefined ? !Show_data : show;
  $("#rawdata-btn").text(Show_data ? "Hide Raw Data" : "View Raw Data");
  $("#rawdata").toggle(Show_data);
  if (Show_data) update(true);
}
(<any>window).toggleShowData = toggleShowData;

export function initCatalog(table: JQuery<HTMLTableElement>) {
  downloadVue.$mount("#download");
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
      zeroRecords: "",
    },
    dom: 'i<"#download">rtlp',
    deferRender: true,
    pagingType: "simple",
  };
  if (Catalog.sort) topts.order = Catalog.sort.map((o) => [o, "asc"]);
  if ((<any>window).Query) {
    if (Query.sample != null) Sample = Query.sample;
    if (Query.seed != null) Seed = Query.seed;
    if (Query.offset) topts.displayStart = Query.offset;
    if (Query.limit) topts.pageLength = Query.limit;
    if (Query.sort)
      topts.order = Query.sort.map((o) => {
        return [Fields_idx[o.field], o.asc ? "asc" : "desc"];
      });
    if (Query.fields && Query.fields.length) {
      for (let f of Catalog.fields) {
        f.disp = Query.fields.indexOf(<string>f.name) >= 0;
      }
    }
  }
  topts.columns = Catalog.fields.map((c) => {
    return {
      name: c.name,
      className:
        c.base === "f" || c.base === "i" ? "dt-body-right" : "dt-body-left",
      visible: c.disp,
      render: render_funct(c),
    };
  });
  TCat = table.DataTable(topts);

  for (let f of Catalog.fields) {
    if (f.flag !== undefined) addFilter(f);
  }
  if ((<any>window).Query && Query.filter) {
    for (let f of Query.filter) {
      const filt = addFilter(f.field);
      if (filt instanceof NumericFilter && typeof f.value === "object")
        filt.setRange(f.value.lb, f.value.ub);
      if (filt instanceof SelectFilter && typeof f.value !== "object")
        filt.setValue(f.value);
    }
  }
  filterVue.$mount("#filt");
  filterTab.$mount("#filt-tab");
  for (let b of (<any>(
    document.getElementsByClassName("colvis")
  )) as HTMLInputElement[])
    colvisUpdate(b);
  plotVue.$mount("#plot");
  toggleShowData(false);
  update();
}
