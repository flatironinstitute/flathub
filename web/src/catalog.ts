import Vue from "vue";
import Highcharts from "highcharts";
import Highcharts_3d from "highcharts/highcharts-3d";
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
  LogScale,
  toggle_log,
  axis_title,
  render_funct,
  tooltip_formatter,
  axis_options,
  histogram_options,
  updateMathJax,
  progressVue,
} from "./common";

Highcharts_3d(Highcharts);
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
const ScatterCount = 2000;
const Scatter3DCount = 1000;
var Sample: number = 1;
var Seed: undefined | number;
var Update_aggs: number = -1;
var PlotX: undefined | NumericFilter;
var PlotY: undefined | NumericFilter;
var PlotCond: undefined | NumericFilter; /* mutually exclusive with PlotY */
var PlotZ: undefined | NumericFilter;
var PlotC: undefined | Field; /* color for Scatterplot */
var Scatterplot: boolean = false;
var Chart: Highcharts.Chart | undefined;
var Last_fields: string[] = [];
var Last_query: undefined | Dict<string>;
var Show_data: boolean = true;

const downloadVue = new Vue({
  data: {
    bulk: "",
    query: "",
  },
  computed: {
    link: function () {
      return this.bulk + this.query;
    },
  },
});

function set_download(query: Dict<string>) {
  downloadVue.query = "?" + $.param(query);
  py_text(query);
}

const plotVue = new Vue({
  data: {
    type: "x",
    filter: {
      x: PlotX,
      y: PlotY || PlotCond,
      z: PlotZ
    },
    log: false, /* for histogram */
    color: PlotC,
    colorlog: false, /* for scatter */
  },
  methods: {
    // Count log toggle (aka replot)
    toggle_log: function () {
      if (Chart) toggle_log(Chart, this.log);
    },
    // Reload
    go: function (ev: any) {
      if (Chart && ev.target && ev.target.name.startsWith('log')) {
        const a: 'x'|'y'|'z' = ev.target.name.slice(3);
        if (a in this.filter && this.type == 's' || (this.type == 'c' && a == 'y')) {
          const log = this.filter[a].plotLog;
          (<Array<Highcharts.Axis>>(<any>Chart)[a+'Axis'])[0].update({
            type: log ? "logarithmic" : "linear"
          });
          return;
        }
      }
      plotShow();
    },
    tooltip: function () {
    },
  },
});

function zoomRange(
  f: NumericFilter,
  wid: number,
  axis: Highcharts.AxisOptions,
  scatter: boolean = false
) {
  const unlog = f.plotLogScale == 'server' ? Math.exp : (x: number) => x;
  f.setRange(
    unlog(wid ? wid * Math.floor(<number>axis.min / wid) : <number>axis.min),
    unlog(wid ? wid * Math.ceil(<number>axis.max / wid) : <number>axis.max)
  );
  f.change();
}

function zoomEvent(xwid: number = 0, ywid: number = 0): (event: Highcharts.ChartSelectionContextObject) => boolean {
  const plotx = PlotX;
  const ploty = PlotY;
  const scatter = Scatterplot;
  return function (event: Highcharts.ChartSelectionContextObject) {
    event.preventDefault();
    zoomRange(plotx, xwid, event.xAxis[0], scatter);
    if (ploty) {
      const i = Update_aggs;
      zoomRange(ploty, ywid, event.yAxis[0], scatter);
      /* make sure both filters apply, in case y happens to be above x */
      if (Update_aggs < i) Update_aggs = i;
    }
    return false; // Don't zoom
  };
}

function plotDraw(
  agg: AggrTerms<number>,
  size: Dict<number>
) {
  const plotLabel = document.getElementById("plotlabel");
  const data: number[][] = [];
  let xwid = size[PlotX.name] / 2;
  let ywid = PlotY ? size[PlotY.name] / 2 : NaN;
  let cmax = 0;
  const key = (x: { key: any }) => parseFloat(x.key);
  for (let x of agg.buckets) {
    if (PlotCond) {
      if (x.doc_count && x.pct) {
        data.push([
          key(x) + xwid,
          x.pct.values["0.0"],
          x.pct.values["25.0"],
          x.pct.values["50.0"],
          x.pct.values["75.0"],
          x.pct.values["100.0"],
          x.doc_count,
        ]);
        if (x.doc_count > cmax)
          cmax = x.doc_count;
      }
    } else if (PlotY && x.hist) {
      for (let y of x.hist.buckets)
        if (y.doc_count) {
          data.push([key(x) + xwid, key(y) + ywid, y.doc_count]);
          if (y.doc_count > cmax)
            cmax = y.doc_count;
        }
    } else if (x.doc_count) {
      data.push([key(x), x.doc_count]);
      if (x.doc_count > cmax)
        cmax = x.doc_count;
    }
  }
  if (data.length <= 1) {
    $("#plot-chart").text("No data for plot");
    return;
  }
  if (!plotLabel) {
    $("#plot-chart").before(
      `<p id="plotlabel">Click and drag to zoom into the figure. Filters in the right column reflect plot selections.</p>`
    );
  }

  const opts: Highcharts.Options = histogram_options(PlotX.field, PlotX.plotLogScale, plotVue.log);
  opts.xAxis = <Highcharts.XAxisOptions>PlotX.axisOptions();
  const renderx = PlotX.render;
  if (PlotY) {
    opts.colorAxis = <Highcharts.ColorAxisOptions>opts.yAxis;
    opts.colorAxis.reversed = false;
    opts.colorAxis.max = cmax;
    opts.yAxis = PlotY.axisOptions();
    (<Highcharts.ChartOptions>opts.chart).zoomType = "xy";
    (<Highcharts.ChartOptions>opts.chart).events = {
      selection: zoomEvent(xwid, ywid)
    };
    const rendery = PlotY.render;
    opts.tooltip.formatter = tooltip_formatter([
        {key: 'x', render: renderx, left: xwid, right: xwid},
        {key: 'y', render: rendery, left: ywid, right: ywid}
      ], {key: 'value'});
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
      selection: zoomEvent(wid)
    };
    (<Highcharts.TooltipOptions>opts.tooltip).footerFormat = "drag to filter";
    if (PlotCond) {
      /* condmedian */
      opts.colorAxis = <Highcharts.ColorAxisOptions>opts.yAxis;
      opts.colorAxis.reversed = false;
      opts.yAxis = PlotCond.axisOptions();
      opts.tooltip.formatter = tooltip_formatter([
          {key: 'x', render: renderx, left: xwid, right: xwid}
        ], {key: 'y'});
      opts.colorAxis.minColor = "#bbbbbb";
      opts.colorAxis.max = cmax;
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
      opts.tooltip.formatter = tooltip_formatter([
          {key: 'x', render: renderx, right: wid}
        ], {key: 'y'});
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
  Chart = Highcharts.chart("plot-chart", opts);
  plotVue.$forceUpdate();
}

function chartDrag3D(ev: MouseEvent|TouchEvent|PointerEvent) {
    const eStart = Chart.pointer.normalize(ev);

    const alpha = Chart.options.chart.options3d.alpha,
	beta = Chart.options.chart.options3d.beta,
	sensitivity = 4,  // lower is more sensitive
	handlers: Function[] = [];

    function drag(e: MouseEvent|TouchEvent|PointerEvent) {
	// Get e.chartX and e.chartY
	const eEnd = Chart.pointer.normalize(e);

	Chart.update({
	    chart: {
		options3d: {
		    alpha: alpha + (eEnd.chartY - eStart.chartY) / sensitivity,
		    beta: beta + (eStart.chartX - eEnd.chartX) / sensitivity
		}
	    }
	}, undefined, undefined, false);
    }

    function unbindAll() {
	handlers.forEach(function (unbind) {
	    if (unbind) {
		unbind();
	    }
	});
	handlers.length = 0;
    }

    handlers.push(Highcharts.addEvent(document, 'mousemove', drag));
    handlers.push(Highcharts.addEvent(document, 'touchmove', drag));
    handlers.push(Highcharts.addEvent(document, 'mouseup', unbindAll));
    handlers.push(Highcharts.addEvent(document, 'touchend', unbindAll));
};

function scatterplotDraw(
  res: Array<Dict<number>>
) {
  const opts: Highcharts.Options = histogram_options(PlotX.field);
  opts.xAxis = <Highcharts.XAxisOptions>PlotX.axisOptions();
  opts.yAxis = <Highcharts.YAxisOptions>PlotY.axisOptions();
  const xname = PlotX.name;
  const yname = PlotY.name;
  let getData = (r: Dict<number>) => [r[xname],r[yname]];
  let stype: 'scatter'|'scatter3d' = 'scatter';
  const sopts = {
    marker: {
      symbol: 'circle',
      radius: 1.5
    },
    turboThreshold: 0,
  };
  const dims = [
    {key: 'x', render: PlotX.render},
    {key: 'y', render: PlotY.render}
  ];
  if (PlotZ) {
    opts.chart.options3d = {
      enabled: true,
      alpha: 10,
      beta: 20,
      depth: 400
    };
    opts.zAxis = <Highcharts.ZAxisOptions>PlotZ.axisOptions();
    const zname = PlotZ.name;
    getData = (r) => [r[xname],r[yname],r[zname]];
    stype = 'scatter3d';
    opts.plotOptions.scatter3d = sopts;
    opts.chart.zoomType = undefined;
    dims.push({key: 'z', render: PlotZ.render});
  } else {
    opts.plotOptions.scatter = sopts;
    opts.chart.zoomType = "xy";
    opts.chart.events = {
      selection: zoomEvent()
    };
  }
  let val;
  if (PlotC) {
    opts.legend.enabled = true;
    opts.legend.title = axis_title(PlotC);
    const cname = PlotC.name;
    const render = render_funct(PlotC);
    if (PlotC.terms) {
      /* categorical plotc */
      opts.series = [];
      const series: { [val: number]: Highcharts.SeriesScatterOptions|Highcharts.SeriesScatter3dOptions } = {};
      for (let r of res) {
        const z = r[cname];
        if (!(z in series)) {
          series[z] = {
            type: stype,
            name: render_funct(PlotC)(z),
            legendIndex: z,
            data: [],
          };
          opts.series.push(series[z]);
        }
        series[z].data.push(getData(r));
      }
    } else {
      /* color scale */
      /* coloraxis extremes calculation is broken, do it manually: */
      let min: number, max: number;
      type Point = {x: number; y: number; z?: number; c:number};
      const data: Point[] = [];
      /* colorAxis.type = logarithmic doesn't seem to work, fake it */
      const log = plotVue.colorlog;
      for (let r of res) {
        let c = r[cname];
        if (log)
          c = c > 0 ? Math.log10(c) : undefined;
        if (min == null || c < min)
          min = c;
        if (max == null || c > max)
          max = c;
        const d = getData(r);
        const p: Point = {x:d[0], y:d[1], c:c};
        if (PlotZ)
          p.z = d[2];
        data.push(p);
      }
      const rf = render_funct(PlotC, log, 2);
      opts.colorAxis = {
        min: min,
        max: max,
        minColor: '#ff0000',
        maxColor: '#0000ff',
        labels: {
          formatter: function () {
            return rf(this.value);
          },
          allowOverlap: true
        },
        reversed: PlotC.reversed || false
      };
      opts.series = [{
        type: stype,
        colorKey: 'c',
        data: data,
        color: '#e7e7e8',
      }];
      val = {key:'c', render: render};
    }
  } else {
    const data: number[][] = [];
    for (let r of res)
      data.push(getData(r));
    opts.series = [{
      type: stype,
      data: data,
    }];
  }
  opts.tooltip.formatter = tooltip_formatter(dims, val);
  Chart = Highcharts.chart("plot-chart", opts);
  (<any>window).Chart = Chart;
  if (PlotZ) {
    Highcharts.addEvent(Chart.container, 'mousedown', chartDrag3D);
    Highcharts.addEvent(Chart.container, 'touchstart', chartDrag3D);
  }
  plotVue.$forceUpdate();
}

function plotShow() {
  if (progressVue.update)
    return;
  PlotX = undefined;
  PlotCond = undefined;
  PlotY = undefined;
  PlotZ = undefined;
  PlotC = undefined;
  const axis = plotVue.type;
  const selx = <HTMLSelectElement>document.getElementById("plot-x");
  const x = selx && addFilter(selx.value);
  if (x instanceof NumericFilter) {
    PlotX = x;
    if (axis !== "x") {
      const sely = <HTMLSelectElement>document.getElementById("plot-y");
      const y = sely && addFilter(sely.value);
      if (y instanceof NumericFilter) {
        if (axis === 'c')
          PlotCond = y;
        else {
          PlotY = y;
          if (axis == "s") {
            const selz = <HTMLSelectElement>document.getElementById("plot-z");
            const z = selz && addFilter(selz.value);
            if (z instanceof NumericFilter)
              PlotZ = z;
            const selc = <HTMLSelectElement>document.getElementById("plot-c");
            PlotC = selc && Catalog.fields[Fields_idx[selc.value]];
          }
        }
      }
    }
  }
  Scatterplot = !!(axis === 's' && PlotX && PlotY);
  const old = plotVue.filter;
  plotVue.filter = {
    x: PlotX,
    y: PlotY || PlotCond,
    z: PlotZ,
  };
  plotVue.color = PlotC;
  for (let o of Object.values(old))
    if (o && !Object.values(plotVue.filter).includes(o))
      o.removeIfClear();
  if (PlotX)
    update(false);
  else {
    if (Chart) Chart.destroy();
    Chart = undefined;
    $("#plotlabel").remove();
    $("#plot-chart").empty();
  }
}

/* elasticsearch max_result_window */
const DisplayLimit = 10000;
var Update_paging: boolean = false;

function update(paging: boolean = true) {
  if (progressVue.update) return;
  if (paging) Update_paging = paging;
  progressVue.update = setTimeout(() => {
    TCat.draw(Update_paging);
    Update_paging = false;
  });
}

function visibleFields(): string[] {
  if (Show_data) {
    return TCat.columns(":visible")
      .dataSrc()
      .toArray()
      .filter((n) => n !== "_id");
  } else {
    const cols = TCat.columns();
    const cvis = (<any>cols.visible()).toArray();
    return cols
      .dataSrc()
      .toArray()
      .filter((n, i) => n !== "_id" && cvis[i]);
  }
}

function querySample(sample: number, seed: number|undefined): string {
  let s = <any>sample;
  if (seed != undefined) s += "@" + seed;
  return s;
}

function ajax(data: any, callback: (data: any) => void, opts: any) {
  const query: Dict<string> = {
    sort: data.order
      .map((o: any) => {
        return (o.dir == "asc" ? "" : "-") + data.columns[o.column].data;
      })
      .join(" "),
  };

  let aggs = Filters;
  if (Update_aggs >= 0) aggs = aggs.slice(Update_aggs);
  else Update_aggs = Filters.length;
  for (let fi = 0; fi < Update_aggs; fi++) {
    const filt = Filters[fi];
    const q = filt.query();
    if (q != null) query[filt.name] = q;
  }
  let sample = Sample;
  const seed = Seed;
  if (sample < 1)
    query.sample = querySample(sample, seed);

  Last_fields = visibleFields();
  if (Show_data) {
    query.offset = data.start;
    query.limit = data.length;
    query.fields = Last_fields.join(" ");
  } else {
    query.limit = <any>0;
    query.fields = "";
  }
  if (aggs) query.aggs = aggs.map((filt) => filt.name).join(" ");
  if (PlotX && !Scatterplot) {
    if (PlotCond)   query.hist = PlotX.histQuery(64) + " " + PlotCond.name;
    else if (PlotY) query.hist = PlotX.histQuery(64) + " " + PlotY.histQuery(16);
    else            query.hist = PlotX.histQuery(128);
  }
  const xhr = $.ajax({
    method: "GET",
    url: "/" + Catalog.name + "/catalog",
    data: query,
  });
  progressVue.update = xhr;
  xhr.then(
    (res: CatalogResponse) => {
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
      if (!Show_data) query.fields = Last_fields.join(" ");
      delete query.aggs;
      delete query.hist;
      url_update(query);
      delete query.limit;
      delete query.offset;
      set_download((Last_query = query));
      if (!filterVue.$el)
        filterVue.$mount("#filt");

      /* do another query for scatter plot */
      if (Scatterplot) {
        delete query.sort;
        const count = PlotZ ? Scatter3DCount : ScatterCount;
        if (res.hits.total > count)
          sample *= count/res.hits.total;
        if (sample < 1)
          query.sample = querySample(sample, seed);
        query.limit = <any>count;
        query.fields = PlotX.name + " " + PlotY.name;
        if (PlotZ)
          query.fields += " " + PlotZ.name;
        if (PlotC)
          query.fields += " " + PlotC.name;
        const xhr = $.ajax({
          method: "GET",
          url: "/" + Catalog.name + "/catalog",
          data: query,
        });
        progressVue.update = xhr;
        xhr.then(
          (res: CatalogResponse) => {
            scatterplotDraw(res.hits.hits);
            progressVue.update = undefined;
          },
          (xhr, msg, err) => {
            $("#error")
              .text(msg + ": " + err)
              .show();
            progressVue.update = undefined;
          }
        );
      } else {
        if (PlotX) {
          if (res.aggregations && res.aggregations.hist)
            plotDraw(
              res.aggregations.hist as AggrTerms<number>,
              res.histsize || {}
            );
          else $("#plot-chart").text("No data for histogram");
        }
        progressVue.update = undefined;
      }
    },
    (xhr, msg, err) => {
      callback({
        draw: data.draw,
        data: [],
        error: msg + ": " + err,
      });
      $("#error")
        .text(msg + ": " + err)
        .show();
      progressVue.update = undefined;
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
    this.render = render_funct(this.field, this.plotLogScale == 'server');
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

  get plotLogScale(): LogScale {
    return null;
  }
}

class SelectFilter extends Filter {
  value: string = "";

  update_aggs(aggs: AggrTerms<string>) {
    super.update_aggs(aggs);
  }

  change() {
    const val = this.value;
    super.change(val, this.field.wildcard || (this.field.attachment ? val !== '0' : !val));
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

  reset() {
    this.value = '';
    this.change();
    filterVue.$forceUpdate();
  }
}

class NumericFilter extends Filter {
  lbv: number;
  ubv: number;
  plotLog: boolean = false;

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

  private plotRemove(): boolean {
    let ch = false;
    for (let x of "xyz") {
      const sel = <HTMLSelectElement>document.getElementById("plot-"+x);
      if (sel && sel.value == this.name) {
        ch = true;
        sel.value = '';
      }
    }
    if (ch) {
      plotShow();
      return true;
    }
  }

  reset() {
    this.setRange((<AggrStats>this.aggs).min, (<AggrStats>this.aggs).max);
    this.change();
  }

  remove() {
    this.plotRemove();
    super.remove();
  }

  removeIfClear() {
    if (this.lbv == (<AggrStats>this.aggs).min && this.ubv == (<AggrStats>this.aggs).max)
      this.remove();
  }

  setRange(lbv: number, ubv: number) {
    this.lbv = lbv;
    this.ubv = ubv;
    if (!(this.lbv > 0)) this.plotLog = false;
    /* vue isn't updating when called from highcharts: */
    filterVue.$forceUpdate();
    filterTab.$forceUpdate();
  }

  get plotLogScale(): LogScale {
    return this.plotLog ? Scatterplot || this === PlotCond ? 'client' : 'server' : null;
  }

  histQuery(n: number): string {
    return this.field.name + (this.plotLogScale == 'server' ? ":log" : ":") + n.toString();
  }

  axisOptions(): Highcharts.AxisOptions {
    return axis_options(this.field, this.plotLogScale, this.lbv, this.ubv);
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
  if (field.base === 's') return new SelectFilter(field);
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
    "import flathub.client\n" +
    cat +
    " = flathub.client.Catalog(" +
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
    if (q != null) st += ",\n  " + Filters[i].name + " = " + q;
  }
  if (query.sort) st += ",\n  sort = " + JSON.stringify(query.sort.split(" "));
  if (Sample < 1) {
    st += ",\n  sample = " + Sample;
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

// todo: how do i get these going?
function toggleShowData(show?: boolean) {
  Show_data = show === undefined ? !Show_data : show;
  $("#rawdata-btn").text(Show_data ? "Hide Raw Data" : "View Raw Data");
  $("#rawdata").toggle(Show_data);
  if (Show_data) update(true);
}
(<any>window).toggleShowData = toggleShowData;

function array_render(rf: (x: any) => string, data: any): string {
  if (!data)
    return "";
  if (!Array.isArray(data))
    return rf(data);
  let ext = "";
  if (data.length > 3) {
    /* TODO: make this click to expand */
    ext = ",...[" + data.length + "]";
    data = data.slice(0,3);
  }
  return data.map(rf).toString() + ext;
}

function cell_render(field: Field): (data: any, type: string, row: any) => string {
  if (field.attachment)
    return (data, type, row) => (data ? "<a href='/api/" + Catalog.name + "/attachment/" + field.name + "/" + encodeURIComponent(row._id) + "'><img class='download-icon' src='/web/download.svg'></a>" : "");
  const rf = render_funct(field);
  if (field.type.startsWith("array "))
    return (data, type, row) => array_render(rf, data);
  return rf;
}

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
  }
  topts.columns = [];
  topts.columns.push.apply(topts.columns, Catalog.fields.map((c) => {
    return {
      name: c.name,
      render: cell_render(c),
    };
  }));
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
        filt.setValue(f.value.toString());
    }
  }
  // defer mounting #filt until we get the first data (and aggregates) back
  filterTab.$mount("#filt-tab");
  for (let b of (<any>(
    document.getElementsByClassName("colvis")
  )) as HTMLInputElement[])
    colvisUpdate(b);
  plotVue.$mount("#plot");
  toggleShowData(false);
  update();
}
