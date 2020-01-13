"use strict";
import Highcharts from "highcharts";
import Highcharts_exporting from "highcharts/modules/exporting";

Highcharts_exporting(Highcharts);

export function assert<T>(x: undefined | null | false | T): T {
  if (!x) throw new Error("assertion failure");
  return x;
}

export type Dict<T> = { [name: string]: T };

export type Field = {
  name: string;
  type: string;
  title: string;
  descr?: null | string;
  units?: null | string;
  flag?: boolean;
  disp: boolean;
  base: "f" | "i" | "b" | "s";
  terms?: boolean;
  enum?: null | string[];
  dict?: null | string;
  scale?: number;
};

export type Catalog = {
  name: string;
  title: string;
  descr: null | string;
  bulk: string[];
  fields: Field[];
  count?: number;
  sort?: string[];
};

export type AggrStats = {
  min: number;
  max: number;
  avg: number;
};
export type AggrTerms<T> = {
  buckets: Array<{
    key: T;
    from?: T;
    to?: T;
    doc_count: number;
    hist?: AggrTerms<any>;
    pct?: { values: Dict<number> };
  }>;
};

export type Aggr = AggrStats | AggrTerms<string | number>;

export type CatalogResponse = {
  hits: {
    total: number;
    hits: Dict<any>[];
  };
  aggregations?: Dict<Aggr>;
  histsize: Dict<number>;
};

export function fill_select_terms(
  s: HTMLSelectElement,
  f: Field,
  a: AggrTerms<string>
) {
  if (!f.flag /* required */) {
    const def = document.createElement("option");
    def.text = "all";
    s.add(def);
  }
  a.buckets.sort((c, d) => -(c.key < d.key) || +(c.key > d.key));
  for (let b of a.buckets) {
    const opt = document.createElement("option");
    opt.value = b.key;
    if (f.enum && b.key in f.enum) opt.text = f.enum[<any>b.key];
    else opt.text = b.key;
    opt.text += " (" + b.doc_count + ")";
    s.add(opt);
  }
  s.disabled = false;
}

export function field_option(f: Field): HTMLOptionElement {
  const o = document.createElement("option");
  o.value = f.name;
  o.text = f.title;
  if (f.descr) o.title = f.descr;
  return o;
}

export function updateMathJax() {
  let MathJax = (<any>window).MathJax;

  if (MathJax)
    setTimeout(() => {
      if (!MathJax.Hub.queue.pending)
        MathJax.Hub.Queue(["Typeset", MathJax.Hub]);
    });
}

export function field_title(
  field: Field,
  rmf?: (ev: MouseEvent) => any
): HTMLSpanElement {
  const h = document.createElement("span");
  h.textContent = field.title;
  if (rmf) {
    const rm = document.createElement("button");
    rm.className = "button-remove";
    rm.innerHTML = "&times;";
    rm.onclick = rmf;
    h.insertBefore(rm, h.firstChild);
  }
  if (field.units) {
    const units = document.createElement("span");
    units.className = "units";
    units.appendChild(document.createTextNode(field.units));
    h.appendChild(units);
  }
  if (field.descr) {
    h.className = "tooltip-dt";
    const tt = document.createElement("span");
    tt.className = "tooltiptext";
    tt.innerHTML = field.descr.toUpperCase();
    h.appendChild(tt);
  }

  updateMathJax();

  return h;
}

export function render_funct(field: Field, prec: number = 8): (data: any) => string {
  if (field.base === "f")
    return data => (data != undefined ? parseFloat(data).toPrecision(prec) : data);
  if (field.enum) {
    const e: string[] = field.enum;
    return data => (data in e ? e[data] : data);
  }
  return data => data;
}

export function toggle_log(chart: Highcharts.Chart) {
  const axis = <Highcharts.Axis>chart.get("tog");
  if ((<any>axis).userOptions.type !== "linear") {
    axis.update({
      min: 0,
      type: "linear"
    });
  } else {
    axis.update({
      min: null,
      type: "logarithmic"
    });
  }
}

export function axis_title(f: Field) {
  return {
    // useHTML: true, // not enough for mathjax
    text: f.title + (f.units ? " [" + f.units + "]" : "")
  };
}

export function histogram_options(f: Field, log: boolean = false): Highcharts.Options {
  const render = render_funct(f);
  const renders = render_funct(f, 3);
  return {
    chart: {
      animation: false,
      zoomType: "x"
    },
    legend: {
      enabled: false
    },
    title: {
      text: undefined
    },
    credits: {
      enabled: false
    },
    xAxis: {
      type: log ? "logarithmic" : "linear",
      title: axis_title(f),
      gridLineWidth: 1,
      labels: {
        formatter: function() {
          return renders(this.value);
        }
      }
    },
    yAxis: {
      id: "tog",
      type: "linear",
      title: { text: "Count" },
      allowDecimals: false,
      min: 0
    },
    tooltip: {
      animation: false,
      formatter: function(this: Highcharts.TooltipFormatterContextObject): string {
        const wid = <number>(<Highcharts.SeriesColumnOptions>this.series.options).pointInterval;
        return (
          (wid ?
            "[" +
            render(this.x) +
            "," +
            render(this.x + wid) +
          ")"
          : render(this.x))
          + ": " +
          this.y
        );
      }
    },
    exporting: {
      enabled: true
    },
    plotOptions: {
      column: {
        grouping: false,
        groupPadding: 0,
        pointPadding: 0,
        borderWidth: 0,
        shadow: false,
        pointPlacement: 0.5,
        animation: { duration: 0 },
        states: {
          hover: {
            enabled: false
          }
        }
      }
    }
  };
}

(<any>window).toggleDisplay = function toggleDisplay(ele: string) {
  $("#" + ele).toggle();
};
