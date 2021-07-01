"use strict";

import Vue from "vue";
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
  synopsis?: null | string;
  descr?: null | string;
  units?: null | string;
  flag?: boolean;
  disp: boolean;
  base: "f" | "i" | "b" | "s" | "v";
  terms?: boolean;
  enum?: null | string[];
  dict?: null | string;
  scale?: number;
  reversed?: boolean;
  attachment?: boolean;
  wildcard?: boolean;
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
type Bucket<T> = {
  key: T;
  from?: T;
  to?: T;
  doc_count: number;
  hist?: AggrTerms<any>;
  pct?: { values: Dict<number> };
};
export type AggrTerms<T> = {
  buckets: Array<Bucket<T>>;
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

export type LogScale = 'client'|'server'|null;

type RenderFun = (x: any) => string;

export function closeModal() {
  const modal = <HTMLSelectElement>document.getElementById("browser-modal");
  modal.classList.add("hidden");
}
(<any>window).closeModal = closeModal;

/* deprecated: use select-terms below */
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

Vue.component("select-terms", {
  props: {
    field: Object, // Field
    aggs: Object, // AggrTerms<string>
    value: String,
    change: Function,
  },
  computed: {
    terms: function () {
      return (
        (<AggrTerms<string>>this.aggs).buckets &&
        this.aggs.buckets.sort(
          (c: Bucket<any>, d: Bucket<any>) =>
            -(c.key < d.key) || +(c.key > d.key)
        )
      );
    },
  },
  template: `
    <select v-bind:value="value" v-on:input="$emit('input',$event.target.value)" v-on:change="change">
      <option v-if="!field.flag" value="">all</option>
      <option v-for="b in terms" v-bind:value="b.key">{{field.enum&&b.key in field.enum?field.enum[b.key]:b.key}} ({{b.doc_count}})</option>
    </select>
  `,
});

export function field_option(f: Field): HTMLOptionElement {
  const o = document.createElement("option");
  o.value = f.name;
  o.text = f.title;
  if (f.descr) o.title = f.descr;
  return o;
}

export function updateMathJax() {
  const MathJax = (<any>window).MathJax;

  if (MathJax)
    setTimeout(() => {
      if (!MathJax.Hub.queue.pending)
        MathJax.Hub.Queue(["Typeset", MathJax.Hub]);
    });
}

Vue.component("field-title", {
  props: ["field", "rmf"],
  template: `
  <div class="filter-text">
    <button v-if="rmf" class="button-remove" v-on:click="rmf">&times;</button>
    <span v-bind:class="{ 'tooltip-dt': field.descr }">
      {{field.title}}
      <span v-if="field.descr" class="tooltiptext">{{field.descr}}</span>
    </span>
    <span v-if="field.units" class="filter-text-units">{{field.units}}</span>
  </div>
  `,
});

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

export function render_funct(
  field: Field,
  log: boolean = false
): RenderFun {
  if (field.base === "f") {
    const p = log ? (x: any) => Math.exp(parseFloat(x)) : parseFloat;
    return (data) => (data != undefined ? p(data).toPrecision(8) : data);
  }
  if (field.enum) {
    const e: string[] = field.enum;
    return (data) => (data in e ? e[data] : data);
  }
  return (data) => data;
}

export function toggle_log(chart: Highcharts.Chart, log?: boolean) {
  const axis = <Highcharts.Axis>chart.get("tog");
  if (log === undefined)
    log = (<any>axis).userOptions.type === "linear";
  axis.update({
    min: log ? null : 0,
    type: log ? "logarithmic" : "linear",
  });
}

export function axis_title(f: Field) {
  return {
    // useHTML: true, // not enough for mathjax
    text: f.title + (f.units ? " [" + f.units + "]" : ""),
  };
}

type PointDim = {
  key: string,
  render?: RenderFun,
  left?: number,
  right?: number
};

function point_formatter(d: PointDim): (x: Highcharts.Point) => string {
  const r = d.render || ((x: any) => x);
  if (d.left || d.right) {
    const left = d.left || 0;
    const right = d.right || 0;
    return (p: any) => {
      const x = p[d.key];
      return '['+r(x-left)+','+r(x+right)+')'
    };
  } else
    return (p: any) => r(p[d.key]);
}

export function tooltip_formatter(dim: Array<PointDim>, val: PointDim|undefined=undefined):
  (this: Highcharts.TooltipFormatterContextObject) => string {
  const dimf = dim.map(point_formatter);
  const valf = val && point_formatter(val);
  return function (this: Highcharts.TooltipFormatterContextObject) {
    const p = this.point;
    let r = dimf.map((f) => f(p)).join(', ');
    if (valf)
      r += ": " + valf(p);
    return r;
  };
}

export function axis_options(field: Field, log: LogScale, min: number|undefined=undefined, max: number|undefined=undefined): Highcharts.AxisOptions {
  return {
    type: log == 'client' ? "logarithmic" : "linear",
    title: axis_title(field),
    gridLineWidth: 1,
    labels: {
      formatter:
        field.base === "f"
          ? log == 'server'
            ? function () {
                const v = Math.exp(<number>this.value);
                let d = (<any>this.axis).tickInterval;
                return v.toPrecision(
                  1 + Math.max(0, -Math.floor(Math.log10(Math.exp(d) - 1)))
                );
              }
            : function () {
                const v = <number>this.value;
                const d = (<any>this.axis).tickInterval;
                return v.toPrecision(
                  1 +
                    Math.max(
                      0,
                      Math.floor(Math.log10(Math.abs(v))) -
                        Math.floor(Math.log10(d))
                    )
                );
              }
          : render_funct(field, log == 'server')
    },
    min: log == 'server' ? Math.log(min) : min,
    max: log == 'server' ? Math.log(max) : max,
    reversed: field.reversed || false
  };
}

export function histogram_options(
  field: Field,
  log: LogScale = null,
  ylog: boolean = false
): Highcharts.Options {
  const render = render_funct(field, log == 'server');
  return {
    chart: {
      animation: false,
      zoomType: "x",
    },
    legend: {
      enabled: false,
    },
    title: {
      text: undefined,
    },
    credits: {
      enabled: false,
    },
    xAxis: <Highcharts.XAxisOptions>axis_options(field, log),
    yAxis: {
      id: "tog",
      type: ylog ? "logarithmic" : "linear",
      title: { text: "Count" },
      allowDecimals: false,
      min: ylog ? null : 0,
    },
    tooltip: {
      animation: false,
    },
    exporting: {
      enabled: true,
    },
    plotOptions: {
      series: {
        animation: { duration: 0 },
      },
      column: {
        grouping: false,
        groupPadding: 0,
        pointPadding: 0,
        borderWidth: 0,
        shadow: false,
        pointPlacement: 0.5,
        states: {
          hover: {
            enabled: false,
          },
        },
      },
    },
  };
}

export const progressVue = new Vue({
  data: {
    update: undefined
  },
  methods: {
    cancel: function () {
      if (this.update && this.update.abort)
        this.update.abort('update canceled');
    }
  }
});

