"use strict";
import Highcharts from "highcharts";

export function assert<T>(x: undefined|null|false|T): T {
  if (!x)
    throw new Error("assertion failure");
  return x;
}

export type Dict<T> = { [name: string]: T };

export type Field = {
  name: string,
  type: string,
  title: string,
  descr?: null|string,
  units?: null|string,
  top?: boolean,
  disp: boolean,
  base: 'f'|'i'|'b'|'s',
  terms?: boolean,
  enum?: null|string[],
  dict?: null|string
};

export type Catalog = {
  name: string,
  title: string,
  descr: null|string,
  bulk: string[],
  fields: Field[],
  count?: number,
};

export type AggrStats = {
  min: number,
  max: number,
  avg: number
};
export type AggrTerms<T> = {
  buckets: Array<{ key: T, doc_count: number, hist?: AggrTerms<any> }>
}

export type Aggr = AggrStats|AggrTerms<string|number>;

export type CatalogResponse = {
  hits: {
    total: number,
    hits: Dict<any>[]
  },
  aggregations?: Dict<Aggr>,
  histsize: number[]
};

export function fill_select_terms(s: HTMLSelectElement, f: Field, a: AggrTerms<string>) {
  s.add(document.createElement('option'));
  for (let b of a.buckets) {
    const opt = document.createElement('option');
    opt.value = b.key;
    if (f.enum && b.key in f.enum)
      opt.text = f.enum[<any>b.key];
    else
      opt.text = b.key;
    opt.text += ' (' + b.doc_count + ')';
    s.add(opt);
  }
  s.disabled = false;
}

export function field_option(f: Field): HTMLOptionElement {
  const o = document.createElement('option');
  o.value = f.name;
  o.text = f.title;
  if (f.descr)
    o.title = f.descr;
  return o;
}

export function render_funct(field: Field): (data: any) => string {
  if (field.base === 'f')
    return (data) => data != undefined ? parseFloat(data).toPrecision(8) : data;
  if (field.enum) {
    const e: string[] = field.enum;
    return (data) => data in e ? e[data] : data;
  }
  return (data) => data;
}

const axisProto = (<any>Highcharts).Axis.prototype;
axisProto.allowNegativeLog = true;
axisProto.log2lin = function (x: number) {
  return x >= 1 ? Math.log10(x) : -1;
};
axisProto.lin2log = function (x: number) {
  return Math.round(Math.pow(10, x));
};

export function toggle_log(chart: Highcharts.ChartObject) {
  const axis = <Highcharts.AxisObject>chart.get('tog');
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

export function axis_title(f: Field) {
  return {
    // useHTML: true, // not enough for mathjax
    text: f.title + (f.units ? ' [' + f.units + ']' : '')
  };
}

export function histogram_options(f: Field): Highcharts.Options {
  const render = render_funct(f);
  return {
    chart: {
      animation: false,
      zoomType: 'x',
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
      title: axis_title(f),
      gridLineWidth: 1,
    },
    yAxis: {
      id: 'tog',
      type: 'linear',
      title: { text: 'Count' },
      min: 0,
    },
    tooltip: {
      animation: false,
      formatter: function (this: Highcharts.PointObject): string {
        return '[' + render(this.x) + ',' + render(this.x+<number>(<Highcharts.ColumnChartSeriesOptions>this.series.options).pointInterval) + '): ' + this.y;
      }
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
  $('#'+ele).toggle();
}
