import $ from "jquery";
import "datatables.net";
import DataTablesScroller from "datatables.net-scroller";
import Chart from "chart.js";

DataTablesScroller();

function assert(x: null|undefined): never;
function assert(x: object): void;
function assert(x: any): void {
  if (x == null)
    throw new Error("assertion failure");
}

type Dict<T> = { [name: string]: T };

type Field = {
  name: string,
  type: string,
  title: string,
  descr: null|string,
  units: null|string,
  top: boolean,
  disp: boolean
};

type Catalog = {
  uri: string,
  bulk: string[],
  fields: Field[],
  count?: number,
};

type Query = undefined|string|number|{lb:string|number, ub:string|number};

interface Filter {
  field: number;
  name: string;
  query: Query;
  isint: boolean;
  update_aggs?: (aggs: Dict<any>) => void;
  select?: HTMLSelectElement;
  lb?: HTMLInputElement;
  ub?: HTMLInputElement;
  change?: () => void;
}

var TCat: DataTables.Api;
declare const Catalog: Catalog;
declare const Query: {offset:number, limit:number, sort:{field:string,asc:boolean}[], fields:string[], filter:{field:string,value:Query}, aggs:string[], hist:string|null};
const Fields_idx: Dict<number> = {};
const Filters: Array<Filter> = [];
var Sample: number = 1;
var Seed: undefined|number = 0;
var Update_aggs: number = 0;
var Histogram: number = -1;
const Histogram_bins = 100;
var Histogram_chart: Chart|undefined;
var Histogram_bin_width = 0;

var Download_query: Dict<string> = {};
function set_download(query: Dict<string> = Download_query) {
  delete query.limit;
  delete query.offset;
  delete query.aggs;
  delete query.hist;
  query.fields = TCat.columns(':visible').dataSrc().join(' ');
  const q = '?' + $.param(Download_query = query);
  const h = $('#download').html('download as ');
  for (let f of Catalog.bulk) {
    const a = document.createElement('a');
    h.append(a);
    a.id = 'download.' + f;
    a.href = Catalog.uri + '/' + f + q;
    a.appendChild(document.createTextNode(f));
    h.append(document.createTextNode(' '));
  }
}

var Histogram_drag_start: number|null = null;

function getChartX(point: any) {
  /* needs internal chart.js access? */
  return point._xScale.getLabelForIndex(point._index, point._datasetIndex);
}

function histogram(agg: {buckets: {key:number,doc_count:number}[]}) {
  const hist = Filters[Histogram];
  const field = Catalog.fields[hist.field];
  const data = {
    datasets: [{
      label: field.title,
      data: agg.buckets.map(d => { return {x:d.key,y:d.doc_count}; }),
      pointRadius: 0,
      showLine: true,
      steppedLine: true,
      fill: 'origin',
    }]
  };
  const xlabel = field.title + (field.units ? ' (' + field.units + ')' : '');
  Histogram_drag_start = null;
  $('#dhist').show();
  if (Histogram_chart) {
    Histogram_chart.data = data;
    (<any>Histogram_chart).options.scales.xAxes[0].scaleLabel.labelString = xlabel;
    Histogram_chart.update();
  }
  else
    Histogram_chart = new Chart('hist', {
      options: {
        maintainAspectRatio: false,
        scales: {
          xAxes: [<Chart.ChartXAxe>{
            type: 'linear',
            scaleLabel: {
              display: true,
              labelString: xlabel
            }
          }],
          yAxes: [<Chart.ChartYAxe>{
            type: 'linear',
            ticks: {
              beginAtZero: true
            },
            scaleLabel: {
              display: true,
              labelString: 'count'
            }
          }]
        },
        animation: {
          duration: 0
        },
        tooltips: {
          mode: 'index',
          intersect: false,
          callbacks: {
            label: function (item) {
              return "[" + item.xLabel + "," + (<any>item.xLabel+Histogram_bin_width) + "): " + item.yLabel + "\n(drag to filter)";
            }
          }
        },
        events: ["mousedown", "mouseup", "mousemove", "mouseout", "touchstart", "touchmove", "touchend"],
        hover: {
          mode: 'index',
          intersect: false,
          onHover: function(ev, points) {
            if (ev.type === 'mouseout')
              Histogram_drag_start = null;
            else if (!points)
              return;
            else if (ev.type === 'mousedown' || ev.type === 'touchstart')
              Histogram_drag_start = getChartX(points[0]);
            else if (Histogram_drag_start && (ev.type === 'mouseup' || ev.type === 'touchend')) {
              let left = Histogram_drag_start;
              let right = getChartX(points[0]);
              if (left == right) {
                /* select one bucket? ignore? */
                return;
              }
              if (right < left) {
                left = right;
                right = Histogram_drag_start;
              }
              right += Histogram_bin_width;
              const filt = Filters[Histogram];
              if (!(filt && filt.lb && filt.ub && filt.change))
                return;
              filt.lb.valueAsNumber = left;
              filt.ub.valueAsNumber = right;
              filt.change();
            }
          }
        },
      },
      type: 'scatter',
      data: data
    });
}

function hist_toggle_log(xy: string) {
  if (!Histogram_chart)
    return;
  const axis: Chart.CommonAxe = (<any>Histogram_chart).options.scales[xy+'Axes'][0];
  // let label = <Chart.ScaleTitleOptions>axis.scaleLabel;
  if (axis.type === 'logarithmic') {
    axis.type = 'linear';
    // label.labelString = (label.labelString as string).substr(4);
  } else {
    axis.type = 'logarithmic';
    // label.labelString = 'log ' + label.labelString;
  }
  Histogram_chart.update();
}

/* elasticsearch max_result_window */
const displayLimit = 10000;

function ajax(data: any, callback: ((data: any) => void), opts: any) {
  const query: any = {
    sort: data.order.map((o: any) => {
      return (o.dir == "asc" ? '' : '-') + data.columns[o.column].data;
    }).join(' ')
  };
  for (let fi = 0; fi < Update_aggs; fi++) {
    const filt = Filters[fi];
    if (filt.query != null)
      query[filt.name] = typeof filt.query === 'object' ? filt.query.lb+','+filt.query.ub : filt.query;
  }
  if (Sample < 1) {
    query.sample = Sample;
    if (Seed != undefined)
      query.sample += '@' + Seed;
  }
  query.offset = data.start;
  query.limit = data.length;
  const aggs = Filters.slice(Update_aggs);
  if (aggs)
    query.aggs = aggs.map((filt) => filt.name).join(' ');
  if (Histogram >= 0) {
    const hist = Filters[Histogram];
    const wid = typeof hist.query === 'object' ? <number>hist.query.ub - <number>hist.query.lb : null;
    if (wid && wid > 0) {
      Histogram_bin_width = wid/Histogram_bins;
      if (hist.isint)
        Histogram_bin_width = Math.ceil(Histogram_bin_width);
      query.hist = hist.name + ':' + Histogram_bin_width;
    }
  }
  $('td.loading').show();
  $.ajax({
    method: 'GET',
    url: Catalog.uri + '/catalog',
    data: query
  }).then((res: any) => {
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
    for (let filt of aggs) {
      const f = filt.update_aggs;
      if (f)
        f(res.aggregations[filt.name]);
    }
    Update_aggs = Filters.length;
    if (res.aggregations && res.aggregations.hist)
      histogram(res.aggregations.hist);
    set_download(query);
  }, (xhr, msg, err) => {
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
  seed.disabled = true;
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
    TCat.draw();
  };

  add_filt_row('sample', 'random sample',
    $('<span>').append('fraction ').append(samp),
    $('<span>').append('seed ').append(seed));
}

function add_filter(idx: number) {
  const field = Catalog.fields[idx];
  if (!field || !TCat)
    return;
  const tcol = TCat.column(field.name+':name').visible(true);
  if (Filters.some((f) => f.field === idx))
    return;
  const filt: Filter = {
    field: idx,
    name: field.name,
    query: undefined,
    isint: false
  };
  const update = () => {
    const i = Filters.indexOf(filt);
    if (i >= 0 && Update_aggs > i)
      Update_aggs = i+1;
  };
  const label = $(document.createElement('span'));
  label.append(
    $('<button class="remove">&times;</button>').on('click', function () {
      if (!TCat) return;
      const i = Filters.indexOf(filt);
      if (i < 0) return;
      if (Histogram > i)
        Histogram -=1;
      else if (Histogram === i) {
        Histogram =-1;
        $('#dhist').hide();
      }
      Filters.splice(i, 1);
      $('tr#filt-'+field.name).remove();
      Update_aggs = i;
      (<DataTables.ColumnMethods><any>tcol.search('')).visible(true).draw();
    }),
    field.title);
  switch (field.type) {
    case 'keyword': {
      const select = document.createElement('select');
      select.name = field.name;
      select.disabled = true;
      filt.update_aggs = (aggs: any) => {
        $(select).empty();
        select.appendChild(document.createElement('option'));
        for (let b of aggs.buckets) {
          const opt = document.createElement('option');
          opt.setAttribute('value', b.key);
          opt.textContent = b.key + ' (' + b.doc_count + ')';
          select.appendChild(opt);
        }
        select.value = '';
        select.disabled = false;
      };
      filt.change = function () {
        const val = select.value;
        if (val)
          filt.query = val;
        else
          filt.query = undefined;
        update();
        (<DataTables.ColumnMethods><any>tcol.search(val)).visible(!val).draw();
      };
      select.onchange = filt.change;
      filt.select = select;
      add_filt_row(field.name, label, select);
      break;
    }
    case 'byte':
    case 'short':
    case 'integer':
      filt.isint = true;
    case 'half_float':
    case 'float':
    case 'double': {
      const lb = <HTMLInputElement>document.createElement('input');
      lb.name = field.name+".lb";
      lb.title = "Lower bound for " + field.title + " values"
      const ub = <HTMLInputElement>document.createElement('input');
      ub.name = field.name+".ub";
      ub.title = "Upper bound for " + field.title + " values"
      lb.type = ub.type = "number";
      lb.step = ub.step = filt.isint ? <any>1 : "any";
      lb.disabled = ub.disabled = true;
      const avg = document.createElement('span');
      avg.innerHTML = "<em>loading...</em>";
      filt.update_aggs = (aggs: any) => {
        filt.query = { lb: aggs.min, ub: aggs.max };
        lb.defaultValue = lb.value = lb.min = ub.min = aggs.min;
        ub.defaultValue = ub.value = lb.max = ub.max = aggs.max;
        lb.disabled = ub.disabled = false;
        avg.textContent = aggs.avg;
      };
      filt.change = function () {
        const lbv = lb.valueAsNumber;
        const ubv = ub.valueAsNumber;
        if (lbv == ubv)
          filt.query = lbv;
        else
          filt.query = {
            lb:isFinite(lbv) ? lbv : lb.defaultValue,
            ub:isFinite(ubv) ? ubv : ub.defaultValue
          };
        update();
        (<DataTables.ColumnMethods><any>tcol.search(lbv+" TO "+ubv)).visible(lbv!=ubv).draw();
      };
      lb.onchange = ub.onchange = filt.change;
      filt.lb = lb;
      filt.ub = ub;
      add_filt_row(field.name, label,
        $('<span>').append(lb).append(' &ndash; ').append(ub),
        $('<span><em>M</em> = </span>').append(avg),
        $('<button>histogram</button>').on('click', function () {
          const i = Filters.indexOf(filt);
          if (i !== Histogram) {
            Histogram = Filters.indexOf(filt);
            tcol.draw(false);
          }
        })
      );
      break;
    }
    default:
      return;
  }
  Filters.push(filt);
  tcol.search('');
}

(<any>window).hide_column = function hide_column(event:Event) {
  if (TCat) {
    TCat.column((<HTMLElement>event.target).id.substr(5)+':name').visible(false);
    set_download();
  }
  event.stopPropagation();
  return false;
};

function init() {
  Update_aggs = 0;
  const table = $('table#tcat');
  if (!(<any>window).Catalog || !table.length)
    return;
  for (let i = 0; i < Catalog.fields.length; i++)
    Fields_idx[Catalog.fields[i].name] = i;
  const topts: DataTables.Settings = {
    serverSide: true,
    ajax: ajax,
    deferLoading: 1,
    scrollX: true,
    pageLength: 50,
    processing: true,
    dom: 'i<"#download">rtlp',
    deferRender: true,
    pagingType: 'simple',
    columns: Catalog.fields.map((c) => {
      return { name: c.name };
    })
  };
  if ((<any>window).Query) {
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
        c.visible = Query.fields.indexOf(<string>c.name) >= 0;
  }
  TCat = table.DataTable(topts);
  /* for debugging: */
  (<any>window).TCat = TCat;

  const addfilt = <HTMLSelectElement>document.createElement('select');
  addfilt.appendChild(document.createElement('option'));
  add_filt_row('', addfilt, 'Select field to view/filter');
  for (let i = 0; i < Catalog.fields.length; i++) {
    let f = Catalog.fields[i];
    let opt = document.createElement('option');
    opt.setAttribute('value', i.toString());
    opt.textContent = f.title;
    if (f.descr)
      opt.setAttribute('title', f.descr);
    addfilt.appendChild(opt);
    if (f.top)
      add_filter(i);
  }
  addfilt.onchange = function () {
    add_filter(<any>addfilt.value);
    TCat.draw(false);
  };
  add_sample();
  TCat.draw();

  for (let xy of "xy")
    $('#dhist-'+xy+'-tog').on('click', hist_toggle_log.bind(undefined, xy));
}

$(init);
