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
  query: {method: string, uri: string, csv: string},
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
const Filters: Array<Filter> = [];
var Update_aggs: number = 0;
var Histogram: number = -1;
const Histogram_bins = 100;
var Histogram_chart: Chart|undefined;
var Histogram_bin_width = 0;

function set_download(query: Dict<string>) {
  let a = <HTMLAnchorElement>document.getElementById('download.csv');
  if (!a)
    a = <HTMLAnchorElement>$('#download').html('download as <a id="download.csv">csv</a>').children('a')[0];
  delete query.limit;
  delete query.offset;
  delete query.aggs;
  delete query.hist;
  query.fields = TCat.columns(':visible').dataSrc().join(',');
  a.href = Catalog.query.csv + '?' + $.param(query);
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
  let xlabel = field.title + (field.units ? ' (' + field.units + ')' : '');
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
  let axis: Chart.CommonAxe = (<any>Histogram_chart).options.scales[xy+'Axes'][0];
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
    }).join(',')
  };
  for (let fi = 0; fi < Update_aggs; fi++) {
    const filt = Filters[fi];
    if (filt.query != null)
      query[filt.name] = typeof filt.query === 'object' ? filt.query.lb+','+filt.query.ub : filt.query;
  }
  query.offset = data.start;
  query.limit = data.length;
  const aggs = Filters.slice(Update_aggs);
  if (aggs)
    query.aggs = aggs.map((filt) => filt.name).join(',');
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
  $.ajax({
    method: Catalog.query.method,
    url: Catalog.query.uri,
    data: query
  }).then((res: any) => {
    $('td.loading').remove();
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
      let f = filt.update_aggs;
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
  let id = 'filt-'+name;
  let tr = <HTMLTableRowElement|null>document.getElementById(id);
  if (tr) return;
  let tab = <HTMLTableElement>document.getElementById('filt');
  tr = document.createElement('tr');
  tr.id = id;
  if (tab.lastChild)
    $(tr).insertBefore(<HTMLTableRowElement>tab.lastChild);
  else
    $(tr).appendTo(tab);
  for (let node of nodes) {
    let td = $(document.createElement('td')).appendTo(tr);
    td.append(node);
  }
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
    let i = Filters.indexOf(filt);
    if (i >= 0 && Update_aggs > i)
      Update_aggs = i+1;
  };
  let label = $(document.createElement('span'));
  label.append(
    $('<button class="remove">&times;</button>').on('click', function () {
      if (!TCat) return;
      let i = Filters.indexOf(filt);
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
      let select = document.createElement('select');
      select.name = field.name;
      select.disabled = true;
      filt.update_aggs = (aggs: any) => {
        $(select).empty();
        select.appendChild(document.createElement('option'));
        for (let b of aggs.buckets) {
          let opt = document.createElement('option');
          opt.setAttribute('value', b.key);
          opt.textContent = b.key + ' (' + b.doc_count + ')';
          select.appendChild(opt);
        }
        select.value = '';
        select.disabled = false;
      };
      filt.change = function () {
        let val = select.value;
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
      let lb = <HTMLInputElement>document.createElement('input');
      lb.name = field.name+".lb";
      let ub = <HTMLInputElement>document.createElement('input');
      ub.name = field.name+".ub";
      lb.type = ub.type = "number";
      lb.step = ub.step = filt.isint ? <any>1 : "any";
      lb.disabled = ub.disabled = true;
      let avg = document.createElement('span');
      avg.innerHTML = "<em>loading...</em>";
      filt.update_aggs = (aggs: any) => {
        filt.query = { lb: aggs.min, ub: aggs.max };
        lb.defaultValue = lb.value = lb.min = ub.min = aggs.min;
        ub.defaultValue = ub.value = lb.max = ub.max = aggs.max;
        lb.disabled = ub.disabled = false;
        avg.textContent = aggs.avg;
      };
      filt.change = function () {
        let lbv = lb.valueAsNumber;
        let ubv = ub.valueAsNumber;
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

function init() {
  Update_aggs = 0;
  TCat = $('table#tcat').DataTable({
    serverSide: true,
    ajax: ajax,
    deferLoading: 1,
    scrollX: true,
    pageLength: 50,
    processing: true,
    dom: 'i<"#download">rtlp',
    deferRender: true,
    pagingType: 'simple',
  });
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
  $('.hide').on('click', function (event) {
    if (TCat)
      TCat.column(this.id.substr(5)+':name').visible(false);
    event.stopPropagation();
  });
  TCat.draw();

  for (let xy of "xy")
    $('#dhist-'+xy+'-tog').on('click', hist_toggle_log.bind(undefined, xy));
}

$(init);
