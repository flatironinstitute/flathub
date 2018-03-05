import $ from "jquery";
import "datatables.net";
System.import('datatables.net').then(jq => jq());

function assert(x: null|undefined): never;
function assert(x: object): void;
function assert(x: any): void {
  if (x == null)
    throw new Error("assertion failure");
}

type Dict<T> = { [name: string]: T };

type Properties = Dict<{
  type: string
}>;

type Catalog = {
  name: string,
  query: {method: string, uri: string},
  fixed: string[],
  count?: number,
  props: Properties
};

type Query = undefined|string|number|{lb:number, ub:number};

interface Filter {
  name: string;
  query: Query;
  update_aggs: (aggs: Dict<any>) => void;
}

var TCat: DataTables.Api|null = null;
const Catalog: Catalog = (<any>window).Catalog;
const Filters: Array<Filter> = [];
var Update_aggs: number = 0;

function select_options(select: HTMLSelectElement, options: string[]) {
  $(select).empty();
  select.appendChild(document.createElement('option'));
  for (let v of options) {
    let opt = document.createElement('option');
    opt.setAttribute('value', v);
    opt.textContent = v;
    select.appendChild(opt);
  }
}

function set_download(query: Dict<string>) {
  let a = document.getElementById('download.csv');
  if (!a)
    a = $('#download').html('download as <a id="download.csv">csv</a>').children('a')[0];
  a.href = Catalog.query.csv + '?' + $.param(query);
}

function ajax(data: any, callback: ((data: any) => void), opts: any) {
  const aggs: string[] = [];
  const query: any = {
    sort: data.order.map((o: any) => {
      return (o.dir == "asc" ? '' : '-') + data.columns[o.column].data;
    }).join(',')
  };
  for (let fi = 0; fi < Filters.length; fi++) {
    const filt = Filters[fi];
    if (fi < Update_aggs) {
      if (filt.query != null)
        query[filt.name] = typeof filt.query == 'object' ? filt.query.lb+','+filt.query.ub : filt.query;
    } else {
      aggs.push(filt.name);
    }
  }
  set_download(query);
  query.offset = data.start;
  query.limit = data.length;
  query.aggs = aggs.join(',');
  $.ajax({
    method: Catalog.query.method,
    url: Catalog.query.uri,
    data: query
  }).then((res: any) => {
    Catalog.count = Math.max(Catalog.count || 0, res.hits.total);
    callback({
      draw: data.draw,
      recordsTotal: Catalog.count,
      recordsFiltered: res.hits.total,
      data: res.hits.hits
    });
    for (let fi = Update_aggs; fi < Filters.length; fi++) {
      Filters[fi].update_aggs(res.aggregations[Filters[fi].name]);
    }
    Update_aggs = Filters.length;
  }, (xhr, msg, err) => {
    callback({
      draw: data.draw,
      data: [],
      error: msg + ": " + err
    });
  });
}

function add_filt_row(name: string, ...nodes: Array<JQuery.htmlString | JQuery.TypeOrArray<JQuery.Node | JQuery<JQuery.Node>>>): HTMLTableRowElement {
  let id = 'filt.'+name;
  let tr = <HTMLTableRowElement|null>document.getElementById(id);
  if (tr) return tr;
  let tab = <HTMLTableElement>document.getElementById('filt');
  tr = document.createElement('tr');
  if (tab.lastChild)
    $(tr).insertBefore(<HTMLTableRowElement>tab.lastChild);
  else
    $(tr).appendTo(tab);
  for (let node of nodes) {
    let td = $(document.createElement('td')).appendTo(tr);
    td.append(node);
  }
  return tr;
}

function add_filter(name: string) {
  if (Filters.some((f) => f.name == name))
    return;
  let isint: boolean = false;
  switch (Catalog.props[name].type) {
    case 'keyword': {
      let select = document.createElement('select');
      select.name = name;
      let filt: Filter = {
        name: name,
        query: undefined,
        update_aggs: (aggs: any) => {
          select_options(select, aggs.buckets.map((b: any) => b.key));
          select.value = '';
        }
      };
      let next = Filters.push(filt);
      let onchange = function () {
        if (!TCat)
          return;
        let val = select.value;
        if (val)
          filt.query = val;
        else
          filt.query = undefined;
        Update_aggs = Math.min(Update_aggs, next);
        TCat.column(name).visible(!val).search(val).draw();
      };
      onchange();
      select.onchange = onchange;
      add_filt_row(name, name, select);
      break;
    }
    case 'byte':
    case 'short':
    case 'integer':
      isint = true;
    case 'half_float':
    case 'float':
    case 'double': {
      let lb = <HTMLInputElement>document.createElement('input');
      lb.name = name+".lb";
      let ub = <HTMLInputElement>document.createElement('input');
      ub.name = name+".ub";
      lb.type = ub.type = "number";
      lb.step = ub.step = isint ? <any>1 : "any";
      let avg = document.createElement('span');
      let filt: Filter = {
        name: name,
        query: undefined,
        update_aggs: (aggs: any) => {
          lb.defaultValue = lb.value = lb.min = ub.min = aggs.min;
          ub.defaultValue = ub.value = lb.max = ub.max = aggs.max;
          avg.textContent = aggs.avg;
        }
      };
      let next = Filters.push(filt);
      let onchange = function () {
        if (!TCat)
          return;
        let lbv = lb.valueAsNumber;
        let ubv = ub.valueAsNumber;
        if (lbv == ubv)
          filt.query = lbv;
        else
          filt.query = {
            lb:isFinite(lbv) ? lbv : '',
            ub:isFinite(ubv) ? ubv : ''
          };
        Update_aggs = Math.min(Update_aggs, next);
        TCat.column(name).search(lbv+" TO "+ubv).draw();
      };
      onchange();
      lb.onchange = ub.onchange = onchange;
      add_filt_row(name, name,
        $('<span>').append(lb).append(' &ndash; ').append(ub),
        $('<span><em>M</em> = </span>').append(avg));
      break;
    }
  }
}

function init() {
  let cols = [];
  let addfilt = <HTMLSelectElement>document.createElement('select');
  select_options(addfilt, Object.keys(Catalog.props));
  add_filt_row('', addfilt);
  for (let p in Catalog.props) {
    cols.push({
      data: p,
      name: p,
      title: p,
      defaultContent: "",
    });
  }
  addfilt.onchange = function () {
    add_filter(addfilt.value);
  };
  if (Catalog.fixed)
    Catalog.fixed.forEach(add_filter);
  Update_aggs = 0;
  if (TCat)
    TCat.destroy();
  TCat = $('table#tcat').DataTable({
    serverSide: true,
    ajax: ajax,
    columns: cols,
    scrollX: true,
    pageLength: 50,
    dom: 'l<"#download">rtip',
  });
}

$(() => {
  init();
});
