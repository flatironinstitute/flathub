import $ from "jquery";
import "datatables.net";
import DataTablesScroller from "datatables.net-scroller";

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
  descr: string,
  top: boolean,
  disp: boolean
};

type Catalog = {
  title: string,
  query: {method: string, uri: string, csv: string},
  fields: Field[],
  count?: number,
};

type Query = undefined|string|number|{lb:string|number, ub:string|number};

interface Filter {
  name: string;
  query: Query;
  update_aggs?: (aggs: Dict<any>) => void;
}

var TCat: DataTables.Api;
declare const Catalog: Catalog;
const Filters: Array<Filter> = [];
var Update_aggs: number = 0;

function select_options(select: HTMLSelectElement, options: string[]|Dict<string>) {
  $(select).empty();
  select.appendChild(document.createElement('option'));
  for (let i in options) {
    let opt = document.createElement('option');
    opt.setAttribute('value', i);
    opt.textContent = (<any>options)[i];
    select.appendChild(opt);
  }
}

function set_download(query: Dict<string>) {
  let a = <HTMLAnchorElement>document.getElementById('download.csv');
  if (!a)
    a = <HTMLAnchorElement>$('#download').html('download as <a id="download.csv">csv</a>').children('a')[0];
  a.href = Catalog.query.csv + '?' + $.param(query);
}

function ajax(data: any, callback: ((data: any) => void), opts: any) {
  const query: any = {
    sort: data.order.map((o: any) => {
      return (o.dir == "asc" ? '' : '-') + data.columns[o.column].data;
    }).join(',')
  };
  for (let fi = 0; fi < Update_aggs; fi++) {
    const filt = Filters[fi];
    if (filt.query != null)
      query[filt.name] = typeof filt.query == 'object' ? filt.query.lb+','+filt.query.ub : filt.query;
  }
  const aggs = Filters.slice(Update_aggs);
  set_download(query);
  query.offset = data.start;
  query.limit = data.length;
  if (aggs)
    query.aggs = aggs.map((filt) => filt.name).join(',');
  $.ajax({
    method: Catalog.query.method,
    url: Catalog.query.uri,
    data: query
  }).then((res: any) => {
    $('td.loading').remove();
    Catalog.count = Math.max(Catalog.count || 0, res.hits.total);
    callback({
      draw: data.draw,
      recordsTotal: Catalog.count,
      recordsFiltered: res.hits.total,
      data: res.hits.hits
    });
    for (let filt of aggs) {
      let f = filt.update_aggs;
      if (f)
        f(res.aggregations[filt.name]);
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

function add_filter(field: Field) {
  if (!field || !TCat)
    return;
  const tcol = TCat.column(field.name+':name').visible(true);
  if (Filters.some((f) => f.name == field.name))
    return;
  let isint: boolean = false;
  const filt: Filter = {
    name: field.name,
    query: undefined,
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
      Filters.splice(i, 1);
      $('tr#filt-'+field.name).remove();
      Update_aggs = i;
      tcol.search('').draw();
    }),
    field.name);
  switch (field.type) {
    case 'keyword': {
      let select = document.createElement('select');
      select.name = field.name;
      select.disabled = true;
      filt.update_aggs = (aggs: any) => {
        let opts: Dict<string> = {};
        for (let b of aggs.buckets)
          opts[b.key] = b.key + ' (' + b.doc_count + ')';
        select_options(select, opts);
        select.value = '';
        select.disabled = false;
      };
      let onchange = function () {
        let val = select.value;
        if (val)
          filt.query = val;
        else
          filt.query = undefined;
        update();
        (<DataTables.ColumnMethods><any>tcol.search(val)).visible(!val).draw();
      };
      select.onchange = onchange;
      add_filt_row(field.name, label, select);
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
      lb.name = field.name+".lb";
      let ub = <HTMLInputElement>document.createElement('input');
      ub.name = field.name+".ub";
      lb.type = ub.type = "number";
      lb.step = ub.step = isint ? <any>1 : "any";
      lb.disabled = ub.disabled = true;
      let avg = document.createElement('span');
      avg.innerHTML = "<em>loading...</em>";
      filt.update_aggs = (aggs: any) => {
        lb.defaultValue = lb.value = lb.min = ub.min = aggs.min;
        ub.defaultValue = ub.value = lb.max = ub.max = aggs.max;
        lb.disabled = ub.disabled = false;
        avg.textContent = aggs.avg;
      };
      let onchange = function () {
        let lbv = lb.valueAsNumber;
        let ubv = ub.valueAsNumber;
        if (lbv == ubv)
          filt.query = lbv;
        else
          filt.query = {
            lb:isFinite(lbv) ? lbv : '',
            ub:isFinite(ubv) ? ubv : ''
          };
        update();
        (<DataTables.ColumnMethods><any>tcol.search(lbv+" TO "+ubv)).visible(lbv!=ubv).draw();
      };
      lb.onchange = ub.onchange = onchange;
      add_filt_row(field.name, label,
        $('<span>').append(lb).append(' &ndash; ').append(ub),
        $('<span><em>M</em> = </span>').append(avg));
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
  });
  /* for debugging: */
  (<any>window).TCat = TCat;
  let addfilt = <HTMLSelectElement>document.createElement('select');
  select_options(addfilt, Catalog.fields.map((f) => f.name));
  add_filt_row('', addfilt);
  addfilt.onchange = function () {
    add_filter(Catalog.fields[<any>addfilt.value]);
    TCat.draw();
  };
  for (let f of Catalog.fields)
    if (f.top)
      add_filter(f);
  $('.hide').on('click', function (event) {
    if (TCat)
      TCat.column(this.id.substr(5)+':name').visible(false);
    event.stopPropagation();
  });
  TCat.draw();
}

$(init);
