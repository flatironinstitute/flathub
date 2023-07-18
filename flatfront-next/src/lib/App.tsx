"use client";

import type { components } from "./flathub-schema";
import type { Context, Dispatch, SetStateAction } from "react";
import React from "react";
import * as uuid from "uuid";
import { Listbox, Switch } from "@headlessui/react";
import { ChevronUpDownIcon } from "@heroicons/react/20/solid";
import * as d3 from "d3";
import KaTeX from "katex";
import { produce } from "immer";
import memoize from "fast-memoize";

const GLYPH = {
  EM_DASH: `—`,
};

const AppControllerContext: Context<AppController> = React.createContext(
  {} as AppController
);

export default function App() {
  return (
    <AppControllerContext.Provider value={useAppControllerFactory()}>
      <main
        className="ms-auto me-auto flex flex-col gap-y-10"
        style={{ width: `min(900px, 90vw)` }}
      >
        <div className="h-10" />
        <Cells />
        <GlobalControls />
        <div className="h-10" />
      </main>
    </AppControllerContext.Provider>
  );
}

function Cells(): React.JSX.Element {
  const cell_components: Record<CellType, React.JSX.ElementType> = {
    catalog: CatalogCell,
    plot: PlotCell,
  };

  const controller = useAppController();

  const rendered = controller.cells_list.map((cell) => {
    const Component = cell_components[cell.type];
    return <Component key={cell.id} id={cell.id} />;
  });

  return <>{rendered}</>;
}

function CatalogCell({ id: cell_id }: { id: CellID }): React.JSX.Element {
  const controller = useAppController();
  const cell_data = controller.catalog_cells[cell_id];
  const catalog_response =
    controller.catalog_responses[cell_data.catalog_name]?.response;
  return (
    <CellWrapper>
      <CellSection label="id">{cell_id}</CellSection>
      <CellSection label="catalog id">{cell_data.catalog_name}</CellSection>
      <CellSection label="catalog">
        <div className="text-2xl">{catalog_response?.title}</div>
      </CellSection>
      <CellSection label="fields">
        <CatalogFieldsSection id={cell_id} />
      </CellSection>
      <CellSection label="filters">
        <CatalogFilterSection id={cell_id} />
      </CellSection>
    </CellWrapper>
  );
}

function CatalogFieldsSection({
  id: cell_id,
}: {
  id: CellID;
}): React.JSX.Element {
  const controller = useAppController();
  const fields = controller.get_cell_fields(cell_id)?.nodes_depth_first;
  console.log(`fields`, fields);
  const active_filter_names = controller.get_cell_active_filter_names(cell_id);
  const field_list = fields.map((field_node, index) => {
    const is_active = active_filter_names.has(field_node.data.name);
    const is_leaf = field_node.children === undefined;
    const is_required = field_node.data.required;
    let add_remove_switch = null;
    if (is_leaf) {
      const on_click = is_active
        ? () => controller.remove_filter(cell_id, field_node.data.name)
        : () => controller.add_filter(cell_id, field_node.data.name);
      add_remove_switch = (
        <button
          className="disabled:opacity-50"
          onClick={on_click}
          disabled={is_required}
        >
          {is_required
            ? `Filter is Required`
            : is_active
            ? `Remove Filter`
            : `Add Filter`}
        </button>
      );
    }
    return (
      <div
        className="rounded-md bg-white dark:bg-slate-600 p-4"
        key={field_node.data.name}
      >
        <FieldTitle node={field_node}></FieldTitle>
        {add_remove_switch}
      </div>
    );
  });
  return (
    <div>
      <div className="h-4"></div>
      <input
        className="w-full dark:bg-slate-900 rounded-lg text-lg leading-5 py-2 px-3 focus:ring-2 focus:ring-white focus:outline-none"
        type="text"
        placeholder="search"
      />
      <div className="h-4"></div>
      <div className="h-[600px] overflow-y-scroll overflow-x-visible flex flex-col gap-y-4">
        {field_list}
      </div>
    </div>
  );
}

function CatalogFilterSection({
  id: cell_id,
}: {
  id: CellID;
}): React.JSX.Element {
  const controller = useAppController();
  const active_filter_names = controller.get_cell_active_filter_names(cell_id);
  const all_fields = controller.get_cell_fields(cell_id);
  const active_filters = all_fields?.nodes_depth_first.filter((field_node) =>
    active_filter_names.has(field_node.data.name)
  );
  log(`filters`, active_filter_names);

  const filter_list = active_filters.map((field_node) => {
    // log(`active filter`, field_node);
    const is_required = field_node.data.required;
    const field_name = field_node.data.name;
    let remove_button = null;
    if (!is_required) {
      remove_button = (
        <button
          className="text-red-500 hover:text-red-700"
          onClick={() => controller.remove_filter(cell_id, field_name)}
        >
          remove
        </button>
      );
    }
    const filter_controls = (
      <FilterControls cell_id={cell_id} field_name={field_name} />
    );
    return (
      <div
        className="rounded-md bg-white dark:bg-slate-600 p-4"
        key={field_node.data.name}
      >
        <FieldTitle node={field_node}></FieldTitle>
        {filter_controls}
        {remove_button}
      </div>
    );
  });

  return (
    <div>
      <div className="h-4"></div>
      <div className="flex flex-col gap-y-4 ps-1 pe-1">{filter_list}</div>
    </div>
  );
}

function FilterControls({
  cell_id,
  field_name,
}: {
  cell_id: CellID;
  field_name: FieldName;
}): React.JSX.Element {
  const controller = useAppController();
  const value = controller.get_filter_value(cell_id, field_name);
  return (
    <div>
      <div>{value}</div>
      <input
        type="range"
        min="0"
        max="100"
        onInput={(evt) => {
          controller.set_filter_value(
            cell_id,
            field_name,
            evt.currentTarget.valueAsNumber
          );
        }}
      />
    </div>
  );
}

function FieldTitle({ node }: { node: FieldNode }): React.JSX.Element {
  const title_strings = get_field_titles(node);
  const titles = title_strings.map((title, i) => (
    <React.Fragment key={title}>
      {i > 0 ? <span>:</span> : ``}
      <Katex source={title} />
    </React.Fragment>
  ));
  return <div className="flex gap-x-2">{titles}</div>;
}

function get_field_titles<T extends { title?: string }>(
  node: d3.HierarchyNode<T>
): string[] {
  const titles: string[] = [];
  let current_node: d3.HierarchyNode<T> | null = node;
  while (current_node !== null) {
    if (current_node.data.title?.length ?? 0 > 0) {
      titles.push(current_node.data.title ?? `unknown`);
    }
    current_node = current_node.parent;
  }
  return titles.reverse();
}

function Katex({ source }: { source: string }): React.JSX.Element {
  let html = source;
  if (source.charCodeAt(0) === 92) {
    const fixed = source.replace(/^\\\(/, ``).replace(/\\\)$/, ``);
    html = KaTeX.renderToString(fixed, {
      output: "mathml",
      throwOnError: false,
      trust: true,
    });
  }
  return <span dangerouslySetInnerHTML={{ __html: html }} />;
}

function PlotCell({ id: cell_id }: { id: CellID }): React.JSX.Element {
  const cell_data = useAppController().plot_cells[cell_id];
  return (
    <CellWrapper>
      <CellSection label="id">{cell_id}</CellSection>
      <CellSection label="type">{cell_data.plot_type}</CellSection>
    </CellWrapper>
  );
}

function CellSection({
  label,
  children = null,
}: {
  label: string;
  children?: React.ReactNode;
}): React.JSX.Element {
  return (
    <div className="flex flex-col">
      <CellSectionLabel>{label}</CellSectionLabel>
      {children}
    </div>
  );
}

function CellSectionLabel({
  children,
}: {
  children: React.ReactNode;
}): React.JSX.Element {
  return (
    <div className="text-slate-400 dark:text-slate-500 uppercase text-sm">
      {children}
    </div>
  );
}

function CellWrapper({
  children,
}: {
  children: React.ReactNode;
}): React.JSX.Element {
  return (
    <div className="rounded font-mono bg-slate-200 dark:bg-slate-700 p-6 shadow-lg shadow-black dark:shadow-lg dark:shadow-black w-full transition-all flex flex-col gap-y-10">
      {children}
    </div>
  );
}

function GlobalControls(): React.JSX.Element {
  const controller = useAppController();
  return (
    <CellWrapper>
      <div className="grid grid-cols-10 auto-rows-[40px] gap-x-4 gap-y-4 items-center">
        <CatalogSelect />
        <DarkModeToggle />
      </div>
    </CellWrapper>
  );
}

function CatalogSelect() {
  const [selected, set_selected] = React.useState<TopResponseItem | null>(null);

  const controller = useAppController();
  const catalog_list_unsorted = controller.top_response ?? [];

  const loading = catalog_list_unsorted.length === 0;

  const catalog_list = d3.sort(catalog_list_unsorted, (d) => d.order);

  return (
    <>
      <div className="col-span-2">Add Catalog</div>
      <div className="col-span-6">
        <Listbox value={selected} onChange={set_selected} disabled={loading}>
          <div className="relative">
            <Listbox.Button className="relative w-full cursor-pointer rounded-md bg-white dark:bg-slate-900 py-2 pl-3 pr-10 text-left shadow-md disabled:opacity-50 disabled:cursor-wait">
              <span className="block truncate">
                {loading ? `Loading...` : selected?.title ?? `Select a Catalog`}
              </span>
              <span className="pointer-events-none absolute inset-y-0 right-0 flex items-center pr-2">
                <ChevronUpDownIcon
                  className="h-5 w-5 text-gray-400"
                  aria-hidden="true"
                />
              </span>
            </Listbox.Button>
            <Listbox.Options className="absolute mt-1 w-full overflow-auto rounded-md bg-white dark:bg-slate-900 py-1 shadow-lg">
              {catalog_list.map((catalog_list_item) => (
                <Listbox.Option
                  key={catalog_list_item.name}
                  value={catalog_list_item}
                  className={({ active }) =>
                    `relative cursor-pointer select-none py-2 pl-3 pr-4 ${
                      active ? `bg-slate-700` : ``
                    }`
                  }
                >
                  {catalog_list_item.title}
                </Listbox.Option>
              ))}
            </Listbox.Options>
          </div>
        </Listbox>
      </div>
      <button
        className="col-span-2 py-2 bg-white dark:bg-slate-900 rounded-md disabled:opacity-50"
        disabled={!selected}
        onClick={() => {
          if (selected) {
            // controller.add_catalog_cell(selected.name);
          }
        }}
      >
        Add
      </button>
    </>
  );
}

function DarkModeToggle() {
  const [dark_mode, set_dark_mode] = React.useState(true);

  React.useEffect(() => {
    if (dark_mode) {
      document.documentElement.classList.add("dark");
    } else {
      document.documentElement.classList.remove("dark");
    }
  }, [dark_mode]);

  return (
    <>
      <div className="col-span-2">Dark Mode</div>
      <div className="col-span-8">
        <Switch
          checked={dark_mode}
          onChange={set_dark_mode}
          className={`bg-white dark:bg-slate-900 relative inline-flex h-8 w-14 items-center rounded-full`}
        >
          <span className="sr-only">Enable notifications</span>
          <span
            className={`${
              dark_mode ? "translate-x-7" : "translate-x-1"
            } inline-block h-6 w-6 transform rounded-full bg-slate-500 dark:bg-white transition`}
          />
        </Switch>
      </div>
    </>
  );
}

function useAppController(): AppController {
  return React.useContext(AppControllerContext);
}

function useAppControllerFactory() {
  log(`useAppControllerFactory`);

  // The "top response" is the list of catalogs, which we only need to fetch once.
  const top_response = useStateObject<TopResponse | null>(null);

  const catalog_responses = useStateObject<
    Record<CatalogName, ResponseWrapper<CatalogResponse>>
  >({});

  // Fetch the top response on load
  React.useEffect(() => {
    fetch_from_api_raw<TopResponse>(`/`).then((response: TopResponse) =>
      top_response.set(response)
    );
  }, []);

  const initial_actions: Action[] = React.useMemo(
    () => [
      {
        type: `add_catalog_cell`,
        catalog_name: `camels`,
      },
      {
        type: `add_plot_cell`,
        plot_type: `scatterplot`,
      },
      {
        type: `add_catalog_cell`,
        catalog_name: `camels`,
      },
      {
        type: `add_catalog_cell`,
        catalog_name: `candels`,
      },
      {
        type: `add_catalog_cell`,
        catalog_name: `candels`,
      },
    ],
    []
  );

  const [app_state, dispatch] = React.useReducer<
    React.Reducer<AppState, Action>,
    Action[]
  >(
    produce((draft, action) => {
      console.log(`processing action`, action);
      app_reducer(draft, action);
    }),
    initial_actions,
    get_initial_app_state
  );

  log(`app_state`, app_state);

  // When catalog_cells changes, fetch the catalog data if we haven't already
  React.useEffect(() => {
    const existing_catalogs = catalog_responses.value;
    for (const { catalog_name } of Object.values(app_state.catalog_cells)) {
      // Check if we already have catalog response
      if (catalog_name in existing_catalogs) {
        log(`already have catalog response for ${catalog_name}`);
        continue;
      }
      // Fetch catalog, then add to catalog_responses
      fetch_from_api<CatalogResponse>(`/${catalog_name}`).then((response) => {
        catalog_responses.set((old_responses) => ({
          ...old_responses,
          [catalog_name]: response,
        }));
      });
      // Only fetch one catalog at a time
      break;
    }
  }, [app_state.catalog_cells, catalog_responses.value]);

  const get_catalog_fields = useMemoized(
    (catalog_name: CatalogName) => {
      console.count(`get_catalog_fields ${catalog_name}`);
      window.performance.mark(`get_catalog_fields`);
      const fields =
        catalog_responses.value[catalog_name]?.response?.fields ?? [];
      const root = { sub: fields } as Field;
      const hierarchy: FieldNode = d3.hierarchy<Field>(
        root,
        (d) => d?.sub ?? []
      );
      const nodes_depth_first = get_nodes_depth_first(hierarchy).filter(
        (d) => `name` in d.data
      );
      return {
        hierarchy,
        nodes_depth_first,
        raw: fields,
      };
    },
    [catalog_responses.value]
  );

  const get_catalog_initial_filters = useMemoized(
    (catalog_name: CatalogName) => {
      console.count(`get_catalog_initial_filters: ${catalog_name}`);
      window.performance.mark(`get_catalog_initial_filters`);
      const { nodes_depth_first } = get_catalog_fields(catalog_name);
      const initial_filters = nodes_depth_first.filter(
        (node) => `required` in node.data && node.height === 0
      );
      return initial_filters;
    },
    [get_catalog_fields]
  );

  const get_catalog_name = (cell_id: CellID) =>
    app_state.catalog_cells[cell_id]?.catalog_name;

  const get_cell_fields = (cell_id: CellID) => {
    console.count(`get_cell_fields ${cell_id}`);
    window.performance.mark(`get_cell_fields`);
    const catalog_name = get_catalog_name(cell_id);
    return get_catalog_fields(catalog_name);
  };

  const get_cell_active_filter_names = useMemoized(
    (cell_id: CellID) => {
      console.count(`get_cell_filters ${cell_id}`);
      window.performance.mark(`get_cell_filters`);
      const catalog_name = app_state.catalog_cells[cell_id].catalog_name;
      let active_filter_names: string[] = get_catalog_initial_filters(
        catalog_name
      ).map((d) => d.data.name);
      for (const action of app_state.filter_actions) {
        if (!(`cell_id` in action)) continue;
        if (action.cell_id !== cell_id) continue;
        if (action.type === `remove_filter`) {
          const field_name = action.field_name;
          active_filter_names = active_filter_names.filter(
            (name) => name !== field_name
          );
        }
        if (action.type === `add_filter`) {
          const field_name = action.field_name;
          active_filter_names.push(field_name);
        }
      }
      return new Set(active_filter_names);
    },
    [
      get_catalog_initial_filters,
      app_state.catalog_cells,
      app_state.filter_actions,
    ]
  );

  const get_filter_value = (cell_id: CellID, field_name: FieldName) => {
    return app_state.filter_values[cell_id]?.[field_name] ?? null;
  };

  const set_filter_value = (
    cell_id: CellID,
    field_name: FieldName,
    value: FieldValue
  ) => {
    dispatch({ type: `set_filter_value`, cell_id, field_name, value });
  };

  const add_filter = (cell_id: CellID, field_name: FieldName) => {
    dispatch({ type: `add_filter`, cell_id, field_name });
  };

  const remove_filter = (cell_id: CellID, field_name: FieldName) => {
    dispatch({ type: `remove_filter`, cell_id, field_name });
  };

  const temp = useStateObject<number>(0);

  return {
    top_response: top_response.value,
    catalog_responses: catalog_responses.value,
    get_cell_fields,
    get_cell_active_filter_names,
    get_filter_value,
    set_filter_value,
    add_filter,
    remove_filter,
    temp,
    ...app_state,
  };
}

function app_reducer(app_state: AppState, action: Action) {
  const {
    cells_list,
    catalog_cells,
    plot_cells,
    filter_values,
    filter_actions,
  } = app_state;
  if (action.type === "add_catalog_cell") {
    const num_catalog_cells = Object.keys(catalog_cells).length;
    const id: CellID = `catalog_cell_${num_catalog_cells}`;
    cells_list.push({ type: "catalog", id: id });
    catalog_cells[id] = { catalog_name: action.catalog_name };
  } else if (action.type === "add_plot_cell") {
    const num_plot_cells = Object.keys(plot_cells).length;
    const id: CellID = `plot_cell_${num_plot_cells}`;
    cells_list.push({ type: "plot", id: id });
    plot_cells[id] = { plot_type: action.plot_type };
  } else if (action.type === `set_filter_value`) {
    const { cell_id, field_name, value } = action;
    filter_values[cell_id] = filter_values[cell_id] ?? {};
    filter_values[cell_id][field_name] = value;
  } else if (action.type === `remove_filter` || action.type === `add_filter`) {
    filter_actions.push(action);
  } else {
    throw new Error(`unknown action type: ${(action as any).type}`);
  }
}

function get_initial_app_state(initial_actions: Action[]) {
  const cells_list: { type: CellType; id: CellID }[] = [];
  const catalog_cells: Record<CellID, CatalogCellData> = {};
  const plot_cells: Record<CellID, PlotCellData> = {};
  const filter_values: Record<CellID, Record<FieldName, FieldValue>> = {};
  const filter_actions: Action[] = [];

  const initial_state = {
    cells_list,
    catalog_cells,
    plot_cells,
    filter_values,
    filter_actions,
  };

  initial_actions.forEach((action: Action) => {
    app_reducer(initial_state, action);
  });

  return initial_state;
}

// function state_from_actions(actions: Action[]) {
//   const state: AppState = {};
//   for (const action of actions) {
//     if (action.type === `add_catalog_cell`) {
//     }
//   }
// }

// function create_state_handler(initial_actions?: Action[]) {
//   const action_map = new Map();
//   return {};
// }

function useMemoized<T, U>(fn: (t: T) => U, deps: any[]): typeof fn {
  return React.useCallback(memoize<typeof fn>(fn), deps);
}

function useStateObject<T>(initial_value: T): StateObject<T> {
  const [value, set] = React.useState<T>(initial_value);
  return { value, set };
}

function get_nodes_depth_first<T>(
  root: d3.HierarchyNode<T>
): d3.HierarchyNode<T>[] {
  const nodes: d3.HierarchyNode<T>[] = [];
  root.eachBefore((d) => nodes.push(d));
  return nodes;
}

function fetch_from_api_raw<T>(path: string, search_params?: any): Promise<T> {
  const FLATHUB_API_BASE_URL = `https://flathub.flatironinstitute.org`;
  const url = new URL(`/api${path}`, FLATHUB_API_BASE_URL);
  if (search_params) {
    url.search = new URLSearchParams(search_params).toString();
  }
  log(`💥 fetching`, url.toString());
  return fetch(url.toString())
    .then((response) => {
      if (!response.ok) {
        throw new Error(`API Fetch Error: ${response.status}`);
      }
      return response.json();
    })
    .then((json) => json as T);
}

function fetch_from_api<T>(
  path: string,
  search_params?: any
): Promise<ResponseWrapper<T>> {
  return fetch_from_api_raw<T>(path, search_params).then((json) => ({
    request: { path, search_params },
    response: json,
  }));
}

function log(...args: any[]) {
  console.log(`🌔`, ...args);
}

type AppState = ReturnType<typeof get_initial_app_state>;

type CellID = `${CellType}_cell_${number}`;

type CellType = "catalog" | "plot";

// type Things = "result_set" | "filter" | "filter_value" | "query_fields";

// type StateHandler = ReturnType<typeof create_state_handler>;

type Action =
  | ActionBase<`add_catalog_cell`, CatalogCellData>
  | ActionBase<`add_plot_cell`, PlotCellData>
  | ActionBase<
      `set_filter_value`,
      { cell_id: CellID; field_name: FieldName; value: FieldValue }
    >
  | ActionBase<`add_filter`, { cell_id: CellID; field_name: FieldName }>
  | ActionBase<`remove_filter`, { cell_id: CellID; field_name: FieldName }>;

type ActionBase<T extends string, U> = U & {
  type: T;
};

type PlotCellData = {
  plot_type: string;
};

type CatalogCellData = {
  catalog_name: string;
};

type TopResponseItem = TopResponse[number];

type TopResponse =
  components["responses"]["top"]["content"]["application/json"];

type CatalogResponse =
  components["responses"]["catalog"]["content"]["application/json"];

type FieldNode = d3.HierarchyNode<Field>;

type FieldValue = components["schemas"]["FieldValueScalar"];

type FieldName = Field["name"];

type Field = components["schemas"]["FieldGroup"];

type CatalogName = components["schemas"]["CatalogMeta"]["name"];

type StateObject<T> = {
  value: T;
  set: Dispatch<SetStateAction<T>>;
};

type ResponseWrapper<T> = {
  request: {
    path: string;
    search_params?: any;
  };
  response: T;
};

type AppController = ReturnType<typeof useAppControllerFactory>;
