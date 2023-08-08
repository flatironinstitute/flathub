"use client";

import "highcharts/css/highcharts.css";

import type * as schema from "./flathub-schema";
import type {
  ColumnDef,
  GroupColumnDef,
  AccessorColumnDef,
  HeaderGroup,
} from "@tanstack/react-table";
import type {
  QueryObserverResult,
  QueryObserverOptions,
} from "@tanstack/query-core";
import type { Readable, Writable } from "svelte/store";

import React from "react";
import {
  useQuery,
  useMutation,
  useQueryClient,
  QueryClient,
  QueryClientProvider,
} from "@tanstack/react-query";
import {
  createColumnHelper,
  flexRender,
  getCoreRowModel,
  getPaginationRowModel,
  useReactTable,
} from "@tanstack/react-table";
import { QueryObserver, parseQueryArgs } from "@tanstack/query-core";

import { TrashIcon } from "@radix-ui/react-icons";

import { Listbox, Switch } from "@headlessui/react";
import { ChevronUpDownIcon, CheckCircleIcon } from "@heroicons/react/20/solid";
import * as d3 from "d3";
import renderMathInElement from "katex/contrib/auto-render";
import Highcharts from "highcharts";
import HighchartsReact from "highcharts-react-official";
import { produce } from "immer";
import * as Slider from "@radix-ui/react-slider";
import { readable, writable, derived, get } from "svelte/store";
import * as Dialog from "@radix-ui/react-dialog";
import { Cross1Icon } from "@radix-ui/react-icons";

const query_client = new QueryClient();

const catalog_name_store = derived<any, { get(cell_id: CellID): string }>(
  [],
  () => {
    return {
      get(cell_id: CellID) {
        return `camels`;
      },
    };
  }
);

const actions_store = writable<Action[]>([
  { type: `add_query_cell`, catalog_name: `camels`, cell_id: `query_cell_1` },
  { type: `add_query_cell`, catalog_name: `camels`, cell_id: `query_cell_2` },
  { type: `add_query_cell`, catalog_name: `camels`, cell_id: `query_cell_3` },
]);

function unique_by<T>(arr: T[], key: (t: T) => any) {
  const seen = new Set();
  return arr.filter((item) => {
    const k = key(item);
    return seen.has(k) ? false : seen.add(k);
  });
}

const cells_store = derived(actions_store, ($actions) => {
  const cell_actions = $actions.filter((action): action is CellAction =>
    [`add_query_cell`].includes(action.type)
  );
  const cells = [];
  for (const cell_action of cell_actions) {
    if (cell_action.type === `add_query_cell`) {
      cells.push({
        type: `query`,
        id: cell_action.cell_id,
        catalog_name: cell_action.catalog_name,
      });
    }
  }
  return unique_by(cells, (d) => d.id);
});

const dispatch_action = (action: Action) => {
  actions_store.update(($actions) => [...$actions, action]);
};

const filter_state_store = writable<
  Record<
    CellID,
    {
      [filter_name: string]: {
        gte: number;
        lte: number;
      };
    }
  >
>({});

const set_filter_value = (
  cell_id: CellID,
  filter_name: string,
  filter_value: {
    gte: number;
    lte: number;
  }
) => {
  filter_state_store.update((fitler_state_object) => {
    return produce(fitler_state_object, (draft) => {
      draft[cell_id] = draft[cell_id] || {};
      draft[cell_id][filter_name] = filter_value;
    });
  });
};

function subscribe_to_many<T>(
  store: Readable<Record<string, any>>,
  debug?: string
): Readable<Record<string, T>> {
  return derived(store, ($store, set, update) => {
    const entries = Object.entries($store);
    if (debug) log(`subscribe_to_many`, debug, entries);
    const unsubscribe_fns = entries.map(([key, observer]) => {
      const current = observer.getCurrentResult();
      update((d) => ({ ...d, [key]: current }));
      return observer.subscribe((result: T) => {
        update((d) => ({ ...d, [key]: result }));
      });
    });
    return () => {
      unsubscribe_fns.forEach((fn) => fn());
    };
  });
}

const catalog_metadata_queries_by_catalog_name_store: Readable<
  Record<string, QueryObserverResult<CatalogMetadataWrapper>>
> = subscribe_to_many(
  derived(
    cells_store,
    ($cells) => {
      const obj: Record<string, QueryObserver> = {};
      $cells.forEach((cell) => {
        const catalog_name = cell.catalog_name;
        const observer = create_query_observer({
          staleTime: Infinity,
          queryKey: ["catalog_metadata", { catalog: catalog_name }],
          queryFn: async (): Promise<CatalogMetadataWrapper> =>
            fetch_catalog_metadata(catalog_name),
        });
        obj[catalog_name] = observer;
      });
      return obj;
    },
    {}
  )
);

function create_query_observer<T>(options: QueryObserverOptions) {
  const defaulted_options = query_client.defaultQueryOptions(options);
  const observer = new QueryObserver(query_client, defaulted_options);
  return observer as QueryObserver<T>;
}

async function fetch_catalog_metadata(
  catalog_name: string
): Promise<CatalogMetadataWrapper> {
  const path = `/${catalog_name}`;
  const FLATHUB_API_BASE_URL = `https://flathub.flatironinstitute.org`;
  const url = new URL(`/api${path}`, FLATHUB_API_BASE_URL);
  log(`💥 fetching`, url.toString());
  try {
    const response = await fetch(url.toString(), {
      method: `GET`,
      headers: new Headers({
        "Content-Type": `application/json`,
      }),
    });
    if (!response.ok) {
      throw new Error(`API Fetch Error: ${response.status}`);
    }
    const metadata = (await response.json()) as CatalogResponse;
    log(`💥 metadata`, metadata);
    const catalog_fields_raw = metadata.fields ?? null;
    const root = { sub: catalog_fields_raw } as FieldGroup;
    const hierarchy: d3.HierarchyNode<FieldGroup> = d3.hierarchy<FieldGroup>(
      root,
      (d) => d?.sub ?? []
    );
    const nodes = get_nodes_depth_first<FieldGroup>(hierarchy).filter(
      (d) => `name` in d.data
    );
    const nodes_by_name = new Map<string, d3.HierarchyNode<FieldGroup>>();
    for (const node of nodes) {
      const name = node.data.name;
      if (nodes_by_name.has(name)) {
        throw new Error(`Duplicate node name: ${name}`);
      }
      nodes_by_name.set(name, node);
    }
    const initial_filter_names = nodes
      .filter((node) => node.height === 0 && `required` in node.data)
      .map((node) => node.data.name);
    const initial_column_names = nodes
      .filter((node) => node.height === 0 && node.data.disp === true)
      .map((node) => node.data.name);
    return {
      metadata,
      hierarchy,
      nodes,
      nodes_by_name,
      initial_filter_names,
      initial_column_names,
    };
  } catch (err: any) {
    throw new Error(`API Fetch Error: ${err.toString()}`);
  }
}

const catalog_metadata_store: Readable<{
  get(catalog_name: string): CatalogMetadataWrapper | null;
}> = derived(
  catalog_metadata_queries_by_catalog_name_store,
  ($catalog_metadata_queries_by_catalog_name) => {
    return {
      get: (catalog_name: string) => {
        const query = $catalog_metadata_queries_by_catalog_name[catalog_name];
        if (query.data) {
          return query.data;
        } else {
          return null;
        }
      },
    };
  }
);

const actions_by_type_store = derived(actions_store, ($actions) => {
  const actions_by_type = d3.group(
    $actions,
    (d) => d.type,
    (d) => d.cell_id
  );
  return Object.fromEntries(actions_by_type);
});

const actions_by_cell_id_store = derived(actions_store, ($actions) => {
  const actions_by_cell_id = d3.group(
    $actions,
    (d) => d.cell_id,
    (d) => d.type
  );
  return actions_by_cell_id;
});

const filter_set_store: Readable<{
  get(cell_id: CellID): Set<string>;
}> = derived(
  [
    catalog_name_store,
    catalog_metadata_queries_by_catalog_name_store,
    actions_by_cell_id_store,
  ],
  ([
    $catalog_name,
    $catalog_metadata_queries_by_catalog_name,
    $actions_by_cell_id,
  ]) => {
    // log(`actions by actions_by_cell_id`, $actions_by_cell_id);
    return {
      get(cell_id: CellID): Set<string> {
        const catalog_name = $catalog_name.get(cell_id);
        const catalog_metadata_query =
          $catalog_metadata_queries_by_catalog_name[catalog_name];
        const catalog_metadata = catalog_metadata_query?.data;
        const actions_for_cell = $actions_by_cell_id.get(cell_id);
        const remove_filter_actions =
          actions_for_cell?.get("remove_filter") ?? [];
        const add_filter_actions = actions_for_cell?.get("add_filter") ?? [];
        const filter_list_actions = [
          ...add_filter_actions,
          ...remove_filter_actions,
        ];
        const filter_set: Set<string> = new Set(
          catalog_metadata?.initial_filter_names ?? []
        );
        for (const action of filter_list_actions) {
          if (action.type === `remove_filter`) {
            filter_set.delete(action.filter_name);
            // filter_list = filter_list.filter((filter_name) => {
            //   return filter_name !== action.filter_name;
            // });
          } else if (action.type === `add_filter`) {
            filter_set.add(action.filter_name);
            // filter_list = [...filter_list, action.filter_name];
          } else {
            throw new Error(`unknown filter action type: ${action.type}`);
          }
        }
        return filter_set;
      },
    };
  }
);

const filter_values_store: Readable<{ get(cell_id: CellID): Filters }> =
  derived(
    [
      catalog_name_store,
      catalog_metadata_store,
      filter_set_store,
      filter_state_store,
    ],
    ([$catalog_name, $catalog_metadata, $filter_set, $filter_state]) => {
      return {
        get(cell_id: CellID) {
          const catalog_name = $catalog_name.get(cell_id);
          const hierarchy = $catalog_metadata.get(catalog_name)?.hierarchy;
          if (!hierarchy) {
            return {};
          }
          const filter_set: Set<string> = $filter_set.get(cell_id);
          const initial_filters: Filters = get_initial_cell_filters(
            filter_set,
            hierarchy
          );
          const filter_state_for_cell = $filter_state[cell_id] ?? {};
          const filters: Filters = {};
          for (const [field_name, initial_value] of Object.entries(
            initial_filters
          )) {
            const filter_state = filter_state_for_cell[field_name];
            const value = filter_state ?? initial_value;
            filters[field_name] = value;
          }
          return filters;
        },
      };
    }
  );

const debounced_filter_values_store: Readable<{
  get(cell_id: CellID): Filters;
}> = debounce_store(filter_values_store, 500);

function debounce_store<T>(store: Readable<T>, delay: number) {
  return derived(
    store,
    ($store, set) => {
      const timeout = setTimeout(() => {
        set($store);
      }, 500);
      return () => {
        clearTimeout(timeout);
      };
    },
    get(store)
  );
}

const query_config_by_cell_id_store = derived(
  [
    catalog_name_store,
    catalog_metadata_queries_by_catalog_name_store,
    debounced_filter_values_store,
  ],
  ([
    $catalog_name,
    $catalog_metadata_queries_by_catalog_name,
    $filter_values,
  ]) => {
    return {
      get(cell_id: CellID) {
        const catalog_name = $catalog_name.get(cell_id);

        const catalog_metadata_query =
          $catalog_metadata_queries_by_catalog_name[catalog_name];

        const field_names = get_catalog_initial_column_names(
          catalog_metadata_query
        );

        const filters = $filter_values?.get(cell_id);

        const request_body: DataRequestBody = {
          object: true,
          count: 100,
          fields: field_names,
          ...filters,
        };

        const query_config = {
          path: `/camels/data`,
          body: request_body,
        };
        return query_config;
      },
    };
  }
);

const data_queries_store: Readable<
  Record<CellID, QueryObserverResult<DataResponse>>
> = subscribe_to_many(
  derived(
    [cells_store, query_config_by_cell_id_store],
    ([$cells, $query_config_by_cell_id]) => {
      const obj: Record<CellID, QueryObserver<DataResponse>> = {};
      for (const { id: cell_id } of $cells) {
        const query_config = $query_config_by_cell_id.get(cell_id);
        const observer = create_query_observer<DataResponse>({
          staleTime: Infinity,
          enabled: false,
          queryKey: [`data`, query_config],
          queryFn: async (): Promise<DataResponse> => {
            return fetch_from_api<DataResponse>(
              query_config.path,
              query_config.body
            ).then((response) => {
              log(`query response`, response);
              return response;
            });
          },
        });
        obj[cell_id] = observer;
      }
      return obj;
    },
    {}
  ),
  `data queries`
);

const field_metadata_store = derived(
  [catalog_metadata_store],
  ([$catalog_metadata]) => {
    return {
      get(catalog_name: string, field_name: string): CatalogHierarchyNode {
        const catalog_field_hierarchy: CatalogHierarchyNode | undefined =
          $catalog_metadata.get(catalog_name)?.hierarchy;
        if (!catalog_field_hierarchy) {
          throw new Error(
            `Could not find catalog metadata for ${catalog_name}`
          );
        }
        const field_metadata = catalog_field_hierarchy.find(
          (node) => node.data.name === field_name
        );
        if (!field_metadata) {
          throw new Error(`Could not find field metadata for ${field_name}`);
        }
        return field_metadata;
      },
    };
  }
);

function useContextHelper<T>(debug?: string) {
  const context = React.createContext<T | null>(null);

  const useContext = (): T => {
    const value: T | null = React.useContext(context);
    if (value === null) {
      throw new Error(`useContextHelper: value is null: ${debug}`);
      // console.warn(`useContextHelper: value is null: ${debug}`);
    }
    return value;
  };

  return [useContext, context.Provider] as const;
}

function useStore<T>(store: Readable<T>) {
  const [state, setState] = React.useState<T>(get(store));

  React.useEffect(
    () =>
      store.subscribe((value) => {
        setState(value);
      }),
    [store]
  );

  return state;
}

const [useCellID, CellIDProvider] = useContextHelper<CellID>();
const [useCatalogName, CatalogNameProvider] = useContextHelper<string>();
const [useCatalogMetadata, CatalogMetadataProvider] = useContextHelper<
  CatalogMetadataWrapper | undefined
>();
const [useCellFilters, CellFiltersProvider] =
  useContextHelper<Filters>(`cell filters`);
const [useFieldName, FieldNameProvider] = useContextHelper<string>();

export default function App() {
  const cells = useStore(cells_store);

  return (
    <QueryClientProvider client={query_client}>
      <main
        className="ms-auto me-auto flex flex-col gap-y-10"
        style={{ width: `min(900px, 90vw)` }}
      >
        <div className="h-10" />
        {cells.map((cell) => {
          return (
            <CellIDProvider key={cell.id} value={cell.id}>
              <CatalogNameProvider value={cell.catalog_name}>
                <QueryCell />
              </CatalogNameProvider>
            </CellIDProvider>
          );
        })}
        <div className="h-10" />
      </main>
    </QueryClientProvider>
  );
}

function QueryCell() {
  const cell_id = useCellID();

  const catalog_name = useCatalogName();

  const catalog_metadata_query = useStore(
    catalog_metadata_queries_by_catalog_name_store
  )?.[catalog_name];

  const catalog_metadata = catalog_metadata_query.data;

  const catalog_field_hierarchy: CatalogHierarchyNode | undefined =
    catalog_metadata?.hierarchy;

  const field_names = get_catalog_initial_column_names(catalog_metadata_query);

  const data_query = useStore(data_queries_store)?.[cell_id];

  const fetching = data_query.isFetching;
  const query_data = data_query?.data ?? null;

  const ready_to_render = catalog_field_hierarchy && query_data;

  const has_data = query_data?.length && query_data?.length > 0;

  const filters = useStore(filter_values_store)?.get(cell_id);

  return (
    <CatalogMetadataProvider value={catalog_metadata}>
      <CellFiltersProvider value={filters}>
        <CellWrapper>
          <CellTitle subtitle={cell_id}>Query</CellTitle>
          <FieldsDialog />
          <CellSection label="filters">
            <CellFiltersSection />
          </CellSection>
          <CellSection label="fields">
            <CellFieldsSection field_names={field_names} />
          </CellSection>
          <CellSection label="fetch">
            <BigButton onClick={() => data_query.refetch()}>
              {fetching ? `Fetching Data...` : `Fetch Data`}
            </BigButton>
            <div>status: {data_query.status}</div>
            <div>fetch status: {data_query.fetchStatus}</div>
          </CellSection>
          <CellSection label="table">
            {ready_to_render && has_data ? (
              <Table
                data={query_data}
                catalog_field_hierarchy={catalog_field_hierarchy}
              />
            ) : (
              <PendingBox>No Results</PendingBox>
            )}
          </CellSection>
          <CellSection label="scatterplot">
            {ready_to_render && has_data ? (
              <Scatterplot
                data={query_data}
                catalog_field_hierarchy={catalog_field_hierarchy}
              />
            ) : (
              <PendingBox>No Results</PendingBox>
            )}
          </CellSection>
        </CellWrapper>
      </CellFiltersProvider>
    </CatalogMetadataProvider>
  );
}

function FieldsDialog() {
  const catalog_metadata = useCatalogMetadata();
  const fields_list = catalog_metadata?.nodes ?? [];
  const field_cards = fields_list
    .filter((d) => "name" in d.data)
    .map((field) => {
      return (
        <FieldNameProvider value={field.data.name} key={field.data.name}>
          <FieldCard></FieldCard>
        </FieldNameProvider>
      );
    });
  return (
    <Dialog.Root>
      <Dialog.Trigger className={BigButton.className}>
        Show All Fields
      </Dialog.Trigger>
      <Dialog.Portal>
        <Dialog.Overlay className="fixed inset-0 z-20 bg-black/50" />
        <Dialog.Content
          className={[
            `fixed z-50 w-[95vw] max-w-3xl rounded-lg p-4`,
            `top-[50%] left-[50%] -translate-x-[50%] -translate-y-[50%]`,
            `bg-white dark:bg-slate-700`,
            `focus:outline-none focus-visible:ring focus-visible:ring-purple-500 focus-visible:ring-opacity-75`,
          ].join(" ")}
        >
          <Dialog.Title className="text-sm font-medium text-slate-900 dark:text-slate-100">
            All Fields
          </Dialog.Title>
          <div className="h-4" />
          <Dialog.Description />
          <div className="h-4" />
          <input
            className="w-full dark:bg-slate-900 rounded-lg text-lg leading-5 py-2 px-3 focus:ring-2 focus:ring-slate-50 focus:outline-none"
            type="text"
            placeholder="search"
          />
          <div className="h-4" />
          <div className="h-[600px] overflow-y-scroll overflow-x-visible grid grid-cols-1 gap-4">
            {field_cards}
          </div>
          <Dialog.Close
            className={[
              "absolute top-3.5 right-3.5 inline-flex items-center justify-center rounded-full p-1",
              "focus:outline-none focus-visible:ring focus-visible:ring-purple-500 focus-visible:ring-opacity-75",
            ].join(" ")}
          >
            <Cross1Icon className="h-4 w-4 text-slate-500 hover:text-slate-700 dark:text-slate-500 dark:hover:text-slate-400" />
          </Dialog.Close>
        </Dialog.Content>
      </Dialog.Portal>
    </Dialog.Root>
  );
}

function CellFiltersSection() {
  const filters = useCellFilters();

  const filter_names = Object.keys(filters);

  const names_order = useCatalogMetadata()?.nodes.map((d) => d.data.name) ?? [];

  const sorted = d3.sort(filter_names, (d) => names_order.indexOf(d));

  return (
    <div className="grid grid-cols-1 gap-x-3 gap-y-2">
      {sorted.map((filter_name) => (
        <FieldNameProvider key={filter_name} value={filter_name}>
          <FieldCard filterMode />
        </FieldNameProvider>
      ))}
    </div>
  );
}

function FieldCard({
  filterMode = false,
}: {
  filterMode?: boolean;
}): React.JSX.Element {
  const [expanded, setExpanded] = React.useState(!filterMode);
  const cell_id = useCellID();
  const field_name = useFieldName();
  const filters = useCellFilters();
  const nodes_by_name = useCatalogMetadata()?.nodes_by_name;
  const field_node = nodes_by_name?.get(field_name);

  if (!field_node) {
    throw new Error(`Could not find node for ${field_name}`);
  }

  const is_leaf = field_node.children === undefined;
  const is_required = field_node.data.required;

  const field_title = <FieldTitles node={field_node}></FieldTitles>;

  const units = (() => {
    if (!expanded) return null;
    if (!field_node.data.units) return null;
    return (
      <div className="text-sm text-slate-500 dark:text-slate-300">
        <Katex>{field_node.data.units}</Katex>
      </div>
    );
  })();

  const field_description = (() => {
    if (!expanded) return null;
    if (!field_node.data.descr) return null;
    return (
      <div className="text-sm text-slate-500 dark:text-slate-300 overflow-hidden">
        <Katex>{field_node.data.descr}</Katex>
      </div>
    );
  })();

  const field_control = (() => {
    if (!filterMode) return null;
    const metadata = field_node?.data;
    // log(`field control`, metadata);
    if (!metadata) {
      throw new Error(`Could not find metadata for ${field_name}`);
    }
    if (metadata.type === `float`) {
      return <ConnectedRangeSlider />;
    }
  })();

  const remove_filter_button = (() => {
    if (!filterMode) return null;
    if (is_required) return null;
    return (
      <TrashIcon
        className="w-5 h-5 cursor-pointer"
        onClick={() => {
          dispatch_action({
            type: `remove_filter`,
            cell_id,
            filter_name: field_name,
          });
        }}
      />
    );
  })();

  const column_toggle = (() => {
    return <div>column</div>;
  })();

  const filter_toggle = (() => {
    if (!is_leaf) return null;
    const is_active_filter = field_name in filters;
    log(`filter active wha`, is_active_filter, field_name, filters);
    const on_change = (checked: boolean) => {
      log(`filter toggle`, checked);
      if (checked) {
        dispatch_action({
          type: `add_filter`,
          cell_id,
          filter_name: field_name,
        });
      } else {
        dispatch_action({
          type: `remove_filter`,
          cell_id,
          filter_name: field_name,
        });
      }
    };
    return (
      <Switch
        checked={is_active_filter}
        onChange={on_change}
        disabled={is_required}
        className="disabled:opacity-50 disabled:cursor-not-allowed"
      >
        {({ checked }) => (
          <div className="flex gap-x-2 items-center">
            <div
              className={`h-3 w-3 outline outline-2 dark:outline-slate-50 rounded-xl ${
                checked ? `bg-slate-50` : `bg-transparent`
              }`}
            ></div>
            <div>Filter</div>
          </div>
        )}
      </Switch>
    );
  })();

  const controls = filterMode ? (
    <>
      <div className="col-span-6">{field_control}</div>
      <div className="col-start-12 justify-self-end">
        {remove_filter_button}
      </div>
    </>
  ) : (
    <>
      <div className="col-start-9 col-span-2 justify-self-center">
        {column_toggle}
      </div>
      <div className="col-start-11 col-span-2 justify-self-center">
        {filter_toggle}
      </div>
    </>
  );

  return (
    <div className="rounded-md bg-slate-50 dark:bg-slate-600 text-md text-slate-200 px-4 py-1 flex flex-col gap-y-4">
      <div className="grid grid-cols-12 items-center ">
        <div className="col-span-5">{field_title}</div>
        {controls}
      </div>
      {units}
      {field_description}
      {expanded && <div className="h-1" />}
    </div>
  );
}

function FieldTitles({
  node,
}: {
  node: CatalogHierarchyNode;
}): React.JSX.Element {
  const title_strings = get_field_titles(node);
  const titles = title_strings.map((title, i, arr) => {
    const is_last = i === arr.length - 1;
    return (
      <React.Fragment key={title}>
        {i > 0 ? <span>:</span> : ``}
        <Katex className={is_last ? `opacity-100` : `opacity-40`}>
          {title}
        </Katex>
      </React.Fragment>
    );
  });
  return (
    <div data-type="field-titles" className="text-md flex gap-x-2">
      {titles}
    </div>
  );
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

function ConnectedRangeSlider() {
  const cell_id = useCellID();
  const catalog_name = useCatalogName();
  const field_name = useFieldName();
  const metadata = useStore(field_metadata_store).get(
    catalog_name,
    field_name
  )?.data;
  const min = metadata.stats?.min;
  const max = metadata.stats?.max;
  if (
    !Number.isFinite(min) ||
    !Number.isFinite(max) ||
    min === null ||
    max === null ||
    min === undefined ||
    max === undefined
  ) {
    throw new Error(
      `Could not find min/max stats for ${field_name} of type ${metadata.type}`
    );
  }
  const filters = useCellFilters();
  const filter_state = filters[field_name];
  if (!(typeof filter_state === `object`)) {
    throw new Error(`Expected filter state to be an object for ${field_name}`);
  }
  if (!(`gte` in filter_state) || !(`lte` in filter_state)) {
    log(filters, filter_state);
    throw new Error(`Expected filter state to have gte/lte for ${field_name}`);
  }
  const low = filter_state.gte;
  const high = filter_state.lte;
  if (typeof low !== `number` || typeof high !== `number`) {
    throw new Error(
      `Expected filter state to have gte/lte numbers for ${field_name}`
    );
  }
  const value = [low, high] as [number, number];
  return (
    <RangeSlider
      min={min}
      max={max}
      value={value}
      onValueChange={([low, high]) => {
        log(`on value change`, low, high);
        set_filter_value(cell_id, field_name, {
          gte: low,
          lte: high,
        });
      }}
    />
  );
}

function RangeSlider({
  min,
  max,
  value,
  onValueChange,
}: {
  min: number;
  max: number;
  value: [number, number];
  onValueChange: (value: [number, number]) => void;
}) {
  const [low, high] = value;
  const format = (d: number) => d3.format(`,.4~g`)(d);
  const range = Math.abs(max - min);
  const step = range / 100;
  const thumb_class = `block h-4 w-4 rounded-full dark:bg-white focus:outline-none focus-visible:ring focus-visible:ring-purple-500 focus-visible:ring-opacity-75`;
  return (
    <div
      className="grid gap-x-2 items-center justify-items-center"
      style={{ gridTemplateColumns: `10ch 1fr 10ch` }}
    >
      <div>{format(low)}</div>
      <Slider.Root
        min={min}
        max={max}
        value={value}
        className="cursor-pointer relative flex h-5 w-full touch-none items-center"
        onValueChange={onValueChange}
        step={step}
      >
        <Slider.Track className="relative h-1 w-full grow rounded-full bg-white dark:bg-slate-800">
          <Slider.Range className="absolute h-full rounded-full bg-purple-600 dark:bg-white" />
        </Slider.Track>
        <Slider.Thumb className={thumb_class} />
        <Slider.Thumb className={thumb_class} />
      </Slider.Root>
      <div>{format(high)}</div>
    </div>
  );
}

function get_field_metadata(
  field_name: string,
  catalog_field_hierarchy?: CatalogHierarchyNode
): CatalogHierarchyNode | undefined {
  if (!catalog_field_hierarchy) {
    return undefined;
  }
  const field_metadata = catalog_field_hierarchy.find(
    (node) => node.data.name === field_name
  );
  if (!field_metadata) {
    throw new Error(`Could not find field metadata for ${field_name}`);
  }
  return field_metadata;
}

function CellFieldsSection({ field_names }: { field_names: string[] }) {
  return (
    <div className="grid grid-cols-3 gap-x-3 gap-y-2">
      {field_names.map((field_name) => {
        return (
          <div
            key={field_name}
            className="text-xs text-slate-200 px-2 rounded dark:bg-slate-600"
          >
            {field_name}
          </div>
        );
      })}
    </div>
  );
}

function get_initial_cell_filters(
  filter_names: Set<string>,
  catalog_field_hierarchy?: d3.HierarchyNode<FieldGroup>
): Filters {
  if (!catalog_field_hierarchy) return {};
  const initial_filter_object: Filters = Object.fromEntries(
    Array.from(filter_names).map((filter_name) => {
      const metadata = catalog_field_hierarchy.find(
        (node) => node.data.name === filter_name
      )?.data;
      if (!metadata) {
        throw new Error(`Could not find metadata for filter ${filter_name}`);
      }
      const initial_value: Filters[string] = (() => {
        const type = metadata.type;
        if (type === `boolean`) {
          return true;
        } else if (type === `byte`) {
          return 0;
        } else if ([`float`, `short`].includes(type)) {
          if (!metadata.stats) {
            throw new Error(
              `Trying to use float filter without stats: ${filter_name}`
            );
          }
          if (
            metadata.stats.min === null ||
            metadata.stats.max === null ||
            !Number.isFinite(metadata.stats.min) ||
            !Number.isFinite(metadata.stats.max)
          ) {
            throw new Error(`Missing min/max for float filter: ${filter_name}`);
          }
          return {
            gte: metadata.stats.min,
            lte: metadata.stats.max,
          };
        } else {
          log(`meta`, metadata);
          throw new Error(`Unexpected filter type: ${type}`);
        }
      })();
      return [filter_name, initial_value];
    })
  );
  return initial_filter_object;
}

function get_catalog_initial_column_names(
  catalog_metadata_query: CatalogMetadataQuery
) {
  if (!catalog_metadata_query.data) {
    return [];
  }
  const initial_column_names =
    catalog_metadata_query.data?.initial_column_names;
  const current_column_names = initial_column_names;
  return current_column_names;
}

function get_nodes_depth_first<T>(
  root: d3.HierarchyNode<T>
): d3.HierarchyNode<T>[] {
  const nodes: d3.HierarchyNode<T>[] = [];
  root.eachBefore((d) => nodes.push(d));
  return nodes;
}

function Scatterplot({
  data,
  catalog_field_hierarchy,
}: {
  data: DataResponse;
  catalog_field_hierarchy: d3.HierarchyNode<FieldGroup>;
}) {
  const options: Highcharts.Options = {
    chart: {
      animation: false,
      styledMode: true,
    },
    title: {
      text: "My chart",
    },
    series: [
      {
        type: "scatter",
        animation: false,
        data: [1, 2, 3],
      },
    ],
  };

  return (
    <div>
      <HighchartsReact
        highcharts={Highcharts}
        options={options}
        containerProps={{ className: `highcharts-dark` }}
      />
    </div>
  );
}

function Table({
  data,
  catalog_field_hierarchy,
}: {
  data: DataResponse;
  catalog_field_hierarchy: d3.HierarchyNode<FieldGroup>;
}) {
  const columns = construct_table_columns(data, catalog_field_hierarchy);

  const table = useReactTable({
    data,
    columns,
    getCoreRowModel: getCoreRowModel(),
    getPaginationRowModel: getPaginationRowModel(),
  });

  const header_groups_raw = table.getHeaderGroups();

  const header_groups = fix_header_groups(header_groups_raw);

  const skip_rendering = new Set();

  return (
    <div className="overflow-x-scroll">
      <table className="text-xs">
        <thead>
          {header_groups.map((headerGroup) => (
            <tr key={headerGroup.id}>
              {headerGroup.headers.map((header_initial) => {
                let header = header_initial;
                let row_span = 1;
                if (skip_rendering.has(header.column.id)) return null;
                // If it's a placeholder:
                // - Skip rendering any future headers
                // - Set row span based on the depth of non-placeholder header
                if (header.isPlaceholder) {
                  skip_rendering.add(header.column.id);
                  const leaves = header.getLeafHeaders();
                  const non_placeholder_header = leaves.find(
                    (leaf) => !leaf.isPlaceholder
                  );
                  if (!non_placeholder_header) {
                    throw new Error(
                      `No non-placeholder header found for ${header.column.id}`
                    );
                  }
                  row_span = 1 + non_placeholder_header.depth - header.depth;
                }
                return (
                  <th
                    key={header.id}
                    colSpan={header.colSpan}
                    rowSpan={row_span}
                  >
                    {
                      <div>
                        {flexRender(
                          header.column.columnDef.header,
                          header.getContext()
                        )}
                      </div>
                    }
                  </th>
                );
              })}
            </tr>
          ))}
        </thead>
        <tbody>
          {table.getRowModel().rows.map((row) => (
            <tr key={row.id}>
              {row.getVisibleCells().map((cell) => (
                <td key={cell.id}>
                  {flexRender(cell.column.columnDef.cell, cell.getContext())}
                </td>
              ))}
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  );
}

function fix_header_groups<T>(raw: HeaderGroup<T>[]): HeaderGroup<T>[] {
  log(`FIX HEADER GROUPS`, raw);
  return raw;
}

/**
 * We have a list of **leaf** fields that we want to display in a table.
 * We need to get their ancestors for the column headers.
 *
 * @param data Table data
 * @param catalog_field_hierarchy The complete field hierarchy for this catalog
 * @returns An array of column definitions for use with react-table
 */
function construct_table_columns<T>(
  data: DataResponse,
  catalog_field_hierarchy: CatalogHierarchyNode
): ColumnDef<Datum>[] {
  if (data.length === 0) return [];
  // Get field names based on keys in data
  const field_names = Object.keys(data[0] ?? {});

  // Find the field nodes in the catalog field hierarchy
  const field_nodes = field_names
    .map((field_name) => {
      return catalog_field_hierarchy.find(
        (d: any) => d.data.name === field_name
      );
    })
    .filter((d): d is CatalogHierarchyNode => typeof d !== "undefined");

  // Get unix-like paths for nodes
  const get_path = (node: CatalogHierarchyNode) => {
    return node
      .ancestors()
      .map((d) => d.data.name ?? `ROOT`)
      .reverse()
      .join(`/`);
  };

  const field_paths = field_nodes.map(get_path);

  const next = (
    node: CatalogHierarchyNode
  ): GroupColumnDef<Datum> | AccessorColumnDef<Datum> | null => {
    // Get the path for this node
    const path = get_path(node);

    const include = field_paths.some((d) => d.startsWith(path));

    // Only include this node if its path is a partial match of one of the field_paths above
    if (!include) return null;

    const child_columns: ColumnDef<Datum>[] = [];

    for (const child of node.children ?? []) {
      const child_column = next(child);
      if (child_column) {
        child_columns.push(child_column);
      }
    }

    const column_base: ColumnDef<Datum> = {
      id: node.data.name,
      header: () => <Katex>{node.data.title ?? node.data.name}</Katex>,
    };

    if (child_columns.length === 0) {
      const column: AccessorColumnDef<Datum> = {
        ...column_base,
        accessorKey: node.data.name,
        cell: (row) => row.getValue(),
      };
      return column;
    } else {
      const column: GroupColumnDef<Datum> = {
        ...column_base,
        columns: child_columns,
      };
      return column;
    }
  };

  const root = next(catalog_field_hierarchy);

  if (root === null) {
    throw new Error(`root is null`);
  }

  if (!("columns" in root)) {
    throw new Error(`root.columns is not defined`);
  }

  const columns = root?.columns ?? [];

  log(`columns`, columns);

  return columns;
}

function PendingBox({ children }: { children: React.ReactNode }) {
  return (
    <div className="h-40 rounded-lg p-4 outline-2 outline-dashed outline-slate-50 grid place-items-center opacity-50">
      {children}
    </div>
  );
}

function CellTitle({
  children,
  subtitle,
}: {
  children: React.ReactNode;
  subtitle?: React.ReactNode;
}): React.JSX.Element {
  return (
    <div>
      <div className="text-2xl font-bold">{children}</div>
      {subtitle && <div className="text-slate-400">{subtitle}</div>}
    </div>
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
      <SimpleLabel>{label}</SimpleLabel>
      <div className="h-4"></div>
      {children}
    </div>
  );
}

function SimpleLabel({
  children,
}: {
  children: React.ReactNode;
}): React.JSX.Element {
  return (
    <div className="text-slate-400 dark:text-slate-400 uppercase text-sm">
      {children}
    </div>
  );
}

function BigButton({
  children,
  onClick,
}: {
  children: React.ReactNode;
  onClick?: () => void;
}): React.JSX.Element {
  return (
    <button className={BigButton.className} onClick={onClick}>
      {children}
    </button>
  );
}

BigButton.className =
  "bg-slate-500 dark:bg-slate-600 rounded-lg py-4 text-white font-bold text-xl";

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

function Katex({
  children,
  className,
}: {
  children: React.ReactNode;
  className?: string;
}): React.JSX.Element {
  const ref = React.useRef<HTMLSpanElement>(null);
  React.useEffect(() => {
    if (ref.current !== null) {
      renderMathInElement(ref.current, {
        output: "mathml",
        throwOnError: false,
        trust: true,
        delimiters: [
          { left: "$$", right: "$$", display: false },
          { left: "$", right: "$", display: false },
          { left: "\\(", right: "\\)", display: false },
        ],
      });
    }
  }, [ref]);
  return (
    <span ref={ref} className={className}>
      {children}
    </span>
  );
}

function fetch_from_api<T>(
  path: string,
  body?: Record<string, any>
): Promise<T> {
  const FLATHUB_API_BASE_URL = `https://flathub.flatironinstitute.org`;
  const url = new URL(`/api${path}`, FLATHUB_API_BASE_URL);
  log(`💥 fetching`, url.toString());
  return fetch(url.toString(), {
    method: `POST`,
    headers: new Headers({
      "Content-Type": `application/json`,
    }),
    body: JSON.stringify(body),
  }).then((response) => {
    if (!response.ok) {
      throw new Error(`API Fetch Error: ${response.status}`);
    }
    return response.json() as T;
  });
}

function log(...args: any[]) {
  console.log(`🌔`, ...args);
}

type Action = FilterListAction | AddQueryCellAction;

type FilterListAction = ActionBase<
  `add_filter` | `remove_filter`,
  { cell_id: CellID; filter_name: string }
>;

type CellAction = AddQueryCellAction;

type AddQueryCellAction = ActionBase<
  `add_query_cell`,
  { catalog_name: string; cell_id: CellID }
>;

// type Action =
//   | ActionBase<`add_catalog_cell`, CatalogCellData>
//   | ActionBase<`add_plot_cell`, PlotCellData>
//   | ActionBase<
//       `set_filter_value`,
//       { cell_id: CellID; field_name: FieldName; value: FilterValue }
//     >
//   | ActionBase<`add_filter`, { cell_id: CellID; field_name: FieldName }>
//   | ActionBase<`remove_filter`, { cell_id: CellID; field_name: FieldName }>
//   | ActionBase<`add_query_column`, { cell_id: CellID; field_name: FieldName }>
//   | ActionBase<
//       `remove_query_column`,
//       { cell_id: CellID; field_name: FieldName }
//     >;

type CatalogMetadataWrapper = {
  metadata: CatalogResponse;
  hierarchy: d3.HierarchyNode<FieldGroup>;
  nodes: d3.HierarchyNode<FieldGroup>[];
  nodes_by_name: Map<string, d3.HierarchyNode<FieldGroup>>;
  initial_filter_names: string[];
  initial_column_names: string[];
};

type ActionBase<T extends string, U> = U & {
  type: T;
};

type CellID = `query_cell_${number}`;

type CatalogHierarchyNode = d3.HierarchyNode<FieldGroup>;

type CatalogMetadataQuery = QueryObserverResult<CatalogMetadataWrapper>;

type Cell = any;

type Field = any;
type Filters = schema.components["schemas"]["Filters"];

type FilterAction = {
  type: `set_filter_value`;
  filter_name: string;
  value: { gte: number; lte: number };
};

// type DataQueryBody = schema.operations["dataPOST"]["requestBody"];

type DataRequestBody = NonNullable<
  schema.operations["dataPOST"]["requestBody"]
>["content"]["application/json"];

type FieldGroup = schema.components["schemas"]["FieldGroup"];

type CatalogResponse =
  schema.components["responses"]["catalog"]["content"]["application/json"];

type DataResponse = Array<Datum>;
type Datum = Record<string, any>;
