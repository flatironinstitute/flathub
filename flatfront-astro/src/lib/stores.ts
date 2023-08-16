import type {
  Action,
  CatalogHierarchyNode,
  CatalogMetadataWrapper,
  CatalogResponse,
  Cell,
  CatalogCell,
  CellAction,
  CellID,
  TableCellID,
  PlotCellID,
  DataRequestBody,
  DataResponse,
  FieldMetadata,
  FilterListAction,
  Filters,
  FilterValueRaw,
  FilterCellID,
  TopResponse,
  ColumnListAction,
} from "./types";
import type {
  QueryObserverResult,
  QueryObserverOptions,
} from "@tanstack/query-core";
import type { Readable } from "svelte/store";

import { QueryClient } from "@tanstack/query-core";
import { QueryObserver } from "@tanstack/query-core";
import * as d3 from "d3";
import { readable, writable, derived, get } from "svelte/store";
import * as lzstring from "lz-string";

import {
  log,
  find_parent_node_by_filter,
  is_filter_cell_id,
  get_field_id,
  set_field_id,
} from "./shared";

const FLATHUB_API_BASE_URL = `https://flathub.flatironinstitute.org`;

const query_client = new QueryClient();

const initial_actions: Action[] = [
  {
    type: "add_catalog_cell",
    cell_id: "catalog_cell_gr8",
    catalog_id: "gr8",
  },
  // {
  //   type: "add_catalog_cell",
  //   cell_id: "catalog_cell_camels",
  //   catalog_id: "camels",
  // },
  // {
  //   type: "add_filter_cell",
  //   cell_id: "filter_cell_1691622596626",
  //   parent_cell_id: "catalog_cell_camels",
  // },
  // {
  //   type: "add_table_cell",
  //   cell_id: "table_cell_1691701006344",
  //   parent_cell_id: "filter_cell_1691622596626",
  // },
];

export const actions = writable<Action[]>(initial_actions);

export const filter_state = writable<
  Record<CellID, Record<string, FilterValueRaw>>
>({} as Record<CellID, Record<string, FilterValueRaw>>);

actions.subscribe((actions) => log(`All actions:`, actions));

filter_state.subscribe((filter_state) =>
  log(`Global Filter State:`, filter_state)
);

debounce_store(actions, 1000).subscribe((actions) => {
  store_data_in_url(actions, `actions`);
});

debounce_store(filter_state, 1000).subscribe((filters) => {
  store_data_in_url(filters, `filters`);
});

const actions_by_type_store = derived(actions, ($actions) => {
  const actions_by_type = d3.group(
    $actions,
    (d) => d.type,
    (d) => d.cell_id
  );
  return Object.fromEntries(actions_by_type);
});

const actions_by_cell_id = derived(actions, ($actions) => {
  const actions_by_cell_id = d3.group($actions, (d) => d.cell_id);
  return actions_by_cell_id;
});

export const top_response: Readable<QueryObserverResult<TopResponse>> =
  readable<QueryObserverResult<TopResponse>>(null, (set, update) => {
    const observer: QueryObserver<TopResponse> = create_query_observer({
      staleTime: Infinity,
      queryKey: ["top"],
      queryFn: async (): Promise<TopResponse> =>
        fetch_api_get<TopResponse>(`/`),
    });
    const current: QueryObserverResult<TopResponse> =
      observer.getCurrentResult();
    set(current);
    const unsubscribe = observer.subscribe((result) => {
      set(result);
      if (result.status === `success`) {
        unsubscribe();
        observer.destroy();
      }
    });
  });

// top_response.subscribe((result) => log(`Top response:`, result, result.data));

const cells: Readable<Cell[]> = derived(actions, ($actions) => {
  const cell_action_types: CellAction["type"][] = [
    `add_catalog_cell`,
    `add_filter_cell`,
    `add_table_cell`,
    `remove_table_cell`,
    `remove_filter_cell`,
  ];
  // Filter cells to only include those with type in cell_action_types.
  const cell_actions = $actions.filter((action): action is CellAction =>
    cell_action_types.some((type) => action.type === type)
  );
  let cells: Cell[] = [];
  for (const cell_action of cell_actions) {
    if (cell_action.type === `add_catalog_cell`) {
      // If cells already includes a cell with this catalog name, don't add it.
      if (
        cells.some(
          (cell) =>
            cell.type === `catalog` &&
            cell.catalog_id === cell_action.catalog_id
        )
      ) {
        log(`Skipping adding duplicate catalog cell ${cell_action.catalog_id}`);
        continue;
      }
      cells.push({
        type: `catalog`,
        cell_id: cell_action.cell_id,
        catalog_id: cell_action.catalog_id,
        parent_cell_id: `root`,
      });
    } else if (cell_action.type === `add_filter_cell`) {
      cells.push({
        type: `filter`,
        cell_id: cell_action.cell_id,
        parent_cell_id: cell_action.parent_cell_id,
      });
    } else if (cell_action.type === `remove_filter_cell`) {
      // Remove the table cell with the given cell_id.
      cells = cells.filter((cell) => cell.cell_id !== cell_action.cell_id);
      const children = cells.filter(
        (cell) => cell.parent_cell_id === cell_action.cell_id
      );
      for (const child of children) {
        cells = cells.filter((cell) => cell.cell_id !== child.cell_id);
      }
    } else if (cell_action.type === `add_table_cell`) {
      cells.push({
        type: `table`,
        cell_id: cell_action.cell_id,
        parent_cell_id: cell_action.parent_cell_id,
      });
    } else if (cell_action.type === `remove_table_cell`) {
      // Remove the table cell with the given cell_id.
      cells = cells.filter((cell) => cell.cell_id !== cell_action.cell_id);
    } else {
      const type = (cell_action as CellAction).type;
      throw new Error(`Unrecognized cell action type ${type}`);
    }
  }
  return unique_by(cells, (d) => d.cell_id);
});

const cells_hierarchy: Readable<d3.HierarchyNode<Cell>> = derived(
  cells,
  ($cells) => {
    log(`Computing cells hierarchy...`, $cells);
    const stratifier = d3
      .stratify<Cell>()
      .id((d: Cell) => d.cell_id)
      .parentId((d: Cell & { parent_cell_id?: CellID }) => d.parent_cell_id);
    const cells_with_root: Cell[] = [
      {
        type: `root`,
        cell_id: `root`,
        parent_cell_id: undefined,
      },
      ...$cells,
    ];
    const hierarchy = stratifier(cells_with_root);
    return hierarchy;
  }
);

cells_hierarchy.subscribe((hierarchy) => log(`Cells hierarchy:`, hierarchy));

const cell_hierarchy_node = derived(cells_hierarchy, ($hierarchy) => ({
  get(cell_id: CellID): d3.HierarchyNode<Cell> {
    const found = $hierarchy.find((d) => d.data.cell_id === cell_id);
    if (found === undefined) {
      throw new Error(`No cell found for cell_id: ${cell_id}`);
    }
    return found;
  },
}));

const parent_cell_id = derived(cell_hierarchy_node, ($hierarchy_node) => ({
  get(cell_id: CellID): CellID {
    const found = $hierarchy_node.get(cell_id);
    if (!found) return null;
    return found.data.parent_cell_id;
  },
}));

export const cells_depth_first = derived(cells_hierarchy, ($hierarchy) => {
  return get_nodes_depth_first($hierarchy);
});

export const catalog_id: Readable<{ get(cell_id: CellID): string }> = derived(
  cell_hierarchy_node,
  ($cell_hierarchy_node) => {
    return {
      get(cell_id: CellID) {
        const found = $cell_hierarchy_node.get(cell_id);
        if (!found) return null;
        let catalog_id = undefined;
        if (found.data.type === `catalog`) {
          catalog_id = found.data.catalog_id;
        } else {
          // Traverse parents until a catalog name is found
          const ancestor: d3.HierarchyNode<Cell> = find_parent_node_by_filter(
            found,
            (d) => d.data.type === `catalog`
          );
          if (!ancestor) {
            throw new Error(
              `No catalog ancestor found for cell_id: ${cell_id}`
            );
          }
          if (ancestor.data.type !== `catalog`) {
            throw new Error(
              `Expected ancestor to be a catalog, but was ${ancestor.data.type}`
            );
          }
          catalog_id = ancestor.data.catalog_id;
        }
        if (catalog_id === undefined) {
          throw new Error(`No catalog ID for cell_id: ${cell_id}`);
        }
        return catalog_id;
      },
    };
  }
);

const catalog_metadata_query = derived(
  cells,
  ($cells, set, update) => {
    update((prev) => {
      const next = new Map(prev);
      const catalog_cells = $cells.filter(
        (cell): cell is CatalogCell => cell.type === `catalog`
      );
      for (const { catalog_id } of catalog_cells) {
        if (next.has(catalog_id)) {
          // log(`Query for ${catalog_id} already exists`);
          continue;
        }
        log(`Creating query for ${catalog_id}...`);
        const observer = create_query_observer<CatalogResponse>({
          staleTime: Infinity,
          queryKey: ["catalog_metadata", { catalog: catalog_id }],
          queryFn: (): Promise<CatalogResponse> =>
            fetch_api_get<CatalogResponse>(`/${catalog_id}`),
        });
        const unsubscribe = observer.subscribe((result) => {
          // log(`Query for ${catalog_id} result:`, result);
          if (result.status === `success`) {
            // log(`Query for ${catalog_id} succeeded`);
            update((prev) => new Map(prev).set(catalog_id, result));
            unsubscribe();
            observer.destroy();
          }
        });
        const current = observer.getCurrentResult();
        next.set(catalog_id, current);
      }
      return next;
    });
  },
  new Map<string, QueryObserverResult<CatalogResponse>>()
);

export const catalog_metadata = derived(
  catalog_metadata_query,
  ($catalog_metadata_query, set, update) => {
    update((prev) => {
      const next = new Map(prev);
      for (const [catalog_id, query] of $catalog_metadata_query) {
        if (next.has(catalog_id)) {
          continue;
        }
        if (query.data) {
          const wrapped = wrap_catalog_response(query.data);
          next.set(catalog_id, wrapped);
        }
      }
      return next;
    });
  },
  new Map<string, CatalogMetadataWrapper>()
);

export const field_metadata = derived(
  [catalog_metadata],
  ([$catalog_metadata]) => {
    return {
      get(catalog_id: string, field_id: string): CatalogHierarchyNode {
        const catalog_field_hierarchy: CatalogHierarchyNode | undefined =
          $catalog_metadata.get(catalog_id)?.hierarchy;
        if (!catalog_field_hierarchy) {
          throw new Error(`Could not find catalog metadata for ${catalog_id}`);
        }
        const field_metadata = catalog_field_hierarchy.find(
          (node) => get_field_id(node.data) === field_id
        );
        if (!field_metadata) {
          throw new Error(`Could not find field metadata for ${field_id}`);
        }
        return field_metadata;
      },
    };
  }
);

export const column_ids = derived(
  [catalog_id, catalog_metadata, actions_by_cell_id],
  ([$catalog_id, $catalog_metadata, $actions_by_cell_id]) => {
    return {
      get(cell_id: TableCellID | PlotCellID): Set<string> {
        const catalog_id = $catalog_id.get(cell_id);
        const catalog_metadata = $catalog_metadata.get(catalog_id);
        if (!catalog_metadata) return new Set();
        const actions = $actions_by_cell_id.get(cell_id);
        const column_list_actions = actions.filter(
          (action): action is ColumnListAction =>
            action.type === `add_column` || action.type === `remove_column`
        );
        const column_ids_set: Set<string> = new Set(
          catalog_metadata.initial_column_ids
        );
        for (const action of column_list_actions) {
          if (action.type === `remove_column`) {
            column_ids_set.delete(action.column_id);
          } else if (action.type === `add_column`) {
            column_ids_set.add(action.column_id);
          } else {
            throw new Error(`unknown filter action type: ${action.type}`);
          }
        }
        return column_ids_set;
      },
    };
  }
);

const filter_ids: Readable<{
  get(cell_id: CellID): Set<string>;
}> = derived(
  [catalog_id, catalog_metadata, actions_by_cell_id],
  ([$catalog_id, $catalog_metadata, $actions_by_cell_id]) => {
    // log(`actions by actions_by_cell_id`, $actions_by_cell_id);
    return {
      get(cell_id: CellID): Set<string> {
        const catalog_id = $catalog_id.get(cell_id);
        // const catalog_metadata_query =
        // $catalog_metadata_queries_by_catalog_id[catalog_id];
        const catalog_metadata = $catalog_metadata.get(catalog_id);
        if (!catalog_metadata) return new Set();
        const actions_for_cell = $actions_by_cell_id.get(cell_id);
        const filter_list_actions = actions_for_cell.filter(
          (action): action is FilterListAction => {
            return (
              action.type === `add_filter` || action.type === `remove_filter`
            );
          }
        );
        const filter_set: Set<string> = new Set(
          catalog_metadata?.initial_filter_ids ?? []
        );
        for (const action of filter_list_actions) {
          if (action.type === `remove_filter`) {
            filter_set.delete(action.filter_id);
          } else if (action.type === `add_filter`) {
            filter_set.add(action.filter_id);
          } else {
            throw new Error(`unknown filter action type: ${action.type}`);
          }
        }
        return filter_set;
      },
    };
  }
);

export const filter_values: Readable<{ get(cell_id: FilterCellID): Filters }> =
  derived(
    [catalog_id, catalog_metadata, filter_ids, filter_state],
    ([$catalog_id, $catalog_metadata, $filter_ids, $filter_state]) => {
      return {
        get(cell_id: FilterCellID) {
          const catalog_id = $catalog_id.get(cell_id);
          const hierarchy = $catalog_metadata.get(catalog_id)?.hierarchy;
          if (!hierarchy) {
            return {};
          }
          const filter_set: Set<string> = $filter_ids.get(cell_id);
          const initial_filters: Filters = get_initial_cell_filters(
            filter_set,
            hierarchy
          );
          const filter_state_for_cell = $filter_state[cell_id] ?? {};
          const filters: Filters = {};
          for (const [field_id, initial_value] of Object.entries(
            initial_filters
          )) {
            const filter_state = filter_state_for_cell[field_id];
            const value = filter_state ?? initial_value;
            filters[field_id] = value;
          }
          return filters;
        },
      };
    }
  );

const debounced_filter_values_store = debounce_store(filter_values, 500);

export const query_config = derived(
  [catalog_id, parent_cell_id, debounced_filter_values_store, column_ids],
  ([$catalog_id, $parent_cell_id, $filter_values, $column_ids]) => {
    return {
      get(cell_id: TableCellID | PlotCellID) {
        const catalog_id = $catalog_id.get(cell_id);

        const parent_cell_id: FilterCellID = is_filter_cell_id(
          $parent_cell_id.get(cell_id)
        );

        const filters = $filter_values?.get(parent_cell_id);

        const column_ids_set: Set<string> = $column_ids?.get(cell_id);
        const column_ids = Array.from(column_ids_set);

        const request_body: DataRequestBody = {
          object: true,
          count: 100,
          fields: column_ids,
          ...filters,
        };

        const query_config = {
          path: `/${catalog_id}/data`,
          body: request_body,
        };
        return query_config;
      },
    };
  }
);

export const data_queries: Readable<
  Record<TableCellID | PlotCellID, QueryObserverResult<DataResponse>>
> = subscribe_to_many(
  derived(
    [cells, query_config],
    ([$cells, $query_config_by_cell_id]) => {
      const obj: Record<
        TableCellID | PlotCellID,
        QueryObserver<DataResponse>
      > = {};
      for (const { type, cell_id } of $cells) {
        if (type !== `table` && type !== `plot`) continue;
        const query_config = $query_config_by_cell_id.get(cell_id);
        const observer = create_query_observer<DataResponse>({
          staleTime: Infinity,
          enabled: false,
          queryKey: [`data`, query_config],
          queryFn: async (): Promise<DataResponse> => {
            return fetch_api_post<DataResponse>(
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
    {} as Record<TableCellID | PlotCellID, QueryObserver<DataResponse>>
  )
);

// =========================================
// FUNCTIONS

function store_data_in_url<T>(data: T, key: string) {
  const compressed = compress_data(data);
  const url = new URL(window.location.href);
  url.searchParams.set(key, compressed);
  window.history.replaceState({}, ``, url.toString());
  const url_length = url.toString().length;
  // log(`URL Length:`, url_length);
  if (url_length > 2000) {
    throw new Error(`URL is too long!`);
  }
}

function compress_data<T>(data: T): string {
  const compressed = lzstring.compressToEncodedURIComponent(
    JSON.stringify(data)
  );
  return compressed;
}

function decompress_data<T>(compressed: string): T {
  const restored = JSON.parse(
    lzstring.decompressFromEncodedURIComponent(compressed)
  );
  return restored;
}

function get_initial_cell_filters(
  filter_ids: Set<string>,
  catalog_field_hierarchy?: d3.HierarchyNode<FieldMetadata>
): Filters {
  if (!catalog_field_hierarchy) return {};
  const initial_filter_object: Filters = Object.fromEntries(
    Array.from(filter_ids).map((filter_id) => {
      const metadata = catalog_field_hierarchy.find(
        (node) => get_field_id(node.data) === filter_id
      )?.data;
      if (!metadata) {
        throw new Error(`Could not find metadata for filter ${filter_id}`);
      }
      const initial_value: Filters[string] = (() => {
        const type = metadata.type;
        if (type === `boolean`) {
          return false;
        } else if (metadata.terms) {
          return 0;
        } else if (type === `byte`) {
          return 0;
        } else if ([`float`, `short`].includes(type)) {
          if (!metadata.stats) {
            throw new Error(
              `Trying to use float filter without stats: ${filter_id}`
            );
          }
          if (
            metadata.stats.min === null ||
            metadata.stats.max === null ||
            !Number.isFinite(metadata.stats.min) ||
            !Number.isFinite(metadata.stats.max)
          ) {
            log(`meta`, metadata);
            throw new Error(`Missing min/max for float filter: ${filter_id}`);
          }
          return {
            gte: metadata.stats.min,
            lte: metadata.stats.max,
          };
        } else {
          return `unknown`;
          log(`meta`, metadata);
          throw new Error(`Unexpected filter type: ${type}`);
        }
      })();
      return [filter_id, initial_value];
    })
  );
  return initial_filter_object;
}

function wrap_catalog_response(
  metadata: CatalogResponse
): CatalogMetadataWrapper {
  const catalog_fields_raw = metadata.fields ?? null;
  const root = {
    sub: catalog_fields_raw,
    __is_root: true,
    __id: `root`,
  } as FieldMetadata;
  const hierarchy: d3.HierarchyNode<FieldMetadata> =
    d3.hierarchy<FieldMetadata>(root, (d) => d?.sub ?? []);
  const nodes = get_nodes_depth_first<FieldMetadata>(hierarchy)
    .filter((d) => !d.data.__is_root)
    .map((d) => {
      set_field_id(d.data);
      return d;
    });
  const nodes_by_id = new Map<string, d3.HierarchyNode<FieldMetadata>>();
  for (const node of nodes) {
    const id = get_field_id(node.data);
    if (nodes_by_id.has(id)) {
      log(`Node:`, node);
      console.error(`Duplicate node ID: ${id}`);
    }
    nodes_by_id.set(id, node);
  }
  const initial_filter_ids = nodes
    .filter((node) => node.height === 0 && `required` in node.data)
    .map((node) => get_field_id(node.data));
  const initial_column_ids = nodes
    .filter((node) => node.height === 0 && node.data.disp === true)
    .map((node) => get_field_id(node.data));
  return {
    metadata,
    hierarchy,
    nodes,
    nodes_by_id,
    initial_filter_ids,
    initial_column_ids,
  };
}

function subscribe_to_many<T>(
  store: Readable<Record<string, any>>,
  debug?: string
): Readable<Record<string, T>> {
  return derived(
    store,
    ($store, set, update) => {
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
    },
    {}
  );
}

function create_query_observer<T>(options: QueryObserverOptions) {
  log(`🐛 Creating Query Observer:`, options.queryKey, options);
  const defaulted_options = query_client.defaultQueryOptions(options);
  const observer = new QueryObserver(query_client, defaulted_options);
  return observer as QueryObserver<T>;
}

async function fetch_api_get<T>(path: string): Promise<T> {
  const url = new URL(`/api${path}`, FLATHUB_API_BASE_URL);
  log(`💥 Fetching`, url.toString());
  const response = await fetch(url.toString(), {
    method: `GET`,
    headers: new Headers({
      "Content-Type": `application/json`,
    }),
  });
  if (!response.ok) {
    throw new Error(`API Fetch Error: ${response.status}`);
  }
  const json: T = await response.json();
  return json;
}

function fetch_api_post<T>(
  path: string,
  body?: Record<string, any>
): Promise<T> {
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

function get_nodes_depth_first<T>(
  root: d3.HierarchyNode<T>
): d3.HierarchyNode<T>[] {
  const nodes: d3.HierarchyNode<T>[] = [];
  root.eachBefore((d) => nodes.push(d));
  return nodes;
}

function unique_by<T>(arr: T[], key: (t: T) => any): T[] {
  const seen = new Set();
  return arr.filter((item) => {
    const k = key(item);
    return seen.has(k) ? false : seen.add(k);
  });
}

function debounce_store<T>(store: Readable<T>, delay: number): Readable<T> {
  return derived(
    store,
    ($store, set) => {
      const timeout = setTimeout(() => {
        set($store);
      }, delay);
      return () => {
        clearTimeout(timeout);
      };
    },
    get(store)
  );
}
