import type {
  Action,
  CatalogHierarchyNode,
  CatalogMetadataQuery,
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
import type { Readable, Writable } from "svelte/store";

import { QueryClient } from "@tanstack/query-core";
import { QueryObserver } from "@tanstack/query-core";
import * as d3 from "d3";
import { readable, writable, derived, get } from "svelte/store";
import * as lzstring from "lz-string";

import { log, find_parent_node_by_filter, is_filter_cell_id } from "./shared";

const FLATHUB_API_BASE_URL = `https://flathub.flatironinstitute.org`;

const query_client = new QueryClient();

export const actions = writable<Action[]>([
  {
    type: "add_catalog_cell",
    cell_id: "catalog_cell_camels",
    catalog_name: "camels",
  },
  {
    type: "add_filter_cell",
    cell_id: "filter_cell_1691622596626",
    parent_cell_id: "catalog_cell_camels",
  },
  {
    type: "add_filter",
    cell_id: "filter_cell_1691622596626",
    filter_name: "Group_GasMetalFractions_He",
  },
  {
    type: "add_table_cell",
    cell_id: "table_cell_1691701006344",
    parent_cell_id: "filter_cell_1691622596626",
  },
  // {
  //   type: `add_filter_cell`,
  //   catalog_name: `camels`,
  //   cell_id: `filter_cell_camels_1`,
  // },
  // {
  //   type: `add_filter_cell`,
  //   catalog_name: `camels`,
  //   cell_id: `filter_cell_camels_2`,
  // },
  // {
  //   type: `add_filter_cell`,
  //   catalog_name: `camels`,
  //   cell_id: `filter_cell_camels_3`,
  // },
  // {
  //   type: "set_plot_control",
  //   cell_id: "query_cell_1",
  //   plot_id: "scatterplot_1",
  //   key: "x_axis",
  //   value: "Subhalo_Spin_y",
  // },
  // {
  //   type: "set_plot_control",
  //   cell_id: "query_cell_1",
  //   plot_id: "scatterplot_1",
  //   key: "y_axis",
  //   value: "Subhalo_Pos_x",
  // },
]);

actions.subscribe((actions) => log(`ALL ACTIONS:`, actions));

debounce_store(actions, 1000).subscribe((actions) => {
  log(`DEBOUNCED ACTIONS:`, actions);
  store_data_in_url(actions, `actions`);
});

export const filter_state = writable<
  Record<CellID, Record<string, FilterValueRaw>>
>({} as Record<CellID, Record<string, FilterValueRaw>>);

filter_state.subscribe((filter_state) =>
  log(`Global Filter State:`, filter_state)
);

debounce_store(filter_state, 1000).subscribe((filters) => {
  log(`DEBOUNCED FILTERS:`, filters);
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
    return observer.subscribe((result) => {
      set(result);
    });
  });

top_response.subscribe((result) => log(`Top response:`, result, result.data));

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
  log(`CELL ACTIONS:`, cell_actions);
  let cells: Cell[] = [];
  for (const cell_action of cell_actions) {
    if (cell_action.type === `add_catalog_cell`) {
      // If cells already includes a cell with this catalog name, don't add it.
      if (
        cells.some(
          (cell) =>
            cell.type === `catalog` &&
            cell.catalog_name === cell_action.catalog_name
        )
      ) {
        log(
          `Skipping adding duplicate catalog cell ${cell_action.catalog_name}`
        );
        continue;
      }
      cells.push({
        type: `catalog`,
        cell_id: cell_action.cell_id,
        catalog_name: cell_action.catalog_name,
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
      log(`CELL ACTION: Removed filter cell ${cell_action.cell_id}`);
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

const hierarchy_node = derived(cells_hierarchy, ($hierarchy) => ({
  get(cell_id: CellID): d3.HierarchyNode<Cell> {
    const found = $hierarchy.find((d) => d.data.cell_id === cell_id);
    if (found === undefined) {
      throw new Error(`No cell found for cell_id: ${cell_id}`);
    }
    return found;
  },
}));

const parent_cell_id = derived(hierarchy_node, ($hierarchy_node) => ({
  get(cell_id: CellID): CellID {
    const found = $hierarchy_node.get(cell_id);
    return found.data.parent_cell_id;
  },
}));

export const cells_depth_first = derived(cells_hierarchy, ($hierarchy) => {
  return get_nodes_depth_first($hierarchy);
});

export const catalog_name: Readable<{ get(cell_id: CellID): string }> = derived(
  hierarchy_node,
  ($hierarchy_node) => {
    return {
      get(cell_id: CellID) {
        const found = $hierarchy_node.get(cell_id);
        let catalog_name = undefined;
        if (found.data.type === `catalog`) {
          catalog_name = found.data.catalog_name;
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
          catalog_name = ancestor.data.catalog_name;
        }
        if (catalog_name === undefined) {
          throw new Error(`No catalog name for cell_id: ${cell_id}`);
        }
        return catalog_name;
      },
    };
  }
);

/**
 * Fetch catalog metadata for each catalog in the list of cells.
 */
export const catalog_metadata_queries_by_catalog_name: Readable<
  Record<string, QueryObserverResult<CatalogMetadataWrapper>>
> = subscribe_to_many(
  derived(
    cells,
    ($cells) => {
      const catalog_cells = $cells.filter(
        (cell): cell is CatalogCell => cell.type === `catalog`
      );
      const obj: Record<string, QueryObserver> = {};
      catalog_cells.forEach((cell) => {
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

export const catalog_metadata: Readable<{
  get(catalog_name: string): CatalogMetadataWrapper | null;
}> = derived(
  catalog_metadata_queries_by_catalog_name,
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

export const field_metadata = derived(
  [catalog_metadata],
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

export const column_names = derived(
  [catalog_name, catalog_metadata, actions_by_cell_id],
  ([$catalog_name, $catalog_metadata, $actions_by_cell_id]) => {
    return {
      get(cell_id: TableCellID | PlotCellID): Set<string> {
        const catalog_name = $catalog_name.get(cell_id);
        const catalog_metadata = $catalog_metadata.get(catalog_name);
        if (!catalog_metadata) return new Set();
        const actions = $actions_by_cell_id.get(cell_id);
        const column_list_actions = actions.filter(
          (action): action is ColumnListAction =>
            action.type === `add_column` || action.type === `remove_column`
        );
        const column_name_set: Set<string> = new Set(
          catalog_metadata.initial_column_names
        );
        for (const action of column_list_actions) {
          if (action.type === `remove_column`) {
            column_name_set.delete(action.column_name);
          } else if (action.type === `add_column`) {
            column_name_set.add(action.column_name);
          } else {
            throw new Error(`unknown filter action type: ${action.type}`);
          }
        }
        return column_name_set;
      },
    };
  }
);

const filter_set_store: Readable<{
  get(cell_id: CellID): Set<string>;
}> = derived(
  [catalog_name, catalog_metadata_queries_by_catalog_name, actions_by_cell_id],
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
        const filter_list_actions = actions_for_cell.filter(
          (action): action is FilterListAction => {
            return (
              action.type === `add_filter` || action.type === `remove_filter`
            );
          }
        );
        const filter_set: Set<string> = new Set(
          catalog_metadata?.initial_filter_names ?? []
        );
        for (const action of filter_list_actions) {
          if (action.type === `remove_filter`) {
            filter_set.delete(action.filter_name);
          } else if (action.type === `add_filter`) {
            filter_set.add(action.filter_name);
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
    [catalog_name, catalog_metadata, filter_set_store, filter_state],
    ([$catalog_name, $catalog_metadata, $filter_set, $filter_state]) => {
      return {
        get(cell_id: FilterCellID) {
          const catalog_name = $catalog_name.get(cell_id);
          const hierarchy = $catalog_metadata.get(catalog_name)?.hierarchy;
          if (!hierarchy) {
            return {};
          }
          const filter_set: Set<string> = $filter_set.get(cell_id);
          // log(`filter_set`, cell_id, filter_set);
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

const debounced_filter_values_store = debounce_store(filter_values, 500);

export const query_config = derived(
  [catalog_name, parent_cell_id, debounced_filter_values_store, column_names],
  ([$catalog_name, $parent_cell_id, $filter_values, $column_names]) => {
    return {
      get(cell_id: TableCellID | PlotCellID) {
        const catalog_name = $catalog_name.get(cell_id);

        const parent_cell_id: FilterCellID = is_filter_cell_id(
          $parent_cell_id.get(cell_id)
        );

        const filters = $filter_values?.get(parent_cell_id);

        const column_names_set: Set<string> = $column_names?.get(cell_id);
        const column_names = Array.from(column_names_set);

        const request_body: DataRequestBody = {
          object: true,
          count: 100,
          fields: column_names,
          ...filters,
        };

        const query_config = {
          path: `/${catalog_name}/data`,
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
  log(`URL Length:`, url_length);
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

function get_catalog_initial_column_names(
  catalog_metadata: CatalogMetadataWrapper
) {
  if (!catalog_metadata) {
    return [];
  }
  const initial_column_names = catalog_metadata.initial_column_names;
  const current_column_names = initial_column_names;
  return current_column_names;
}

function get_initial_cell_filters(
  filter_names: Set<string>,
  catalog_field_hierarchy?: d3.HierarchyNode<FieldMetadata>
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
          return false;
        } else if (metadata.terms) {
          return 0;
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
            log(`meta`, metadata);
            throw new Error(`Missing min/max for float filter: ${filter_name}`);
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
      return [filter_name, initial_value];
    })
  );
  return initial_filter_object;
}

async function fetch_catalog_metadata(
  catalog_name: string
): Promise<CatalogMetadataWrapper> {
  const path = `/${catalog_name}`;
  try {
    const metadata: CatalogResponse = await fetch_api_get<CatalogResponse>(
      path
    );
    log(`💥 metadata`, metadata);
    const catalog_fields_raw = metadata.fields ?? null;
    const root = { sub: catalog_fields_raw } as FieldMetadata;
    const hierarchy: d3.HierarchyNode<FieldMetadata> =
      d3.hierarchy<FieldMetadata>(root, (d) => d?.sub ?? []);
    const nodes = get_nodes_depth_first<FieldMetadata>(hierarchy).filter(
      (d) => `name` in d.data
    );
    const nodes_by_name = new Map<string, d3.HierarchyNode<FieldMetadata>>();
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
    throw new Error(`Catalog Metadata Fetch Error: ${err.toString()}`);
  }
}

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

function create_query_observer<T>(options: QueryObserverOptions) {
  const defaulted_options = query_client.defaultQueryOptions(options);
  const observer = new QueryObserver(query_client, defaulted_options);
  return observer as QueryObserver<T>;
}

async function fetch_api_get<T>(path: string): Promise<T> {
  const url = new URL(`/api${path}`, FLATHUB_API_BASE_URL);
  log(`💥 fetching`, url.toString());
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
