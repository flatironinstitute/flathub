import type {
  Action,
  Cell,
  CellAction,
  CellID,
  CatalogMetadataWrapper,
  Filters,
  DataRequestBody,
  DataResponse,
  CatalogHierarchyNode,
  CatalogMetadataQuery,
  FieldGroup,
  CatalogResponse,
} from "./types";
import type {
  QueryObserverResult,
  QueryObserverOptions,
} from "@tanstack/query-core";
import type { Readable, Writable } from "svelte/store";

import { QueryClient } from "@tanstack/react-query";
import { QueryObserver } from "@tanstack/query-core";

import * as d3 from "d3";
import { writable, derived, get } from "svelte/store";

import { log } from "./shared";

const query_client = new QueryClient();

export const actions = writable<Action[]>([
  { type: `add_query_cell`, catalog_name: `camels`, cell_id: `query_cell_1` },
  { type: `add_query_cell`, catalog_name: `camels`, cell_id: `query_cell_2` },
  { type: `add_query_cell`, catalog_name: `camels`, cell_id: `query_cell_3` },
]);

const actions_by_type_store = derived(actions, ($actions) => {
  const actions_by_type = d3.group(
    $actions,
    (d) => d.type,
    (d) => d.cell_id
  );
  return Object.fromEntries(actions_by_type);
});

const actions_by_cell_id_store = derived(actions, ($actions) => {
  const actions_by_cell_id = d3.group(
    $actions,
    (d) => d.cell_id,
    (d) => d.type
  );
  return actions_by_cell_id;
});

export const filter_state = writable<
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

export const cells: Readable<Cell[]> = derived(actions, ($actions) => {
  const cell_actions = $actions.filter((action): action is CellAction =>
    [`add_query_cell`].includes(action.type)
  );
  const cells: Cell[] = [];
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

const catalog_name: Readable<{ get(cell_id: CellID): string }> = derived(
  cells,
  ($cells) => {
    const cell_id_to_catalog_name = new Map<CellID, string>();
    for (const cell of $cells) {
      cell_id_to_catalog_name.set(cell.id, cell.catalog_name);
    }
    return {
      get(cell_id: CellID) {
        const catalog_name = cell_id_to_catalog_name.get(cell_id);
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

const catalog_metadata: Readable<{
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
  [catalog_name, catalog_metadata],
  ([$catalog_name, $catalog_metadata]) => {
    return {
      get(cell_id: CellID): string[] {
        const catalog_name = $catalog_name.get(cell_id);
        const catalog_metadata = $catalog_metadata.get(catalog_name);
        if (!catalog_metadata) return [];
        const column_names = get_catalog_initial_column_names(catalog_metadata);
        return column_names;
      },
    };
  }
);

const filter_set_store: Readable<{
  get(cell_id: CellID): Set<string>;
}> = derived(
  [
    catalog_name,
    catalog_metadata_queries_by_catalog_name,
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

export const fitler_values: Readable<{ get(cell_id: CellID): Filters }> =
  derived(
    [catalog_name, catalog_metadata, filter_set_store, filter_state],
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
}> = debounce_store(fitler_values, 500);

export const query_config = derived(
  [catalog_name, debounced_filter_values_store, column_names],
  ([$catalog_name, $filter_values, $column_names]) => {
    return {
      get(cell_id: CellID) {
        const catalog_name = $catalog_name.get(cell_id);

        const filters = $filter_values?.get(cell_id);
        const column_names = $column_names?.get(cell_id);

        const request_body: DataRequestBody = {
          object: true,
          count: 100,
          fields: column_names,
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

export const data_queries: Readable<
  Record<CellID, QueryObserverResult<DataResponse>>
> = subscribe_to_many(
  derived(
    [cells, query_config],
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
  )
);

// =========================================
// FUNCTIONS

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
