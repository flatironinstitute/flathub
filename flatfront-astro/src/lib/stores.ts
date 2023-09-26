import type { QueryObserverResult } from "@tanstack/query-core";
import type { Readable } from "svelte/store";
import type {
  Action,
  CatalogHierarchyNode,
  CatalogMetadataWrapper,
  CatalogResponse,
  Cell,
  CellID,
  FieldMetadata,
  Filters,
  FilterValueRaw,
  GlobalFilterState,
  TopResponse
} from "./types";

import * as d3 from "d3";
import { readable, writable, derived, get } from "svelte/store";
import * as lzstring from "lz-string";
import {
  assert_numeric_field_stats,
  fetch_api_get,
  get_field_type,
  has_numeric_field_stats,
  is_catalog_cell_id,
  log
} from "./shared";

const initial_actions: Action.Any[] = [];

export const actions = writable<Action.Any[]>(initial_actions, (set) => {
  const actions = get_data_from_url<Action.Any[]>(`actions`);
  if (actions) {
    log(`Setting actions from URL:`, actions);
    set(actions);
  }
});

export const filter_state = writable<GlobalFilterState>(
  {} as GlobalFilterState,
  (set) => {
    const state = get_data_from_url<GlobalFilterState>(`filters`);
    if (state) {
      log(`Setting filter state from URL:`, state);
      set(state);
    }
  }
);

actions.subscribe((actions) => log(`All actions:`, actions));

debounce_store(actions, 500).subscribe((actions) => {
  store_data_in_url(actions, `actions`);
});

debounce_store(filter_state, 500).subscribe((filters) => {
  store_data_in_url(filters, `filters`);
});

export const cells: Readable<Cell.Any[]> = derived(actions, ($actions) => {
  let cells: Cell.Any[] = [];
  for (const action of $actions) {
    switch (action.type) {
      case `add_catalog_cell`:
        cells.push({
          type: `catalog`,
          cell_id: action.cell_id
        });
        break;
      case `add_comparison_cell`:
        cells.push({
          type: `comparison`,
          cell_id: action.cell_id
        });
        break;
      case `add_table_cell`:
        cells.push({
          type: `table`,
          cell_id: action.cell_id,
          catalog_cell_id: action.catalog_cell_id
        });
        break;
      case `add_plot_cell`:
        cells.push({
          type: `plot`,
          cell_id: action.cell_id,
          catalog_cell_id: action.catalog_cell_id
        });
        break;
      case `remove_cell`:
        cells = cells.filter((cell) => cell.cell_id !== action.cell_id);
        break;
    }
  }
  return cells;
});

export const actions_by_cell_id: Readable<
  d3.InternMap<CellID.Any, Action.Any[]>
> = derived(actions, ($actions) => d3.group($actions, (d) => d.cell_id));

export const catalog_id_by_cell_id: Readable<Map<CellID.Catalog, string>> =
  derived(
    actions_by_cell_id,
    ($actions_by_cell_id) => {
      const map = new Map<CellID.Catalog, string>();
      for (const [cell_id, actions] of $actions_by_cell_id) {
        if (!is_catalog_cell_id(cell_id)) continue;
        const catalog_id = actions
          .filter(
            (action): action is Action.SetCatalog =>
              action.type === `set_catalog`
          )
          .at(-1)?.catalog_id;
        if (catalog_id) {
          map.set(cell_id, catalog_id);
        }
      }
      return map;
    },
    new Map<CellID.Catalog, string>()
  );

export const catalog_query_by_catalog_id = writable(
  {} as Record<string, QueryObserverResult<CatalogResponse>>
);

export const catalog_metadata_by_catalog_id: Readable<
  Record<string, CatalogMetadataWrapper>
> = (() => {
  const output = writable<Record<string, CatalogMetadataWrapper>>({});
  catalog_query_by_catalog_id.subscribe((obj) => {
    const value = get(output);
    for (const [catalog_id, query] of Object.entries(obj)) {
      if (catalog_id in value) continue;
      const catalog_response = query.data;
      if (!catalog_response) continue;
      log(`Creating metadata for ${catalog_id}...`);
      const root = {
        sub: catalog_response.fields
      } as FieldMetadata;
      const hierarchy: CatalogHierarchyNode = d3.hierarchy<FieldMetadata>(
        root,
        (d) => d?.sub ?? []
      );
      const depth_first: Array<CatalogHierarchyNode> = [];
      hierarchy.eachBefore((d) => {
        d.data.__hash = tiny_json_hash(d.data);
        if (!is_root_node(d)) depth_first.push(d);
        get_field_type(d.data);
      });
      const wrapper: CatalogMetadataWrapper = {
        response: catalog_response,
        hierarchy,
        depth_first
      };
      output.update((prev) => ({
        ...prev,
        [catalog_id]: wrapper
      }));
    }
  });
  return output;
})();

function is_root_node(node: CatalogHierarchyNode): boolean {
  return node.depth === 0;
}

function tiny_json_hash(object) {
  const text = JSON.stringify(object);
  let hash = 5381;
  let index = text.length;
  while (index) {
    hash = (hash * 33) ^ text.charCodeAt(--index);
  }
  return (hash >>> 0).toString(16);
}

export const filters_by_cell_id: Readable<Map<CellID.Catalog, Filters>> =
  derived(
    [
      catalog_id_by_cell_id,
      catalog_metadata_by_catalog_id,
      actions_by_cell_id,
      filter_state
    ],
    ([
      $catalog_id_by_cell_id,
      $catalog_metadata_by_catalog_id,
      $actions_by_cell_id,
      $filter_state
    ]) => {
      return new Map(
        [...$catalog_id_by_cell_id.entries()].map(([cell_id, catalog_id]) => {
          const catalog_metadata = $catalog_metadata_by_catalog_id[catalog_id];
          const catalog_hierarchy = catalog_metadata?.hierarchy;
          const cell_actions = $actions_by_cell_id.get(cell_id);
          const filter_names_set = catalog_hierarchy
            ? get_filter_names(catalog_hierarchy, cell_actions)
            : new Set<string>();
          const initial_filters: Filters = get_initial_cell_filters(
            filter_names_set,
            catalog_hierarchy
          );
          const filter_state: Filters = $filter_state?.[cell_id] ?? {};
          const filters = {
            ...initial_filters,
            ...filter_state
          };
          return [cell_id, filters];
        })
      );
    },
    new Map<CellID.Catalog, Filters>()
  );

export function get_filter_names(
  hierarchy: d3.HierarchyNode<FieldMetadata>,
  actions: Action.Any[]
) {
  const nodes = hierarchy.descendants();
  const initial_filter_names = nodes
    .filter((node) => node.height === 0 && `required` in node.data)
    .map((node) => node.data.name);
  log(`initial_filter_names`, initial_filter_names);
  const filter_names_set: Set<string> = new Set(initial_filter_names);
  const filter_list_actions = actions.filter(
    (action): action is Action.FilterListAction => {
      return (
        action.type === `add_filter` ||
        action.type === `remove_filter` ||
        action.type === `remove_child_filters`
      );
    }
  );
  for (const action of filter_list_actions) {
    switch (action.type) {
      case `remove_filter`:
        filter_names_set.delete(action.field_id);
        break;
      case `add_filter`:
        filter_names_set.add(action.field_id);
        break;
      case `remove_child_filters`:
        // const node = nodes.find(
        //   (node) => get_field_id(node) === action.field_id
        // );
        // if (!node) {
        //   throw new Error(
        //     `Could not find node for filter ${action.field_id} in hierarchy`
        //   );
        // }
        // const to_remove = node.leaves().map((node) => get_field_id(node));
        // for (const id of to_remove) {
        //   filter_names_set.delete(id);
        // }
        break;
      default:
        action.type satisfies never;
        throw new Error(`Unknown filter action type: ${action.type}`);
    }
  }
  log(`filter_names_set`, filter_names_set);
  return filter_names_set;
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

      const field_type = get_field_type(metadata);

      const initial_value: FilterValueRaw = (() => {
        switch (field_type) {
          case `ROOT`:
            throw new Error(`Root field should not be a filter`);
          case `INTEGER`:
          case `FLOAT`:
            if (!has_numeric_field_stats(metadata)) {
              throw new Error(
                `Field ${metadata.name} does not have numeric field stats`
              );
            }
            assert_numeric_field_stats(metadata);
            return {
              gte: metadata.stats.min,
              lte: metadata.stats.max
            };
          case `LABELLED_ENUMERABLE_BOOLEAN`:
            return false;
          case `LABELLED_ENUMERABLE_INTEGER`:
          case `ENUMERABLE_INTEGER`:
            return 0;
          case `ARRAY`:
            return null;
          case `STRING`:
            return ``;
          default:
            field_type satisfies never;
        }
      })();

      return [filter_name, initial_value];
    })
  );
  return initial_filter_object;
}

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

function get_data_from_url<T>(key: string): T | undefined {
  const url = new URL(window.location.href);
  const compressed = url.searchParams.get(key);
  if (compressed && compressed.length > 0) {
    const data = decompress_data<T>(compressed);
    return data;
  }
  return undefined;
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
