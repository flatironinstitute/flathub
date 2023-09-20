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

import { QueryObserver } from "@tanstack/query-core";
import * as d3 from "d3";
import { readable, writable, derived, get } from "svelte/store";
import * as lzstring from "lz-string";
import { fetch_api_get, get_field_type, is_catalog_cell_id, log } from "./shared";

const initial_actions: Action.Any[] = [];

export const actions = writable<Action.Any[]>(initial_actions);

export const filter_state = writable<GlobalFilterState>(
  {} as GlobalFilterState
);

actions.subscribe((actions) => log(`All actions:`, actions));

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

const actions_by_cell_id: Readable<d3.InternMap<CellID.Any, Action.Any[]>> =
  derived(actions, ($actions) => d3.group($actions, (d) => d.cell_id));

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
        depth_first.push(d);
        get_field_type(d.data)
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

function get_node_label(node: CatalogHierarchyNode) {
  let label;
  if (is_root_node(node)) {
    label = `root`;
  } else if (node.data.name && node.data.name.length > 0) {
    label = node.data.name;
  } else if (node.data.title && node.data.title.length > 0) {
    label = node.data.title;
  }
  if (!label) {
    console.error(`Could not find label for node:`, node);
    throw new Error(`Could not find label for node: ${node}`);
  }
  return label;
}

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

// export const filters_by_cell_id = readable(new Map());

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

          // log(`filter_ids_set`, {
          //   filter_ids_set,
          //   catalog_hierarchy,
          //   cell_actions,
          //   initial_filters
          // });

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
    (action): action is Action.FilterList => {
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
            lte: metadata.stats.max
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
