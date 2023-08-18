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
  create_query_observer,
  fetch_api_get,
  fetch_api_post,
  get_initial_cell_filters,
  wrap_catalog_response,
  get_nodes_depth_first,
  get_final_filters,
  get_filter_ids,
  get_column_ids,
} from "./shared";

export const top_response: Readable<QueryObserverResult<TopResponse>> =
  readable<QueryObserverResult<TopResponse>>(null, (set) => {
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

const initial_actions: Action[] = [
  {
    type: "add_catalog_cell",
    cell_id: "catalog_cell_gr8",
    catalog_id: "gr8",
  },
  {
    type: "add_catalog_cell",
    cell_id: "catalog_cell_camels",
    catalog_id: "camels",
  },
  {
    type: "add_filter_cell",
    cell_id: "filter_cell_1691622596626",
    parent_cell_id: "catalog_cell_camels",
  },
  {
    type: "add_table_cell",
    cell_id: "table_cell_1691701006344",
    parent_cell_id: "filter_cell_1691622596626",
  },
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

export const actions_by_cell_id: Readable<d3.InternMap<string, Action[]>> =
  derived(actions, ($actions) => d3.group($actions, (d) => d.cell_id));

export const cells: Readable<d3.HierarchyNode<Cell>[]> = derived(
  actions,
  ($actions) => {
    const cell_action_types: CellAction["type"][] = [
      `add_catalog_cell`,
      `add_filter_cell`,
      `add_table_cell`,
      `add_plot_cell`,
      `remove_table_cell`,
      `remove_filter_cell`,
      `remove_plot_cell`,
    ];
    // Filter cells to only include those with type in cell_action_types.
    const cell_actions = $actions.filter((action): action is CellAction =>
      cell_action_types.some((type) => action.type === type)
    );
    let cells: Cell[] = [];
    for (const cell_action of cell_actions) {
      const action_type = cell_action.type;
      if (action_type === `add_catalog_cell`) {
        // If cells already includes a cell with this catalog name, don't add it.
        if (
          cells.some(
            (cell) =>
              cell.type === `catalog` &&
              cell.catalog_id === cell_action.catalog_id
          )
        ) {
          log(
            `Skipping adding duplicate catalog cell ${cell_action.catalog_id}`
          );
          continue;
        }
        cells.push({
          type: `catalog`,
          cell_id: cell_action.cell_id,
          catalog_id: cell_action.catalog_id,
          parent_cell_id: `root`,
        });
      } else if (action_type === `add_filter_cell`) {
        cells.push({
          type: `filter`,
          cell_id: cell_action.cell_id,
          parent_cell_id: cell_action.parent_cell_id,
        });
      } else if (action_type === `add_table_cell`) {
        cells.push({
          type: `table`,
          cell_id: cell_action.cell_id,
          parent_cell_id: cell_action.parent_cell_id,
        });
      } else if (action_type === `add_plot_cell`) {
        cells.push({
          type: `plot`,
          cell_id: cell_action.cell_id,
          parent_cell_id: cell_action.parent_cell_id,
        });
      } else if (
        action_type === `remove_filter_cell` ||
        action_type === `remove_table_cell` ||
        action_type === `remove_plot_cell`
      ) {
        // Remove the table cell with the given cell_id.
        cells = cells.filter((cell) => cell.cell_id !== cell_action.cell_id);
      } else {
        action_type satisfies never;
        throw new Error(`Unrecognized cell action type ${action_type}`);
      }
    }
    // Remove orphan cells
    const all_cell_ids = new Set(cells.map((cell) => cell.cell_id));
    cells = cells.filter((cell) => {
      if (cell.parent_cell_id === undefined) return true;
      if (cell.parent_cell_id === `root`) return true;
      if (all_cell_ids.has(cell.parent_cell_id)) return true;
      return false;
    });
    cells = unique_by(cells, (d) => d.cell_id);
    log(`Computing cells hierarchy...`, cells);
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
      ...cells,
    ];
    const hierarchy: d3.HierarchyNode<Cell> = stratifier(cells_with_root);
    const cell_nodes = get_nodes_depth_first(hierarchy);
    return cell_nodes;
  }
);

export const catalog_id_by_cell_id: Readable<Map<CellID, string>> = derived(
  cells,
  ($cells) => {
    const catalog_id_by_cell_id = new Map<CellID, string>(
      $cells.map((cell) => {
        const cell_id = cell.data.cell_id;
        const cell_type = cell.data.type;
        let catalog_id = undefined;
        if (cell_type === `root`) {
          catalog_id = undefined;
        } else if (cell_type === `catalog`) {
          catalog_id = cell.data.catalog_id;
        } else {
          // Traverse parents until a catalog name is found
          const ancestor: d3.HierarchyNode<Cell> = find_parent_node_by_filter(
            cell,
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
          if (catalog_id === undefined) {
            throw new Error(`No catalog ID for cell_id: ${cell_id}`);
          }
        }
        return [cell_id, catalog_id];
      })
    );
    return catalog_id_by_cell_id;
  },
  new Map<CellID, string>()
);

export const catalog_metadata_query_observer_by_catalog_id: Readable<
  Map<string, QueryObserver<CatalogResponse>>
> = derived(
  cells,
  ($cells, set, update) => {
    const cells = $cells.map((d) => d.data);
    const catalog_cells = cells.filter(
      (cell): cell is CatalogCell => cell.type === `catalog`
    );

    update((prev) => {
      const next = new Map(prev);
      for (const { catalog_id } of catalog_cells) {
        if (next.has(catalog_id)) {
          continue;
        }
        log(`Creating query for ${catalog_id}...`);
        const observer = create_query_observer<CatalogResponse>({
          staleTime: Infinity,
          queryKey: ["catalog_metadata", { catalog_id }],
          queryFn: (): Promise<CatalogResponse> =>
            fetch_api_get<CatalogResponse>(`/${catalog_id}`),
        });
        next.set(catalog_id, observer);
      }
      return next;
    });
  },
  new Map() as Map<string, QueryObserver<CatalogResponse>>
);

export const catalog_metadata_query_by_catalog_id = derived(
  catalog_metadata_query_observer_by_catalog_id,
  ($catalog_metadata_query_observer_by_catalog_id, set, update) => {
    update((prev) => {
      const next = new Map(prev);
      for (const [
        catalog_id,
        observer,
      ] of $catalog_metadata_query_observer_by_catalog_id) {
        if (next.has(catalog_id)) {
          continue;
        }
        const unsubscribe = observer.subscribe((result) => {
          if (result.status === `success`) {
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
  new Map() as Map<string, QueryObserverResult<CatalogResponse>>
);

export const catalog_metadata_wrapper_by_catalog_id = derived(
  catalog_metadata_query_by_catalog_id,
  ($catalog_metadata_query_by_catalog_id, set, update) => {
    update((prev) => {
      const next = new Map(prev);
      for (const [catalog_id, query] of $catalog_metadata_query_by_catalog_id) {
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

export const filters_by_cell_id: Readable<Map<CellID, Filters>> = derived(
  [
    catalog_id_by_cell_id,
    catalog_metadata_wrapper_by_catalog_id,
    actions_by_cell_id,
    filter_state,
  ],
  ([
    $catalog_id_by_cell_id,
    $catalog_metadata_wrapper_by_catalog_id,
    $actions_by_cell_id,
    $filter_state,
  ]) => {
    return new Map(
      [...$catalog_id_by_cell_id.entries()].map(([cell_id, catalog_id]) => {
        const catalog_metadata_wrapper =
          $catalog_metadata_wrapper_by_catalog_id.get(catalog_id);

        const catalog_hierarchy = catalog_metadata_wrapper?.hierarchy;

        const cell_actions = $actions_by_cell_id.get(cell_id);

        const filter_ids_set = catalog_hierarchy
          ? get_filter_ids(catalog_hierarchy, cell_actions)
          : new Set<string>();

        const initial_filters: Filters = get_initial_cell_filters(
          filter_ids_set,
          catalog_hierarchy
        );

        const filter_state: Filters = $filter_state?.[cell_id] ?? {};

        const filters = get_final_filters(initial_filters, filter_state);

        return [cell_id, filters];
      })
    );
  },
  new Map<CellID, Filters>()
);

export const column_ids_by_cell_id: Readable<Map<CellID, Set<string>>> =
  derived(
    [
      catalog_id_by_cell_id,
      catalog_metadata_wrapper_by_catalog_id,
      actions_by_cell_id,
    ],
    ([
      $catalog_id_by_cell_id,
      $catalog_metadata_wrapper_by_catalog_id,
      $actions_by_cell_id,
    ]) => {
      return new Map(
        [...$catalog_id_by_cell_id.entries()].map(([cell_id, catalog_id]) => {
          const catalog_metadata_wrapper =
            $catalog_metadata_wrapper_by_catalog_id.get(catalog_id);

          const catalog_hierarchy = catalog_metadata_wrapper?.hierarchy;

          const cell_actions = $actions_by_cell_id.get(cell_id);

          const column_ids_set = catalog_hierarchy
            ? get_column_ids(catalog_hierarchy, cell_actions)
            : new Set<string>();

          return [cell_id, column_ids_set];
        })
      );
    },
    new Map<CellID, Set<string>>()
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
