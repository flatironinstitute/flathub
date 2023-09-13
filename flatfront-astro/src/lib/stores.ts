import type {
  Action,
  CatalogMetadataWrapper,
  CatalogResponse,
  Cell,
  CellAction,
  CellID,
  Filters,
  FilterValueRaw,
  TopResponse,
  GlobalFilterState,
  Actions,
  QueryParameters
} from "./types";
import type { QueryObserverResult } from "@tanstack/query-core";
import type { Readable } from "svelte/store";

import { QueryObserver } from "@tanstack/query-core";
import * as d3 from "d3";
import { readable, writable, derived, get } from "svelte/store";
import * as lzstring from "lz-string";

import {
  log,
  create_query_observer,
  fetch_api_get,
  get_initial_cell_filters,
  wrap_catalog_response,
  get_final_filters,
  get_filter_ids,
  get_column_ids
} from "./shared";

export const top_response: Readable<QueryObserverResult<TopResponse>> =
  readable<QueryObserverResult<TopResponse>>(null, (set) => {
    const observer: QueryObserver<TopResponse> = create_query_observer({
      staleTime: Infinity,
      queryKey: ["top"],
      queryFn: async (): Promise<TopResponse> => fetch_api_get<TopResponse>(`/`)
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
  // {
  //   type: "add_catalog_cell",
  //   cell_id: "catalog_cell_gr8",
  //   catalog_id: "gr8",
  // },
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

export const filter_state = writable<GlobalFilterState>(
  {} as Record<CellID, Record<string, FilterValueRaw>>
);

(function set_app_state_from_url() {
  const actions_from_url: Action[] = get_data_from_url<Action[]>(`actions`);
  const filters_from_url: GlobalFilterState =
    get_data_from_url<GlobalFilterState>(`filters`);
  if (actions_from_url) {
    log(`Setting actions from URL:`, actions_from_url);
    actions.set(actions_from_url);
  }
  if (filters_from_url) {
    log(`Setting filters from URL:`, filters_from_url);
    filter_state.set(filters_from_url);
  }
})();

actions.subscribe((actions) => log(`All actions:`, actions));

debounce_store(actions, 500).subscribe((actions) => {
  store_data_in_url(actions, `actions`);
});

debounce_store(filter_state, 500).subscribe((filters) => {
  store_data_in_url(filters, `filters`);
});

export const actions_by_cell_id: Readable<d3.InternMap<CellID, Action[]>> =
  derived(actions, ($actions) => d3.group($actions, (d) => d.cell_id));

export const cells: Readable<Cell[]> = derived(actions, ($actions) => {
  const cell_action_types: CellAction["type"][] = [
    `add_cell`,
    `add_table_cell`,
    `add_plot_cell`,
    `remove_table_cell`,
    `remove_plot_cell`
  ];
  // Filter cells to only include those with type in cell_action_types.
  const cell_actions = $actions.filter((action): action is CellAction =>
    cell_action_types.some((type) => action.type === type)
  );
  let cells: Cell[] = [];
  for (const action of cell_actions) {
    const action_type = action.type;
    switch (action_type) {
      case `add_table_cell`: {
        cells.push({
          type: `table`,
          cell_id: action.cell_id,
          parent_cell_id: action.parent_cell_id
        });
        break;
      }
      case `add_plot_cell`: {
        cells.push({
          type: `plot`,
          cell_id: action.cell_id,
          parent_cell_id: action.parent_cell_id
        });
        break;
      }
      case `remove_table_cell`:
      case `remove_plot_cell`: {
        // Remove the table cell with the given cell_id.
        cells = cells.filter((cell) => cell.cell_id !== action.cell_id);
        break;
      }
      case `add_cell`: {
        const cell: Cell = {
          type: `cell`,
          cell_id: action.cell_id,
          parent_cell_id: `root`
        };
        cells.push(cell);
        break;
      }
      default: {
        action_type satisfies never;
        throw new Error(`Unrecognized cell action type ${action_type}`);
      }
    }
  }
  // Remove orphan cells
  let need_to_prune = true;
  while (need_to_prune) {
    need_to_prune = false;
    const all_cell_ids = new Set(cells.map((cell) => cell.cell_id));
    cells = cells.filter((cell) => {
      if (cell.parent_cell_id === undefined) return true;
      if (cell.parent_cell_id === `root`) return true;
      if (all_cell_ids.has(cell.parent_cell_id)) return true;
      need_to_prune = true;
      return false;
    });
  }
  cells = unique_by(cells, (d) => d.cell_id);
  return cells;
});

export const catalog_id_by_cell_id: Readable<Map<CellID, string>> = derived(
  actions_by_cell_id,
  ($actions_by_cell_id) => {
    return new Map<CellID, string>(
      [...$actions_by_cell_id.entries()].map(([cell_id, actions]) => {
        const catalog_id = actions
          .filter(
            (action): action is Actions[`SetCatalog`] =>
              action.type === `set_catalog`
          )
          .at(-1)?.catalog_id;
        return [cell_id, catalog_id];
      })
    );
  },
  new Map<CellID, string>()
);

export const catalog_metadata_query_observer_by_catalog_id: Readable<
  Map<string, QueryObserver<CatalogResponse>>
> = derived(
  catalog_id_by_cell_id,
  ($catalog_id_by_cell_id, set, update) => {
    const catalog_ids = Array.from(
      new Set($catalog_id_by_cell_id.values())
    ).filter((d) => d);

    update((prev) => {
      const next = new Map(prev);
      for (const catalog_id of catalog_ids) {
        if (next.has(catalog_id)) {
          continue;
        }
        log(`Creating query for ${catalog_id}...`);
        const observer = create_query_observer<CatalogResponse>({
          staleTime: Infinity,
          queryKey: ["catalog_metadata", { catalog_id }],
          queryFn: (): Promise<CatalogResponse> =>
            fetch_api_get<CatalogResponse>(`/${catalog_id}`)
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
        observer
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
    filter_state
  ],
  ([
    $catalog_id_by_cell_id,
    $catalog_metadata_wrapper_by_catalog_id,
    $actions_by_cell_id,
    $filter_state
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

export const query_parameters_by_cell_id: Readable<
  Map<CellID, QueryParameters>
> = derived(
  [cells, filter_state],
  ([$cells, $filter_state]) => {
    return new Map(
      $cells.map((cell) => {
        const cell_id = cell.cell_id;
        const cell_type = cell.type;
        if (cell_type === "cell") return [cell_id, undefined];

        const initial_query_parameters: QueryParameters = {
          count: cell_type === `table` ? 25 : 2000
        };

        const filter_state = ($filter_state?.[cell_id] ??
          {}) as QueryParameters;

        const filters = get_final_filters<QueryParameters>(
          initial_query_parameters,
          filter_state
        );

        return [cell_id, filters];
      })
    );
  },
  new Map<CellID, QueryParameters>()
);

export const column_ids_by_cell_id: Readable<Map<CellID, Set<string>>> =
  derived(
    [
      catalog_id_by_cell_id,
      catalog_metadata_wrapper_by_catalog_id,
      actions_by_cell_id
    ],
    ([
      $catalog_id_by_cell_id,
      $catalog_metadata_wrapper_by_catalog_id,
      $actions_by_cell_id
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

function get_data_from_url<T>(key: string): T | undefined {
  const url = new URL(window.location.href);
  const compressed = url.searchParams.get(key);
  if (compressed && compressed.length > 0) {
    const data = decompress_data<T>(compressed);
    return data;
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
