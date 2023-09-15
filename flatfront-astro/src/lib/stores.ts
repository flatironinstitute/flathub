import type { Action, Cell, TopResponse } from "./types";
import type { QueryObserverResult } from "@tanstack/query-core";
import type { Readable } from "svelte/store";

import { QueryObserver } from "@tanstack/query-core";
import * as d3 from "d3";
import { readable, writable, derived, get } from "svelte/store";
import * as lzstring from "lz-string";
import { create_query_observer, fetch_api_get, log } from "./shared";

const initial_actions: Action.Any[] = [];

export const actions = writable<Action.Any[]>(initial_actions);

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
