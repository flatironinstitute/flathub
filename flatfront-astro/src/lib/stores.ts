import type { QueryObserverResult } from "@tanstack/react-query";
import type { Readable } from "svelte/store";
import type {
  Action,
  CatalogHierarchyNode,
  CatalogMetadataWrapper,
  CatalogResponse,
  FieldMetadata,
  GlobalFilterState
} from "./types";

import * as d3 from "d3";
import { writable, derived, get } from "svelte/store";
import * as lzstring from "lz-string";
import { get_field_type, log } from "./shared";

const initial_actions: Action.Any[] = [];

export const actions = writable<Action.Any[]>(initial_actions, (set) => {
  const actions = get_data_from_url<Action.Any[]>(`actions`);
  if (actions) {
    log(`Setting actions from URL:`, actions);
    set(actions);
  }
});

export function dispatch_action(action: Action.Any) {
  actions.update(($actions) => [...$actions, action]);
}

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
  if (actions.length === 0) return;
  store_data_in_url(actions, `actions`);
});

debounce_store(filter_state, 500).subscribe((filters) => {
  if (Object.keys(filters).length === 0) return;
  store_data_in_url(filters, `filters`);
});

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

function tiny_json_hash(object: any) {
  const text = JSON.stringify(object);
  let hash = 5381;
  let index = text.length;
  while (index) {
    hash = (hash * 33) ^ text.charCodeAt(--index);
  }
  return (hash >>> 0).toString(16);
}

// =========================================
// FUNCTIONS

function store_data_in_url<T>(data: T, key: string) {
  log(`Storing ${key} data in URL:`, data);
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
