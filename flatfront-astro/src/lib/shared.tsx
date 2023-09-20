import type { QueryObserverOptions } from "@tanstack/query-core";
import type { Readable } from "svelte/store";
import type {
  Action,
  CatalogHierarchyNode,
  Cell,
  CellID,
  FieldMetadata,
  FieldType
} from "./types";

import React from "react";
import { get } from "svelte/store";
import { QueryClient, QueryObserver } from "@tanstack/query-core";

import * as stores from "./stores";

const FLATHUB_API_BASE_URL = `https://flathub.flatironinstitute.org`;

const query_client = new QueryClient();

const [useCell, CellProvider] = useContextHelper<Cell.Any>(`Cell`);
const [useFieldNode, FieldNodeProvider] =
  useContextHelper<CatalogHierarchyNode>(`FieldNode`);
// const [useFieldID, FieldIDProvider] = useContextHelper<string>(`FieldID`);
// const [usePlotID, PlotIDProvider] = useContextHelper<string>(`PlotID`);
// const [useData, DataProvider] = useContextHelper<Datum[]>(`Data`);

export const Providers = {
  CellProvider,
  FieldNodeProvider
  // FieldIDProvider
  // PlotIDProvider,
  // DataProvider
};

export const hooks = {
  useStore,
  useCell,
  useFieldNode
  // useFieldID
};

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

export function dispatch_action(action: Action.Any) {
  stores.actions.update(($actions) => [...$actions, action]);
}

function useContextHelper<T>(debug: string) {
  const context = React.createContext<T | null>(null);
  const useContext = (): T => {
    const value: T | null = React.useContext(context);
    if (value === null) {
      throw new Error(`useContextHelper: ${debug}: value is null`);
    }
    return value;
  };
  return [useContext, context.Provider] as const;
}

// export function create_query_observer<T>(
//   options: QueryObserverOptions<T>
// ): QueryObserver<T> {
//   log(`🐛 Creating Query Observer:`, options.queryKey, options);
//   const defaulted_options = query_client.defaultQueryOptions(options);
//   return new QueryObserver(query_client, defaulted_options);
// }

export async function fetch_api_get<T>(path: string): Promise<T> {
  const url = new URL(`/api${path}`, FLATHUB_API_BASE_URL);
  log(`💥 Fetching:`, url.toString());
  const response = await fetch(url.toString(), {
    method: `GET`,
    headers: new Headers({
      "Content-Type": `application/json`
    })
  });
  log(`💥 Got Response:`, url.toString());
  if (!response.ok) {
    throw new Error(`API Fetch Error: ${response.status}`);
  }
  const json: T = await response.json();
  return json;
}

export function log(...args: any[]) {
  console.log(`🌔`, ...args);
}

export function is_catalog_cell_id(
  cell_id: CellID.Any
): cell_id is CellID.Catalog {
  return cell_id.match(/^catalog_cell_/) ? true : false;
}

export function is_table_cell_id(cell_id: CellID.Any): cell_id is CellID.Table {
  return cell_id.match(/^table_cell_/) ? true : false;
}

export function assert_catalog_cell_id(
  cell_id: CellID.Any
): asserts cell_id is CellID.Catalog {
  if (!is_catalog_cell_id(cell_id)) {
    throw new Error(`${cell_id} is not a catalog cell id`);
  }
}

export function assert_table_cell_id(
  cell_id: CellID.Any
): asserts cell_id is CellID.Table {
  if (!is_table_cell_id(cell_id)) {
    throw new Error(`${cell_id} is not a table cell id`);
  }
}

export function get_field_type(field: FieldMetadata): FieldType {
  const { type, dtype, base } = field;
  const has_enum = field.enum && field.enum.length > 0 ? true : false;
  const has_terms = field.terms ? true : false;
  if (!type) {
    return `ROOT`;
  }
  const type_string = [type, dtype, base, has_terms, has_enum].join(`_`);
  switch (type_string) {
    case `byte_i1_i_false_false`:
    case `short_i2_i_false_false`:
    case `integer_i4_i_false_false`:
    case `long_i8_i_false_false`:
      return `INTEGER`;
    case `float_f4_f_false_false`:
    case `double_f8_f_false_false`:
      return `FLOAT`;
    case `byte_i1_i_true_true`:
      return `LABELLED_ENUMERABLE_INTEGER`;
    case `boolean_?_b_true_true`:
      return `LABELLED_ENUMERABLE_BOOLEAN`;
    case `byte_i1_i_true_false`:
    case `short_i2_i_true_false`:
      return `ENUMERABLE_INTEGER`;
    case `keyword_S8_s_false_false`:
    case `keyword_S32_s_false_false`:
    case `keyword_S16_s_false_false`:
    case `keyword_S20_s_false_false`:
    case `keyword_S8_s_true_false`:
      return `STRING`;
    case `array float_f4_f_false_false`:
    case `array integer_i4_i_false_false`:
    case `array double_f8_f_false_false`:
      return `ARRAY`;
  }
  console.error(`Could not determine field type for: ${field.name}`, field);
}

// const global_field_ids = new Set<string>();

// export function get_field_id(node: CatalogHierarchyNode): string {
//   const metadata = node.data;
//   let id: string;
//   if (is_root_node(node)) return `root`;
//   if (metadata.__id) id = metadata.__id;
//   if (metadata.name && metadata.name.length > 0) id = metadata.name;
//   else if (metadata.title && metadata.title.length > 0) id = metadata.title;
//   if (typeof id === `undefined`) {
//     log(`🐛`, node);
//     throw new Error(`get_field_id: id is undefined`);
//   }
//   // if (maybe === undefined) {
//   //   throw new Error(`get_field_id: node.data.name is undefined`);
//   // }
//   // if (global_field_ids.has(maybe)) {
//   //   throw new Error(`get_field_id: duplicate field id: ${maybe}`);
//   // }
//   // global_field_ids.add(maybe);
//   // log(`how`, maybe)
//   return id;
// }

export function is_root_node(node: CatalogHierarchyNode): boolean {
  return node.depth === 0;
}


