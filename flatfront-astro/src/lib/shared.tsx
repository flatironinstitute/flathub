import type { Readable } from "svelte/store";
import type {
  Action,
  CatalogHierarchyNode,
  Cell,
  CellID,
  FieldMetadata,
  FieldType,
  FilterValueRaw,
  Filters
} from "./types";

import React from "react";
import * as d3 from "d3";
import { get } from "svelte/store";
import * as immer from "immer";

import * as stores from "./stores";

const FLATHUB_API_BASE_URL = `https://flathub.flatironinstitute.org`;

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

function useCatalogID(): string {
  const catalog_cell_id = hooks.useCell().cell_id;
  assert_catalog_cell_id(catalog_cell_id);
  const catalog_id = hooks
    .useStore(stores.catalog_id_by_cell_id)
    .get(catalog_cell_id);
  return catalog_id;
}

// A react hook that takes a ref and a delay time and returns a boolean. the boolean is true when the element has been visible on the screen for the delay time.
function useDelayVisible(
  ref: React.RefObject<HTMLElement>,
  delay: number
): boolean {
  const [visible, setVisible] = React.useState(false);
  React.useEffect(() => {
    let timeout: number;
    const observer = new IntersectionObserver(
      (entries) => {
        if (entries[0].isIntersecting) {
          timeout = window.setTimeout(() => {
            setVisible(true);
          }, delay);
        }
        if (!entries[0].isIntersecting) {
          clearTimeout(timeout);
        }
      },
      { threshold: 0.5 }
    );
    if (ref.current) {
      observer.observe(ref.current);
    }
    return () => {
      if (ref.current) {
        observer.unobserve(ref.current);
      }
    };
  }, [ref, delay]);
  return visible;
}

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

function useCatalogCellID(): CellID.Catalog {
  const catalog_cell_id = hooks.useCell().cell_id;
  assert_catalog_cell_id(catalog_cell_id);
  return catalog_cell_id;
}

function useFilters(): Filters {
  const catalog_cell_id = useCatalogCellID();
  const filters = hooks
    .useStore(stores.filters_by_cell_id)
    .get(catalog_cell_id);
  return filters;
}

function useFilterValueSetter() {
  const catalog_cell_id = useCatalogCellID();
  const field_node = hooks.useFieldNode();
  const field_id = field_node.data.name;
  const set_filter_value = (filter_value: FilterValueRaw) => {
    stores.filter_state.update((fitler_state_object) => {
      return immer.produce(fitler_state_object, (draft) => {
        draft[catalog_cell_id] = draft[catalog_cell_id] || {};
        draft[catalog_cell_id][field_id] = filter_value;
      });
    });
  };
  return set_filter_value;
}

export const hooks = {
  useStore,
  useCell,
  useFieldNode,
  useDelayVisible,
  useCatalogID,
  useFilters,
  useFilterValueSetter
};

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

export async function fetch_api_post<T, U>(path: string, body: T): Promise<U> {
  const url = new URL(`/api${path}`, FLATHUB_API_BASE_URL);
  log(`💥 Fetching:`, url.toString());
  const response = await fetch(url.toString(), {
    method: `POST`,
    headers: new Headers({
      "Content-Type": `application/json`
    }),
    body: JSON.stringify(body)
  });
  log(`💥 Got Response:`, url.toString());
  if (!response.ok) {
    throw new Error(`API Fetch Error: ${response.status}`);
  }
  const json: U = await response.json();
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

export function has_numeric_field_stats(metadata: FieldMetadata): boolean {
  return (
    metadata.stats &&
    Number.isFinite(metadata.stats.min) &&
    Number.isFinite(metadata.stats.max) &&
    Number.isFinite(metadata.stats.avg) &&
    metadata.stats.min !== metadata.stats.max
  );
}

export function assert_numeric_field_stats(metadata: FieldMetadata) {
  if (!metadata.stats) {
    throw new Error(`Numeric field is missing stats: ${metadata.name}`);
  }
  if (
    !Number.isFinite(metadata.stats.min) ||
    !Number.isFinite(metadata.stats.max) ||
    !Number.isFinite(metadata.stats.avg)
  ) {
    throw new Error(`Numeric field is missing min/max: ${metadata.name}`);
  }
  if (metadata.stats.min === metadata.stats.max) {
    throw new Error(`Numeric field has min === max: ${metadata.name}`);
  }
}

export function assert_numeric_filter_value(
  filter_value: FilterValueRaw
): asserts filter_value is { gte: number; lte: number } {
  if (typeof filter_value !== `object`) {
    throw new Error(`Expected filter state to be an object`);
  }
  if (!(`gte` in filter_value) || !(`lte` in filter_value)) {
    throw new Error(`Expected filter state to have gte andlte properties`);
  }
  const low = filter_value.gte;
  const high = filter_value.lte;
  if (typeof low !== `number` || typeof high !== `number`) {
    throw new Error(
      `Expected filter state to have gte and lte properties of type number`
    );
  }
}

// Given a min, max, and mean, decide if a log scale should be used
export function should_use_log_scale(
  min: number,
  max: number,
  mean: number
): boolean {
  const range = max - min;
  const skewed_min = (mean - min) / range < 0.1;
  const skewed_max = (max - mean) / range < 0.1;
  return skewed_min || skewed_max;
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

export function is_root_node(node: CatalogHierarchyNode): boolean {
  return node.depth === 0;
}

export function is_leaf_node(node: CatalogHierarchyNode): boolean {
  return node.height === 0;
}

export const format = {
  concise: (d) => {
    if (d < 1e4) return d3.format(`,.4~g`)(d);
    return d3.format(`.2~e`)(d);
  },
  commas: (d) => {
    return d3.format(`,`)(d);
  }
};
