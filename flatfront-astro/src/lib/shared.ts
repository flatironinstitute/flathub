import type {
  CatalogHierarchyNode,
  CellID,
  FieldMetadata,
  FieldType,
  FilterValueRaw,
  PlotID
} from "./types";

import React from "react";
import * as d3 from "d3";

export const FLATHUB_API_BASE_URL = `https://flathub.flatironinstitute.org`;

export async function fetch_api_get<T>(path: string): Promise<T> {
  const url = new URL(`/api${path}`, FLATHUB_API_BASE_URL);
  log(`💥 Fetching:`, url.toString());
  const response = await fetch(url.toString(), {
    method: `GET`,
    headers: new Headers({
      "Content-Type": `application/json`
    })
  });
  if (!response.ok) {
    throw new Error(`API Fetch Error: ${response.status}`);
  }
  const json: T = await response.json();
  log(`💥 Got Response:`, url.toString(), json);
  return json;
}

export async function fetch_api_post<T, U>(
  path: string,
  body: T,
  options: RequestInit = {}
): Promise<U> {
  const url = new URL(`/api${path}`, FLATHUB_API_BASE_URL);
  log(`💥 Fetching:`, url.toString(), body);
  const response = await fetch(url.toString(), {
    method: `POST`,
    headers: new Headers({
      "Content-Type": `application/json`
    }),
    body: JSON.stringify(body),
    ...options
  });
  if (!response.ok) {
    throw new Error(`API Fetch Error: ${response.status}`);
  }
  const json: U = await response.json();
  log(`💥 Got Response:`, url.toString(), json);
  return json;
}

export function log(...args: any[]) {
  console.log(`🌔`, ...args);
}

export function is_catalog_cell_id(
  cell_id: CellID.Any | null
): cell_id is CellID.Catalog {
  return cell_id?.match(/^catalog_cell_/) ? true : false;
}

export function is_plot_id(id: string): id is PlotID {
  return id?.match(/^plot_\d+/) ? true : false;
}

export function is_defined(value: any): boolean {
  return typeof value !== `undefined`;
}

export function assert_defined<T>(value: T | undefined): asserts value is T {
  if (!is_defined(value)) throw new Error(`Expected value to be defined`);
}

export function assert_catalog_cell_id(
  cell_id: CellID.Any | undefined
): asserts cell_id is CellID.Catalog {
  assert_defined(cell_id);
  if (!is_catalog_cell_id(cell_id))
    throw new Error(`${cell_id} is not a catalog cell id`);
}

export function assert_plot_id(id: string): asserts id is PlotID {
  if (!is_plot_id(id)) throw new Error(`${id} is not a plot id`);
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
  concise: (d: number) => {
    if (d < 1e4) return d3.format(`,.4~g`)(d);
    return d3.format(`.2~e`)(d);
  },
  commas: (d: number) => {
    return d3.format(`,`)(d);
  }
};

/**
 * Type is either:
 * - ENUMERABLE_INTEGER
 * - LABELLED_ENUMERABLE_INTEGER
 * - LABELLED_ENUMERABLE_BOOLEAN
 */
export function join_enums(
  metadata: FieldMetadata
): Array<{ text: string; count?: number; value: FilterValueRaw }> {
  const field_type = get_field_type(metadata);
  const joined: { text: string; value: any; count?: number }[] = (() => {
    if (field_type === `LABELLED_ENUMERABLE_BOOLEAN`) {
      return [false, true].map((value, index) => {
        const text = metadata.enum?.[index] ?? value.toString();
        const found = metadata.stats?.terms?.find((t) => t.value === value);
        const count = found?.count ?? null;
        return {
          text,
          value,
          count
        };
      });
    } else if (field_type === `LABELLED_ENUMERABLE_INTEGER`) {
      return metadata.enum?.map((text, value) => {
        const found = metadata.stats?.terms?.find((t) => t.value === value);
        const count = found?.count ?? null;
        return {
          text,
          value,
          count
        };
      });
    } else if (field_type === `ENUMERABLE_INTEGER`) {
      return metadata.stats?.terms?.map(({ value, count }) => {
        const text = value.toString();
        return {
          text,
          value,
          count
        };
      });
    } else {
      throw new Error(`Not implemented: ${field_type}`);
    }
  })();
  const has_enum = metadata.enum && metadata.enum.length > 0 ? true : false;
  const sorted = d3.sort(
    joined,
    has_enum ? (d) => -d.count : (d) => Number(d.text)
  );
  const has_count = sorted.some((d) => d.count > 0);
  let filtered = sorted;
  if (has_count) {
    filtered = sorted.filter((d) => d.count > 0);
  }
  return filtered;
}

export function create_context_helper<T>(name: string) {
  const context = React.createContext<T | null>(null);
  const useContext = (): T => {
    const value: T | null = React.useContext(context);
    if (value === null) {
      throw new Error(`useContextHelper: ${name}: value is null`);
    }
    return value;
  };
  return [useContext, context.Provider] as const;
}

export function get_field_titles<T extends { title?: string }>(
  node: d3.HierarchyNode<T>
): string[] {
  const titles: string[] = [];
  let current_node: d3.HierarchyNode<T> | null = node;
  while (current_node !== null) {
    if (current_node.data.title?.length ?? 0 > 0) {
      titles.push(current_node.data.title ?? `unknown`);
    }
    current_node = current_node.parent;
  }
  return titles.reverse();
}
