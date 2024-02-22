import type {
  AppState,
  CatalogHierarchyNode,
  FieldID,
  FilterValueRaw,
  Filters
} from "@/types";

import React from "react";
import lodash_merge from "lodash.merge";
import { useCatalogCellID } from "@/components/contexts/CatalogIDContext";
import { useCatalogMetadata } from "./CatalogMetadataContext";
import { useAppState, useMergeState, useSetAppState } from "./AppStateContext";
import { log } from "@/utils";

const FilterIDsContext = React.createContext<Set<string>>(new Set());
const FilterValuesContext = React.createContext(null);

export function FiltersProvider({ children }) {
  const catalog_cell_id = useCatalogCellID();
  const catalog_metadata = useCatalogMetadata();
  const app_state = useAppState();
  const filter_ids_set: Set<string> =
    catalog_metadata?.initial_filter_ids ?? new Set<string>();
  const show_filters_config: Record<FieldID, boolean> =
    app_state?.cells?.[catalog_cell_id]?.show_filters ?? {};
  for (const [key, value] of Object.entries(show_filters_config)) {
    if (value) {
      filter_ids_set.add(key);
    } else {
      filter_ids_set.delete(key);
    }
  }
  const initial_filter_values: Filters =
    catalog_metadata?.initial_filter_values ?? {};
  const filter_state: Filters =
    app_state?.[catalog_cell_id]?.filter_values ?? {};
  const combined = {
    ...initial_filter_values,
    ...filter_state
  };
  // Sanitize filter_state
  const sanitized: Filters = {};
  for (const [key, value] of Object.entries(combined)) {
    if (value === null) continue;
    if (typeof value === `undefined`) continue;
    sanitized[key] = value;
  }
  return (
    <FilterIDsContext.Provider value={filter_ids_set}>
      <FilterValuesContext.Provider value={sanitized}>
        {children}
      </FilterValuesContext.Provider>
    </FilterIDsContext.Provider>
  );
}

export function useFilterIDs() {
  const filter_names = React.useContext(FilterIDsContext);
  return filter_names;
}

export function useFilterValues(): Filters {
  const filters = React.useContext(FilterValuesContext);
  return filters;
}

export function useFilterValuesWithFieldNames(): Filters {
  const catalog_metadata = useCatalogMetadata();
  const filters = useFilterValues();
  const filter_values_with_field_names: Filters = {};
  for (const [field_id, value] of Object.entries(filters)) {
    const node = catalog_metadata?.get_node_from_id(field_id);
    if (node) {
      filter_values_with_field_names[node.data.name] = value;
    }
  }
  return filter_values_with_field_names;
}

export function useSetFilterValues() {
  const catalog_cell_id = useCatalogCellID();
  const merge_state = useMergeState();
  return (filter_values: Filters) => {
    log(`Setting filter values`, filter_values);
    merge_state({
      [catalog_cell_id]: {
        filter_values: filter_values
      }
    });
  };
}

export function useSetFilterValue() {
  const catalog_metadata = useCatalogMetadata();
  const set_filter_values = useSetFilterValues();
  return (node: CatalogHierarchyNode, value: FilterValueRaw) => {
    const field_id = catalog_metadata.get_id_from_node(node);
    set_filter_values({
      [field_id]: value
    });
  };
}

export function useClearFilterValue() {
  const catalog_cell_id = useCatalogCellID();
  const set_app_state = useSetAppState();
  const catalog_metadata = useCatalogMetadata();
  return (node: CatalogHierarchyNode) => {
    const field_id = catalog_metadata.get_id_from_node(node);
    set_app_state((obj) => {
      lodash_merge<AppState, AppState>(obj, {
        [catalog_cell_id]: {
          filter_values: {
            [field_id]: null
          }
        }
      });
      delete obj[catalog_cell_id].filter_values[field_id];
    });
  };
}

export function useAddFilter() {
  const catalog_cell_id = useCatalogCellID();
  const merge_state = useMergeState();
  const catalog_metadata = useCatalogMetadata();
  return (node: CatalogHierarchyNode) => {
    const field_id = catalog_metadata.get_id_from_node(node);
    merge_state({
      cells: {
        [catalog_cell_id]: {
          show_filters: {
            [field_id]: true
          }
        }
      }
    });
  };
}

export function useRemoveFilter() {
  const catalog_cell_id = useCatalogCellID();
  const set_app_state = useSetAppState();
  const catalog_metadata = useCatalogMetadata();
  return (node: CatalogHierarchyNode) => {
    const field_id = catalog_metadata.get_id_from_node(node);
    set_app_state((obj) => {
      lodash_merge<AppState, AppState>(obj, {
        cells: {
          [catalog_cell_id]: {
            show_filters: {
              [field_id]: false
            },
            filter_values: {
              [field_id]: null
            }
          }
        }
      });
      delete obj[catalog_cell_id].filter_values[field_id];
    });
  };
}

export function useGetCurrentFilterMin(): (field_id: string) => number {
  const filters = useFilterValues();
  const catalog_metadata = useCatalogMetadata();
  return (field_id: string) => {
    if (!field_id) return null;
    if (!catalog_metadata) return null;
    const filter_value = filters[field_id];
    const node = catalog_metadata?.get_node_from_id(field_id);
    const field_stats = node?.data?.stats;
    if (typeof filter_value === `object` && `gte` in filter_value) {
      return Number(filter_value.gte);
    } else if (field_stats && "min" in field_stats) {
      return Number(field_stats.min);
    }
    throw new Error(`Could not get min for ${field_id}`);
  };
}

export function useGetCurrentFilterMax(): (field_id: string) => number {
  const filters = useFilterValues();
  const catalog_metadata = useCatalogMetadata();
  return (field_id: string) => {
    if (!field_id) return null;
    if (!catalog_metadata) return null;
    const filter_value = filters[field_id];
    const node = catalog_metadata?.get_node_from_id(field_id);
    const field_stats = node?.data?.stats;
    if (typeof filter_value === `object` && `lte` in filter_value) {
      return Number(filter_value.lte);
    } else if (field_stats && "max" in field_stats) {
      return Number(field_stats.max);
    }
    throw new Error(`Could not get max for ${field_id}`);
  };
}
