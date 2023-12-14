import type {
  AppState,
  CatalogHierarchyNode,
  FieldID,
  FieldMetadata,
  FilterValueRaw,
  Filters
} from "@/types";

import React from "react";
import lodash_merge from "lodash.merge";
import {
  useCatalogCellID,
  useCatalogID
} from "@/components/contexts/CatalogCellIDContext";
import { useCatalogMetadata } from "./CatalogMetadataContext";
import { useAppState, useMergeState, useSetAppState } from "./AppStateContext";

const FilterIDsContext = React.createContext<Set<string>>(new Set());
const FilterValuesContext = React.createContext(null);

export function FiltersProvider({ children }) {
  const catalog_cell_id = useCatalogCellID();
  const catalog_id = useCatalogID();
  const catalog_metadata = useCatalogMetadata();
  const app_state = useAppState();
  const filter_ids_set: Set<string> =
    catalog_metadata?.initial_filter_ids ?? new Set<string>();
  const show_filters_config: Record<FieldID, boolean> =
    app_state?.show_filters?.[catalog_cell_id]?.[catalog_id] ?? {};
  for (const [key, value] of Object.entries(show_filters_config)) {
    if (value) {
      filter_ids_set.add(key);
    } else {
      filter_ids_set.delete(key);
    }
  }
  const filter_state: Filters =
    app_state?.filter_values?.[catalog_cell_id]?.[catalog_id] ?? {};
  return (
    <FilterIDsContext.Provider value={filter_ids_set}>
      <FilterValuesContext.Provider value={filter_state}>
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

export function useSetFilterValue() {
  const catalog_cell_id = useCatalogCellID();
  const catalog_id = useCatalogID();
  const merge_state = useMergeState();
  const catalog_metadata = useCatalogMetadata();
  return (node: CatalogHierarchyNode, value: FilterValueRaw) => {
    const field_hash = catalog_metadata.get_hash_from_node(node);
    merge_state({
      filter_values: {
        [catalog_cell_id]: {
          [catalog_id]: {
            [field_hash]: value
          }
        }
      }
    });
  };
}

export function useClearFilterValue() {
  const catalog_cell_id = useCatalogCellID();
  const catalog_id = useCatalogID();
  const set_app_state = useSetAppState();
  const catalog_metadata = useCatalogMetadata();
  return (node: CatalogHierarchyNode) => {
    const field_hash = catalog_metadata.get_hash_from_node(node);
    set_app_state((obj) => {
      lodash_merge<AppState, AppState>(obj, {
        filter_values: {
          [catalog_cell_id]: {
            [catalog_id]: {
              [field_hash]: null
            }
          }
        }
      });
      delete obj.filter_values[catalog_cell_id][catalog_id][field_hash];
    });
  };
}

export function useAddFilter() {
  const catalog_cell_id = useCatalogCellID();
  const catalog_id = useCatalogID();
  const merge_state = useMergeState();
  const catalog_metadata = useCatalogMetadata();
  return (node: CatalogHierarchyNode) => {
    const field_hash = catalog_metadata.get_hash_from_node(node);
    merge_state({
      show_filters: {
        [catalog_cell_id]: {
          [catalog_id]: {
            [field_hash]: true
          }
        }
      }
    });
  };
}

export function useRemoveFilter() {
  const catalog_cell_id = useCatalogCellID();
  const catalog_id = useCatalogID();
  const set_app_state = useSetAppState();
  const catalog_metadata = useCatalogMetadata();
  return (node: CatalogHierarchyNode) => {
    const field_hash = catalog_metadata.get_hash_from_node(node);
    set_app_state((obj) => {
      lodash_merge<AppState, AppState>(obj, {
        show_filters: {
          [catalog_cell_id]: {
            [catalog_id]: {
              [field_hash]: false
            }
          }
        },
        filter_values: {
          [catalog_cell_id]: {
            [catalog_id]: {
              [field_hash]: null
            }
          }
        }
      });
      delete obj.filter_values[catalog_cell_id][catalog_id][field_hash];
    });
  };
}
