import type {
  CatalogHierarchyNode,
  FieldID,
  FieldMetadata,
  FilterValueRaw,
  Filters
} from "../types";

import React from "react";
import lodash_merge from "lodash.merge";
import { useCatalogCellID, useCatalogID } from "./CatalogContext";
import { useCatalogMetadata } from "./CatalogMetadataContext";
import { useAppState, useMergeState, useSetAppState } from "./AppStateContext";

const FilterValuesContext = React.createContext(null);
const FilterNamesContext = React.createContext<Set<string>>(new Set());

export function FiltersProvider({ children }) {
  const catalog_cell_id = useCatalogCellID();
  const catalog_id = useCatalogID();
  const catalog_metadata = useCatalogMetadata();
  const catalog_hierarchy = catalog_metadata?.hierarchy;
  const app_state = useAppState();
  const show_filters_config: Record<FieldID, boolean> =
    app_state?.add_filter?.[catalog_cell_id]?.[catalog_id] ?? {};
  const filter_names_set: Set<string> = catalog_hierarchy
    ? get_filter_names(catalog_hierarchy, show_filters_config)
    : new Set<string>();
  const filter_state: Filters =
    app_state?.set_filter_value?.[catalog_cell_id]?.[catalog_id] ?? {};
  return (
    <FilterNamesContext.Provider value={filter_names_set}>
      <FilterValuesContext.Provider value={filter_state}>
        {children}
      </FilterValuesContext.Provider>
    </FilterNamesContext.Provider>
  );
}

export function useFilterNames() {
  const filter_names = React.useContext(FilterNamesContext);
  return filter_names;
}

export function useFilterValues(): Filters {
  const filters = React.useContext(FilterValuesContext);
  return filters;
}

export function get_filter_names(
  hierarchy: d3.HierarchyNode<FieldMetadata>,
  show_filters_config: Record<FieldID, boolean>
) {
  const nodes = hierarchy.descendants();
  const initial_filter_names = nodes
    .filter((node) => node.height === 0 && `required` in node.data)
    .map((node) => node.data.name);
  const filter_names_set: Set<string> = new Set(initial_filter_names);
  for (const [key, value] of Object.entries(show_filters_config)) {
    if (value) {
      filter_names_set.add(key);
    } else {
      filter_names_set.delete(key);
    }
  }
  return filter_names_set;
}

export function useSetFilterValue(node: CatalogHierarchyNode) {
  const catalog_cell_id = useCatalogCellID();
  const catalog_id = useCatalogID();
  const merge_state = useMergeState();
  return (value: FilterValueRaw) => {
    const field_id = node.data.name;
    merge_state({
      set_filter_value: {
        [catalog_cell_id]: {
          [catalog_id]: {
            [field_id]: value
          }
        }
      }
    });
  };
}

export function useClearFilterValue(node: CatalogHierarchyNode) {
  const catalog_cell_id = useCatalogCellID();
  const catalog_id = useCatalogID();
  const set_app_state = useSetAppState();
  return () => {
    const field_id = node.data.name;
    set_app_state((obj) => {
      lodash_merge(obj, {
        set_filter_value: {
          [catalog_cell_id]: {
            [catalog_id]: {
              [field_id]: null
            }
          }
        }
      });
      delete obj.set_filter_value[catalog_cell_id][catalog_id][field_id];
    });
  };
}

export function useRemoveFilter(node: CatalogHierarchyNode) {
  const catalog_cell_id = useCatalogCellID();
  const catalog_id = useCatalogID();
  const set_app_state = useSetAppState();
  return () => {
    const field_id = node.data.name;
    set_app_state((obj) => {
      lodash_merge(obj, {
        add_filter: {
          [catalog_cell_id]: {
            [catalog_id]: {
              [field_id]: false
            }
          }
        },
        set_filter_value: {
          [catalog_cell_id]: {
            [catalog_id]: {
              [field_id]: null
            }
          }
        }
      });
      delete obj.set_filter_value[catalog_cell_id][catalog_id][field_id];
    });
  };
}

export function useAddFilter(node: CatalogHierarchyNode) {
  const catalog_cell_id = useCatalogCellID();
  const catalog_id = useCatalogID();
  const merge_state = useMergeState();

  return () => {
    const field_id = node.data.name;
    merge_state({
      add_filter: {
        [catalog_cell_id]: {
          [catalog_id]: {
            [field_id]: true
          }
        }
      }
    });
  };
}
