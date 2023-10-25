import type {
  CatalogHierarchyNode,
  FieldID,
  FieldMetadata,
  FilterValueRaw,
  Filters
} from "../types";

import React from "react";
import lodash_merge from "lodash.merge";
import {
  assert_numeric_field_stats,
  get_field_type,
  has_numeric_field_stats
} from "../shared";
import { useCatalogCellID, useCatalogID } from "./CatalogContext";
import { useCatalogMetadata } from "./CatalogMetadataContext";
import { useFieldNode } from "./FieldNodeContext";
import { useAppState, useMergeState, useSetAppState } from "./AppStateContext";

const FiltersContext = React.createContext(null);

export function FiltersProvider({ children }) {
  const catalog_cell_id = useCatalogCellID();
  const catalog_id = useCatalogID();
  const catalog_metadata = useCatalogMetadata();
  const catalog_hierarchy = catalog_metadata?.hierarchy;
  const app_state = useAppState();
  const show_filters_config: Record<FieldID, boolean> =
    app_state?.add_filter?.[catalog_cell_id]?.[catalog_id] ?? {};
  const filter_names_set = catalog_hierarchy
    ? get_filter_names(catalog_hierarchy, show_filters_config)
    : new Set<string>();
  const initial_filters: Filters = get_initial_cell_filters(
    filter_names_set,
    catalog_hierarchy
  );
  const filter_state: Filters =
    app_state?.set_filter_value?.[catalog_cell_id]?.[catalog_id];
  const filters = lodash_merge(initial_filters, filter_state);
  return (
    <FiltersContext.Provider value={filters}>
      {children}
    </FiltersContext.Provider>
  );
}

export function useFilters(): Filters {
  const filters = React.useContext(FiltersContext);
  if (filters === null) {
    throw new Error(`useFilters must be used within a FilterProvider`);
  }
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

function get_initial_cell_filters(
  filter_names: Set<string>,
  catalog_field_hierarchy?: d3.HierarchyNode<FieldMetadata>
): Filters {
  if (!catalog_field_hierarchy) return {};
  const initial_filter_object: Filters = Object.fromEntries(
    Array.from(filter_names).map((filter_name) => {
      const metadata = catalog_field_hierarchy.find(
        (node) => node.data.name === filter_name
      )?.data;

      if (!metadata) {
        throw new Error(`Could not find metadata for filter ${filter_name}`);
      }

      const field_type = get_field_type(metadata);

      const initial_value: FilterValueRaw = (() => {
        switch (field_type) {
          case `ROOT`:
            throw new Error(`Root field should not be a filter`);
          case `INTEGER`:
          case `FLOAT`:
            if (!has_numeric_field_stats(metadata)) {
              throw new Error(
                `Field ${metadata.name} does not have numeric field stats`
              );
            }
            assert_numeric_field_stats(metadata);
            return {
              gte: metadata.stats.min,
              lte: metadata.stats.max
            };
          case `LABELLED_ENUMERABLE_BOOLEAN`:
            return false;
          case `LABELLED_ENUMERABLE_INTEGER`:
          case `ENUMERABLE_INTEGER`:
            return 0;
          case `ARRAY`:
            return null;
          case `STRING`:
            return ``;
          default:
            field_type satisfies never;
        }
      })();

      return [filter_name, initial_value];
    })
  );
  return initial_filter_object;
}

export function useSetFilterValue(
  field_node: CatalogHierarchyNode = useFieldNode()
) {
  const field_id = field_node.data.name;
  const catalog_cell_id = useCatalogCellID();
  const catalog_id = useCatalogID();
  const merge_state = useMergeState();
  return (value: FilterValueRaw) => {
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

export function useRemoveFilter() {
  const catalog_cell_id = useCatalogCellID();
  const catalog_id = useCatalogID();
  const set_app_state = useSetAppState();
  return (node: CatalogHierarchyNode) => {
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

export function useAddFilter() {
  const catalog_cell_id = useCatalogCellID();
  const catalog_id = useCatalogID();
  const merge_state = useMergeState();

  return (node: CatalogHierarchyNode) => {
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
