import type { CatalogHierarchyNode, FieldID, FieldMetadata } from "../types";

import React from "react";
import { useAppState, useMergeState } from "./AppStateContext";
import { useCatalogCellID, useCatalogID } from "./CatalogContext";
import { useCatalogMetadata } from "./CatalogMetadataContext";
import { log } from "../shared";

const ColumnsContext = React.createContext<Set<string>>(new Set());

export function ColumnsProvider({ children }) {
  const catalog_id = useCatalogID();
  const catalog_cell_id = useCatalogCellID();
  const catalog_metadata_wrapper = useCatalogMetadata();
  const app_state = useAppState();
  const user_selected_columns =
    app_state.show_columns?.[catalog_cell_id]?.[catalog_id] ?? {};
  const catalog_hierarchy = catalog_metadata_wrapper?.hierarchy;
  const column_ids_set = catalog_hierarchy
    ? get_column_ids(catalog_hierarchy, user_selected_columns)
    : new Set<string>();
  return (
    <ColumnsContext.Provider value={column_ids_set}>
      {children}
    </ColumnsContext.Provider>
  );
}

function get_column_ids(
  hierarchy: d3.HierarchyNode<FieldMetadata>,
  user_selected_columns: Record<FieldID, boolean>
): Set<string> {
  const nodes = hierarchy.descendants();
  const initial_column_ids = nodes
    .filter((node) => node.height === 0 && node.data.disp === true)
    .map((node) => node.data.name);
  const column_ids_set: Set<string> = new Set(initial_column_ids);
  for (const [field_id, selected] of Object.entries(user_selected_columns)) {
    if (selected) {
      column_ids_set.add(field_id);
    } else {
      column_ids_set.delete(field_id);
    }
  }
  return column_ids_set;
}

export function useColumns(): Set<string> {
  const columns = React.useContext(ColumnsContext);
  return columns;
}

export function useAddColumn() {
  const catalog_cell_id = useCatalogCellID();
  const catalog_id = useCatalogID();
  const merge_state = useMergeState();
  return (node: CatalogHierarchyNode) => {
    const field_id = node.data.name;
    merge_state({
      show_columns: {
        [catalog_cell_id]: {
          [catalog_id]: {
            [field_id]: true
          }
        }
      }
    });
  };
}

export function useRemoveColumn() {
  const catalog_cell_id = useCatalogCellID();
  const catalog_id = useCatalogID();
  const merge_state = useMergeState();
  return (node: CatalogHierarchyNode) => {
    const field_id = node.data.name;
    merge_state({
      show_columns: {
        [catalog_cell_id]: {
          [catalog_id]: {
            [field_id]: false
          }
        }
      }
    });
  };
}
