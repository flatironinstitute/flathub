import React from "react";
import { useAppState, useMergeState } from "./AppStateContext";
import {
  useCatalogCellID,
  useCatalogID
} from "@/components/contexts/CatalogIDContext";
import { useCatalogMetadata } from "./CatalogMetadataContext";
import type { RowSelectionState } from "@tanstack/react-table";

const ColumnIDsContext = React.createContext<Set<string>>(new Set());

export function ColumnsProvider({ children }) {
  const catalog_id = useCatalogID();
  const catalog_cell_id = useCatalogCellID();
  const catalog_metadata_wrapper = useCatalogMetadata();
  const app_state = useAppState();
  const column_ids: Set<string> =
    catalog_metadata_wrapper?.initial_column_ids ?? new Set();
  const user_selected_columns =
    app_state.show_columns?.[catalog_cell_id]?.[catalog_id] ?? {};
  for (const [field_id, selected] of Object.entries(user_selected_columns)) {
    if (selected) {
      column_ids.add(field_id);
    } else {
      column_ids.delete(field_id);
    }
  }
  return (
    <ColumnIDsContext.Provider value={column_ids}>
      {children}
    </ColumnIDsContext.Provider>
  );
}

export function useColumnIDs(): Set<string> {
  const column_ids = React.useContext(ColumnIDsContext);
  return column_ids;
}

export function useColumnNames(): Set<string> {
  const catalog_metadata_wrapper = useCatalogMetadata();
  const columns_ids = useColumnIDs();
  const column_field_names = new Set<string>();
  for (const column_id of columns_ids) {
    const node = catalog_metadata_wrapper?.get_node_from_id(column_id);
    if (node) {
      column_field_names.add(node.data.name);
    }
  }
  return column_field_names;
}

export function useSetColumns() {
  const catalog_cell_id = useCatalogCellID();
  const merge_state = useMergeState();
  return (rows_object: RowSelectionState) => {
    merge_state({
      show_columns: {
        [catalog_cell_id]: rows_object
      }
    });
  };
}

export function useAddColumn() {
  const set_columns = useSetColumns();
  return (field_id: string) => {
    set_columns({
      [field_id]: true
    });
  };
}
