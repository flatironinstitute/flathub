import React from "react";
import { useAppState, useMergeState } from "./AppStateContext";
import {
  useCatalogCellID,
  useCatalogID
} from "@/components/contexts/CatalogCellIDContext";
import { useCatalogMetadata } from "./CatalogMetadataContext";
import type { RowSelectionState } from "@tanstack/react-table";

const ColumnsContext = React.createContext<Set<string>>(new Set());

export function ColumnsProvider({ children }) {
  const catalog_id = useCatalogID();
  const catalog_cell_id = useCatalogCellID();
  const catalog_metadata_wrapper = useCatalogMetadata();
  const app_state = useAppState();
  const column_ids_set: Set<string> =
    catalog_metadata_wrapper?.initial_column_ids ?? new Set();
  const user_selected_columns =
    app_state.show_columns?.[catalog_cell_id]?.[catalog_id] ?? {};
  for (const [field_id, selected] of Object.entries(user_selected_columns)) {
    if (selected) {
      column_ids_set.add(field_id);
    } else {
      column_ids_set.delete(field_id);
    }
  }
  return (
    <ColumnsContext.Provider value={column_ids_set}>
      {children}
    </ColumnsContext.Provider>
  );
}

export function useColumns(): Set<string> {
  const columns = React.useContext(ColumnsContext);
  return columns;
}

export function useSetColumns() {
  const catalog_cell_id = useCatalogCellID();
  const catalog_id = useCatalogID();
  const merge_state = useMergeState();
  return (rows_object: RowSelectionState) => {
    merge_state({
      show_columns: {
        [catalog_cell_id]: {
          [catalog_id]: rows_object
        }
      }
    });
  };
}

export function useAddColumn() {
  const set_columns = useSetColumns();
  return (hash: string) => {
    set_columns({
      [hash]: true
    });
  };
}
