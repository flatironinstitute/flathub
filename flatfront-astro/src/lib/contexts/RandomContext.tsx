import React from "react";
import * as controller from "../app-state";
import { useCatalogCellID, useCatalogID } from "./CatalogContext";

const RandomContext = React.createContext(null);

export function RandomProvider({ children }) {
  const catalog_cell_id = useCatalogCellID();
  const catalog_id = useCatalogID();
  const app_state = controller.useAppState();
  const random_config =
    app_state?.set_random_sample?.[catalog_cell_id]?.[catalog_id];

  return (
    <RandomContext.Provider value={random_config}>
      {children}
    </RandomContext.Provider>
  );
}
