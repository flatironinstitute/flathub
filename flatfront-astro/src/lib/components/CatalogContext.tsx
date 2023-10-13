import type { CellID } from "../types";

import React from "react";
import * as controller from "../app-state";
import { assert_catalog_cell_id } from "../shared";

const CatalogCellIDContext = React.createContext<CellID.Catalog | undefined>(
  undefined
);

export function useCatalogCellID() {
  const catalog_cell_id = React.useContext(CatalogCellIDContext);
  if (catalog_cell_id === null) {
    throw new Error(`useCatalogCellID: value is null`);
  }
  assert_catalog_cell_id(catalog_cell_id);
  return catalog_cell_id;
}

export function useCatalogID(): string {
  const catalog_cell_id = useCatalogCellID();
  const catalog_id = controller.useAppState()?.set_catalog?.[catalog_cell_id];
  return catalog_id;
}

export const CatalogCellIDContextProvider = CatalogCellIDContext.Provider;
