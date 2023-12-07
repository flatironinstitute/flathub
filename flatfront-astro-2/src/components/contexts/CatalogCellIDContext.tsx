import type { CellID } from "@/types";

import React from "react";
import { useAppState } from "./AppStateContext";

const CatalogCellIDContext = React.createContext<CellID.Catalog | undefined>(
  undefined
);

export function CatalogCellIDProvider({
  children,
  value: catalog_cell_id
}: {
  children: React.ReactNode;
  value: CellID.Catalog;
}) {
  return (
    <CatalogCellIDContext.Provider value={catalog_cell_id}>
      {children}
    </CatalogCellIDContext.Provider>
  );
}

export function useCatalogCellID() {
  const catalog_cell_id = React.useContext(CatalogCellIDContext);
  return catalog_cell_id;
}

export function useCatalogID(): string {
  const catalog_cell_id = useCatalogCellID();
  const catalog_id = useAppState()?.set_catalog?.[catalog_cell_id];
  return catalog_id;
}
