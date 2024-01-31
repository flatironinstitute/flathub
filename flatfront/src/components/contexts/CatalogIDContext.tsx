import type { CellID } from "@/types";

import React from "react";
import { useAppState } from "./AppStateContext";

const CatalogIDContext = React.createContext<CellID.Catalog | undefined>(
  undefined
);

export function CatalogIDProvider({
  children,
  value: catalog_cell_id
}: {
  children: React.ReactNode;
  value: CellID.Catalog;
}) {
  return (
    <CatalogIDContext.Provider value={catalog_cell_id}>
      {children}
    </CatalogIDContext.Provider>
  );
}

export function useCatalogCellID() {
  const catalog_cell_id = React.useContext(CatalogIDContext);
  return catalog_cell_id;
}

export function useCatalogID(): string {
  const catalog_cell_id = useCatalogCellID();
  const catalog_id = useAppState()?.cells?.[catalog_cell_id]?.catalog_id;
  return catalog_id;
}
