import type { CellID, CountRequestBody, CountResponse } from "../types";

import React from "react";
import { useQuery } from "@tanstack/react-query";
import { assert_catalog_cell_id, fetch_api_post } from "../shared";
import { FiltersProvider, useFilters } from "./FiltersContext";
import { CatalogMetadataProvider } from "./CatalogMetadataContext";
import { RandomProvider } from "./RandomContext";
import { useAppState } from "./AppStateContext";

const CatalogCellIDContext = React.createContext<CellID.Catalog | undefined>(
  undefined
);

const MatchingRowsContext = React.createContext<number | undefined>(undefined);

export function CatalogProvider({
  children,
  value: catalog_cell_id
}: {
  children: React.ReactNode;
  value: CellID.Catalog;
}) {
  return (
    <CatalogCellIDContext.Provider value={catalog_cell_id}>
      <CatalogMetadataProvider>
        <FiltersProvider>
          <RandomProvider>
            <MatchingRowsProvider>{children}</MatchingRowsProvider>
          </RandomProvider>
        </FiltersProvider>
      </CatalogMetadataProvider>
    </CatalogCellIDContext.Provider>
  );
}

function MatchingRowsProvider({ children }: { children: React.ReactNode }) {
  const catalog_id = useCatalogID();
  const filters = useFilters();
  const request_body: CountRequestBody = {
    ...filters
  };
  const query = useQuery({
    queryKey: [`count`, request_body],
    queryFn: async ({ signal }) => {
      const response = await fetch_api_post<CountRequestBody, CountResponse>(
        `/${catalog_id}/count`,
        request_body,
        { signal }
      );
      return response;
    },
    enabled: !!catalog_id
  });
  const result = query?.data ?? null;
  return (
    <MatchingRowsContext.Provider value={result}>
      {children}
    </MatchingRowsContext.Provider>
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

export function useMatchingRows(): number | undefined {
  const matching_rows = React.useContext(MatchingRowsContext);
  return matching_rows;
}
