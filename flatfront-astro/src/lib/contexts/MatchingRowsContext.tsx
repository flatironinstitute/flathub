import type { CountRequestBody, CountResponse } from "../types";

import React from "react";
import { useQuery } from "@tanstack/react-query";
import { fetch_api_post } from "../shared";
import { useCatalogID } from "./CatalogContext";
import { useFilters } from "./FiltersContext";
import { useRandomConfig } from "./RandomContext";

const MatchingRowsContext = React.createContext<number | undefined>(undefined);

export function MatchingRowsProvider({
  children
}: {
  children: React.ReactNode;
}) {
  const catalog_id = useCatalogID();
  const filters = useFilters();
  const random_config = useRandomConfig();
  const request_body: CountRequestBody = {
    ...filters,
    ...random_config
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

export function useMatchingRows(): number | undefined {
  const matching_rows = React.useContext(MatchingRowsContext);
  return matching_rows;
}
