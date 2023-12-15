import type { CountRequestBody, CountResponse } from "@/types";

import React from "react";
import { useQuery } from "@tanstack/react-query";
import { fetch_api_post, format } from "@/utils";
import { useCatalogID } from "@/components/contexts/CatalogIDContext";
import {
  useFilterValues,
  useFilterValuesWithFieldNames
} from "@/components/contexts/FiltersContext";
import { useRandomConfig } from "@/components/contexts/RandomContext";
import { useCatalogMetadata } from "@/components/contexts/CatalogMetadataContext";

const MatchingRowsContext = React.createContext<number | undefined>(undefined);

export function MatchingRowsProvider({
  children
}: {
  children: React.ReactNode;
}) {
  const catalog_id = useCatalogID();
  const filters = useFilterValuesWithFieldNames();
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

export function useMatchingRowsText(): string | null {
  const catalog_id = useCatalogID();
  const catalog_metadata = useCatalogMetadata();
  const total_rows = catalog_metadata?.response?.count;
  const matching = useMatchingRows();
  const r = Number.isFinite(matching)
    ? format.commas(matching)
    : `[Loading...]`;
  const t = total_rows ? format.commas(total_rows) : `[Loading...]`;
  const text = `Filtered to ${r} out of ${t} total rows`;
  if (!catalog_id) return null;
  return text;
}
