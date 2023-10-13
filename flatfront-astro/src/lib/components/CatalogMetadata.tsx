import type {
  CatalogResponse,
  FieldMetadata,
  CatalogHierarchyNode,
  CatalogMetadataWrapper
} from "../types";

import React from "react";
import * as d3 from "d3";
import { useQuery, type UseQueryResult } from "@tanstack/react-query";
import { fetch_api_get, log, is_root_node, get_field_type } from "../shared";
import { useCatalogID } from "./CatalogCell";

const CatalogMetadataContext = React.createContext<
  CatalogMetadataWrapper | undefined
>(undefined);

export function useCatalogMetadata(): CatalogMetadataWrapper | undefined {
  const catalog_metadata = React.useContext(CatalogMetadataContext);
  return catalog_metadata;
}

export default function CatalogMetadataProvider({ children }) {
  const catalog_id = useCatalogID();
  const catalog_query = useQuery({
    queryKey: [`catalog`, catalog_id],
    queryFn: (): Promise<CatalogResponse> => fetch_api_get(`/${catalog_id}`),
    enabled: !!catalog_id,
    staleTime: Infinity
  });
  const catalog_metadata = wrap_catalog_metadata(catalog_query);
  return (
    <CatalogMetadataContext.Provider value={catalog_metadata}>
      {children}
    </CatalogMetadataContext.Provider>
  );
}

function wrap_catalog_metadata(catalog_query: UseQueryResult<CatalogResponse>) {
  const catalog_response = catalog_query.data;
  if (!catalog_response) return undefined;
  log(`Creating metadata for ${catalog_response.name}...`);
  const root = {
    sub: catalog_response.fields
  } as FieldMetadata;
  const hierarchy: CatalogHierarchyNode = d3.hierarchy<FieldMetadata>(
    root,
    (d) => d?.sub ?? []
  );
  const depth_first: Array<CatalogHierarchyNode> = [];
  hierarchy.eachBefore((d) => {
    d.data.__hash = tiny_json_hash(d.data);
    if (!is_root_node(d)) depth_first.push(d);
    get_field_type(d.data);
  });
  const wrapper: CatalogMetadataWrapper = {
    response: catalog_response,
    hierarchy,
    depth_first
  };
  return wrapper;
}

function tiny_json_hash(object: any) {
  const text = JSON.stringify(object);
  let hash = 5381;
  let index = text.length;
  while (index) {
    hash = (hash * 33) ^ text.charCodeAt(--index);
  }
  return (hash >>> 0).toString(16);
}
