import type {
  CatalogMetadataWrapper,
  CatalogResponse,
  FieldMetadata,
  CatalogHierarchyNode
} from "@/types";

import React from "react";
import * as d3 from "d3";
import { useQuery } from "@tanstack/react-query";
import { fetch_api_get, log, is_root_node } from "@/utils";
import { useCatalogID } from "./CatalogCellIDContext";

const CatalogMetadataContext = React.createContext<
  CatalogMetadataWrapper | undefined
>(undefined);

export function CatalogMetadataProvider({ children }) {
  const catalog_id = useCatalogID();
  const catalog_query = useQuery({
    queryKey: [`catalog`, catalog_id],
    queryFn: (): Promise<CatalogResponse> => fetch_api_get(`/${catalog_id}`),
    enabled: !!catalog_id
  });
  const wrapped = React.useMemo(
    () =>
      catalog_query.data
        ? wrap_catalog_response(catalog_query.data)
        : undefined,
    [catalog_query.data]
  );

  return (
    <CatalogMetadataContext.Provider value={wrapped}>
      {children}
    </CatalogMetadataContext.Provider>
  );
}

function wrap_catalog_response(catalog_response: CatalogResponse) {
  log(`Creating metadata for ${catalog_response.name}...`);
  const root = {
    sub: catalog_response.fields
  } as FieldMetadata;
  const hierarchy: CatalogHierarchyNode = d3.hierarchy<FieldMetadata>(
    root,
    (d) => d?.sub ?? []
  );
  const node_to_hash = new Map<CatalogHierarchyNode, string>();
  const hash_to_node = new Map<string, CatalogHierarchyNode>();
  const depth_first: Array<CatalogHierarchyNode> = [];
  const initial_column_ids: Set<string> = new Set();
  hierarchy.eachBefore((node) => {
    const hash = tiny_json_hash(node.data);
    node_to_hash.set(node, hash);
    hash_to_node.set(hash, node);
    if (!is_root_node(node)) depth_first.push(node);
    if (node.height === 0 && node.data.disp === true)
      initial_column_ids.add(hash);
  });
  const get_hash_from_node = (node: CatalogHierarchyNode) =>
    node_to_hash.get(node);
  const get_node_from_hash = (hash: string) => hash_to_node.get(hash);
  const wrapper: CatalogMetadataWrapper = {
    response: catalog_response,
    hierarchy,
    depth_first,
    get_hash_from_node,
    get_node_from_hash,
    initial_column_ids
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

export function useCatalogMetadata(): CatalogMetadataWrapper | undefined {
  const catalog_metadata = React.useContext(CatalogMetadataContext);
  return catalog_metadata;
}
