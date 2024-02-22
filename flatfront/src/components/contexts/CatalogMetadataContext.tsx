import type {
  CatalogMetadataWrapper,
  CatalogResponse,
  FieldMetadata,
  CatalogHierarchyNode,
  CatalogID,
  Filters
} from "@/types";

import React from "react";
import * as d3 from "d3";
import { useQuery } from "@tanstack/react-query";
import { fetch_api_get, log, is_root_node } from "@/utils";
import { useCatalogID } from "./CatalogIDContext";

const CatalogMetadataContext = React.createContext<
  CatalogMetadataWrapper | undefined
>(undefined);

export function CatalogMetadataProvider({ children }) {
  const catalog_id = useCatalogID();
  const catalog_metadata = useCatalogMetadataFromQuery(catalog_id);
  return (
    <CatalogMetadataContext.Provider value={catalog_metadata}>
      {children}
    </CatalogMetadataContext.Provider>
  );
}

export function useCatalogMetadataFromQuery(
  catalog_id: CatalogID
): CatalogMetadataWrapper {
  const catalog_query = useCatalogQuery(catalog_id);
  const wrapped = React.useMemo(
    () =>
      catalog_query.data
        ? wrap_catalog_response(catalog_query.data)
        : undefined,
    [catalog_query.data]
  );
  return wrapped;
}

export function useCatalogQuery(catalog_id: CatalogID) {
  return useQuery({
    queryKey: [`catalog`, catalog_id],
    queryFn: (): Promise<CatalogResponse> => fetch_api_get(`/${catalog_id}`),
    enabled: !!catalog_id
  });
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
  const node_to_id = new Map<CatalogHierarchyNode, string>();
  const id_to_node = new Map<string, CatalogHierarchyNode>();
  const depth_first: Array<CatalogHierarchyNode> = [];
  const initial_column_ids: Set<string> = new Set();
  const initial_filter_ids: Set<string> = new Set();
  const initial_filter_values: Filters = {};
  hierarchy.eachBefore((node) => {
    const id = `${node.data.name}_${tiny_json_hash(node.data)}`;
    node_to_id.set(node, id);
    id_to_node.set(id, node);
    if (!is_root_node(node)) depth_first.push(node);
    if (node.height === 0 && node.data.disp === true)
      initial_column_ids.add(id);
    if (node.height === 0 && `required` in node.data)
      initial_filter_ids.add(id);
    if (node.height === 0 && `default` in node.data)
      initial_filter_values[id] = node.data.default;
  });
  const wrapper: CatalogMetadataWrapper = {
    response: catalog_response,
    hierarchy,
    depth_first,
    get_id_from_node: (node) => node_to_id.get(node),
    get_node_from_id: (id) => id_to_node.get(id),
    initial_column_ids,
    initial_filter_ids,
    initial_filter_values
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
