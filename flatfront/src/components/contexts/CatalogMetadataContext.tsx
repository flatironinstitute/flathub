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
import { useQueries, type UseQueryResult } from "@tanstack/react-query";
import { fetch_api_get, log, is_root_node } from "@/utils";
import { useCatalogID } from "./CatalogIDContext";
import { useAppState } from "./AppStateContext";

const CatalogMetadataContext = React.createContext<
  CatalogMetadataWrapper | undefined
>(undefined);

const AllCatalogMetadataQueriesContext = React.createContext<
  UseQueryResult<CatalogMetadataWrapper>[]
>([]);

export function useCatalogMetadata(): CatalogMetadataWrapper | undefined {
  const catalog_metadata = React.useContext(CatalogMetadataContext);
  return catalog_metadata;
}

export function useAllCatalogMetadataQueries() {
  const catalog_metadata_queries = React.useContext<
    UseQueryResult<CatalogMetadataWrapper>[]
  >(AllCatalogMetadataQueriesContext);
  return catalog_metadata_queries;
}

export function CatalogMetadataProvider({ children }) {
  const catalog_id = useCatalogID();
  const all_catalog_metadata_queries = useAllCatalogMetadataQueries();
  const catalog_metadata =
    all_catalog_metadata_queries.find(
      (query) => query?.data?.response?.name === catalog_id
    )?.data ?? undefined;
  return (
    <CatalogMetadataContext.Provider value={catalog_metadata}>
      {children}
    </CatalogMetadataContext.Provider>
  );
}

export function AllCatalogMetadataQueriesProvider({ children }) {
  const app_state = useAppState();
  const cells = Object.values(app_state?.cells ?? {});
  const catalog_ids = new Set<CatalogID>();
  for (const cell of cells) {
    catalog_ids.add(cell.catalog_id);
  }
  const catalog_metadata_queries = useQueries({
    queries: [...catalog_ids].map((catalog_id) => ({
      queryKey: [`catalog`, catalog_id],
      queryFn: (): Promise<CatalogMetadataWrapper> =>
        fetch_api_get<CatalogResponse>(`/${catalog_id}`).then((response) =>
          wrap_catalog_response(response)
        )
    }))
  });
  return (
    <AllCatalogMetadataQueriesContext.Provider value={catalog_metadata_queries}>
      {children}
    </AllCatalogMetadataQueriesContext.Provider>
  );
}

function wrap_catalog_response(
  catalog_response: CatalogResponse
): CatalogMetadataWrapper {
  log(`Creating metadata for ${catalog_response.name}...`);
  const hierarchy: CatalogHierarchyNode =
    create_catalog_hierarchy(catalog_response);
  const node_to_id = new Map<CatalogHierarchyNode, string>();
  const id_to_node = new Map<string, CatalogHierarchyNode>();
  const depth_first: Array<CatalogHierarchyNode> = [];
  const initial_column_ids: Set<string> = new Set();
  const initial_filter_ids: Set<string> = new Set();
  const initial_filter_values: Filters = {};
  const attachment_field_ids: Set<string> = new Set();
  hierarchy.eachBefore((node) => {
    const is_leaf = node.height === 0;
    const id = is_leaf
      ? node.data.name
      : `${node.data.name}_${tiny_json_hash(node.data)}`;
    node_to_id.set(node, id);
    id_to_node.set(id, node);
    if (!is_root_node(node)) depth_first.push(node);
    if (is_leaf && node.data.disp === true) initial_column_ids.add(id);
    if (is_leaf && `required` in node.data) initial_filter_ids.add(id);
    if (is_leaf && `default` in node.data)
      initial_filter_values[id] = node.data.default;
    if (is_leaf && node.data.required === true) {
      if (`default` in node.data) {
        initial_filter_values[id] = node.data.default;
      } else {
        const initial_value = node.data.stats?.terms?.[0]?.value;
        console.warn(
          `No default value for required filter ${id}. Using value: ${initial_value}`,
          node.data
        );
        if (typeof initial_value !== `undefined`) {
          initial_filter_values[id] = initial_value;
        }
      }
    }
    if (is_leaf && `attachment` in node.data) attachment_field_ids.add(id);
  });
  const wrapper: CatalogMetadataWrapper = {
    response: catalog_response,
    hierarchy,
    depth_first,
    get_id_from_node: (node) => node_to_id.get(node),
    get_node_from_id: (id) => id_to_node.get(id),
    initial_column_ids,
    initial_filter_ids,
    initial_filter_values,
    attachment_field_ids
  };
  log(`Metadata for ${catalog_response.name}:`, wrapper);
  return wrapper;
}

export function create_catalog_hierarchy(
  catalog_response: CatalogResponse
): CatalogHierarchyNode {
  const root = {
    sub: catalog_response?.fields ?? []
  } as FieldMetadata;
  const hierarchy: CatalogHierarchyNode = d3.hierarchy<FieldMetadata>(
    root,
    (d) => d?.sub ?? []
  );
  return hierarchy;
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
