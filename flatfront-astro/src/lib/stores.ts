import type { QueryObserverResult } from "@tanstack/react-query";
import type { Readable } from "svelte/store";
import type {
  CatalogHierarchyNode,
  CatalogMetadataWrapper,
  CatalogResponse,
  FieldMetadata
} from "./types";

import * as d3 from "d3";
import { writable, derived, get } from "svelte/store";
import { get_field_type, log } from "./shared";

export const catalog_query_by_catalog_id = writable(
  {} as Record<string, QueryObserverResult<CatalogResponse>>
);

export const catalog_metadata_by_catalog_id: Readable<
  Record<string, CatalogMetadataWrapper>
> = (() => {
  const output = writable<Record<string, CatalogMetadataWrapper>>({});
  catalog_query_by_catalog_id.subscribe((obj) => {
    const value = get(output);
    for (const [catalog_id, query] of Object.entries(obj)) {
      if (catalog_id in value) continue;
      const catalog_response = query.data;
      if (!catalog_response) continue;
      log(`Creating metadata for ${catalog_id}...`);
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
      output.update((prev) => ({
        ...prev,
        [catalog_id]: wrapper
      }));
    }
  });
  return output;
})();

function is_root_node(node: CatalogHierarchyNode): boolean {
  return node.depth === 0;
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
