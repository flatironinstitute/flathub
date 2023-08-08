import type * as schema from "./flathub-schema";
import type { QueryObserverResult } from "@tanstack/query-core";

// ===========================================
// ACTIONS

export type Action = FilterListAction | AddQueryCellAction;

type FilterListAction = ActionBase<
  `add_filter` | `remove_filter`,
  { cell_id: CellID; filter_name: string }
>;

export type CellAction = AddQueryCellAction;

export type AddQueryCellAction = ActionBase<
  `add_query_cell`,
  { catalog_name: string; cell_id: CellID }
>;

export type ActionBase<T extends string, U> = U & {
  type: T;
};

// ===========================================
// BASICS

export type CatalogMetadataWrapper = {
  metadata: CatalogResponse;
  hierarchy: d3.HierarchyNode<FieldGroup>;
  nodes: d3.HierarchyNode<FieldGroup>[];
  nodes_by_name: Map<string, d3.HierarchyNode<FieldGroup>>;
  initial_filter_names: string[];
  initial_column_names: string[];
};

export type CatalogMetadataQuery = QueryObserverResult<CatalogMetadataWrapper>;

export type CatalogHierarchyNode = d3.HierarchyNode<FieldGroup>;

export type DataResponse = Array<Datum>;
export type Datum = Record<string, any>;

export type Cell = {
  type: `query`;
  id: CellID;
  catalog_name: string;
};

export type CellID = `query_cell_${number}`;

// ===========================================
// SCHEMA

export type DataRequestBody = NonNullable<
  schema.operations["dataPOST"]["requestBody"]
>["content"]["application/json"];

export type FieldGroup = schema.components["schemas"]["FieldGroup"];

export type CatalogResponse =
  schema.components["responses"]["catalog"]["content"]["application/json"];

export type Filters = schema.components["schemas"]["Filters"];
