import type * as schema from "./flathub-schema";
import type { QueryObserverResult } from "@tanstack/query-core";

// ===========================================
// ACTIONS

export type Action =
  | CellAction
  | FilterListAction
  | ColumnListAction
  | PlotControlAction;

export type PlotControlAction = ActionBase<
  `set_plot_control`,
  { cell_id: CellID; plot_id: string; key: string; value: any }
>;

export type ColumnListAction = ActionBase<
  `add_column` | `remove_column`,
  { cell_id: CellID; column_name: string }
>;

export type FilterListAction = ActionBase<
  `add_filter` | `remove_filter`,
  { cell_id: CellID; filter_name: string }
>;

export type CellAction =
  | ActionBase<
      `add_catalog_cell`,
      { catalog_name: string; cell_id: CatalogCellID }
    >
  | ActionBase<
      `add_filter_cell`,
      { cell_id: FilterCellID; parent_cell_id: CatalogCellID }
    >
  | ActionBase<`remove_filter_cell`, { cell_id: FilterCellID }>
  | ActionBase<
      `add_table_cell`,
      {
        cell_id: TableCellID;
        parent_cell_id: FilterCellID;
      }
    >
  | ActionBase<
      `remove_table_cell`,
      {
        cell_id: TableCellID;
      }
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

export type CatalogCell = Cell & {
  type: `catalog`;
};

export type Cell =
  | { type: `root`; cell_id: RootCellID; parent_cell_id: undefined }
  | {
      type: `catalog`;
      cell_id: CatalogCellID;
      catalog_name: string;
      parent_cell_id: `root`;
    }
  | {
      type: `filter`;
      cell_id: FilterCellID;
      parent_cell_id: CatalogCellID;
    }
  | {
      type: `table`;
      cell_id: TableCellID;
      parent_cell_id: FilterCellID;
    };

export type CellID = CatalogCellID | FilterCellID | TableCellID | RootCellID;

export type CatalogCellID = `catalog_cell_${string}`;
export type FilterCellID = `filter_cell_${number}`;
export type TableCellID = `table_cell_${number}`;
export type RootCellID = `root`;

// ===========================================
// SCHEMA

export type TopResponseEntry = TopResponse[number];

export type TopResponse =
  schema.components["responses"]["top"]["content"]["application/json"];

export type DataRequestBody = NonNullable<
  schema.operations["dataPOST"]["requestBody"]
>["content"]["application/json"];

export type FieldGroup = schema.components["schemas"]["FieldGroup"];

export type CatalogResponse =
  schema.components["responses"]["catalog"]["content"]["application/json"];

export type Filters = schema.components["schemas"]["Filters"];

export type FilterValue = { gte: number; lte: number };
