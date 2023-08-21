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
  `add_column` | `remove_column` | `remove_child_columns`,
  { cell_id: CellID; field_id: string }
>;

export type FilterListAction = ActionBase<
  `add_filter` | `remove_filter` | `remove_child_filters`,
  { cell_id: CellID; field_id: string }
>;

export type CellAction =
  | ActionBase<
      `add_catalog_cell`,
      { catalog_id: string; cell_id: CatalogCellID }
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
    >
  | ActionBase<
      `add_plot_cell`,
      { cell_id: PlotCellID; parent_cell_id: FilterCellID }
    >
  | ActionBase<`remove_plot_cell`, { cell_id: PlotCellID }>;

export type ActionBase<T extends string, U> = U & {
  type: T;
};

// ===========================================
// BASICS

export type CatalogMetadataWrapper = {
  metadata: CatalogResponse;
  hierarchy: d3.HierarchyNode<FieldMetadata>;
  nodes_array: d3.HierarchyNode<FieldMetadata>[];
  nodes_by_id: Map<string, d3.HierarchyNode<FieldMetadata>>;
  initial_filter_ids: string[];
  initial_column_ids: string[];
  get_field_metadata: (id: string) => FieldMetadata;
};

export type CatalogMetadataQuery = QueryObserverResult<CatalogMetadataWrapper>;

export type CatalogHierarchyNode = d3.HierarchyNode<FieldMetadata>;

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
      catalog_id: string;
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
    }
  | {
      type: `plot`;
      cell_id: PlotCellID;
      parent_cell_id: FilterCellID;
    };

export type CellID =
  | RootCellID
  | CatalogCellID
  | FilterCellID
  | TableCellID
  | PlotCellID;

export type CatalogCellID = `catalog_cell_${string}`;
export type FilterCellID = `filter_cell_${number}`;
export type TableCellID = `table_cell_${number}`;
export type PlotCellID = `plot_cell_${number}`;
export type RootCellID = `root`;

// ===========================================
// SCHEMA

export type TopResponseEntry = TopResponse[number];

export type TopResponse =
  schema.components["responses"]["top"]["content"]["application/json"];

export type DataRequestBody = NonNullable<
  schema.operations["dataPOST"]["requestBody"]
>["content"]["application/json"];

export type FieldMetadata = schema.components["schemas"]["FieldGroup"] & {
  __id?: string;
  __is_root?: boolean;
};

export type CatalogResponse =
  schema.components["responses"]["catalog"]["content"]["application/json"];

export type FilterValueNumeric = { gte: number; lte: number };
export type FilterValueRaw = Filters[string];

export type Filters = schema.components["schemas"]["Filters"];
