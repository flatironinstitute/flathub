import type * as schema from "./flathub-schema";
import type { QueryObserverResult } from "@tanstack/query-core";

export type GlobalFilterState = Record<CellID, Record<string, FilterValueRaw>>;

// ===========================================
// ACTIONS

export type Action =
  | CellAction
  | FilterListAction
  | ColumnListAction
  | Actions[`SetCatalog`]
  | Actions[`SetCellType`]
  | Actions[`SetPlotType`]
  | Actions[`SetPlotControl`]
  | Actions[`SetDarkMode`];

export type Actions = {
  SetCatalog: ActionBase<
    `set_catalog`,
    { cell_id: GenericCellID; catalog_id: string }
  >;
  SetCellType: ActionBase<
    `set_cell_type`,
    { cell_id: GenericCellID; cell_type: "table" | "plot" }
  >;
  SetPlotType: ActionBase<
    `set_plot_type`,
    { cell_id: CellID; plot_type: "scatterplot" | "heatmap" }
  >;
  SetPlotControl: ActionBase<
    `set_plot_control`,
    { cell_id: CellID; key: string; value: any }
  >;
  SetDarkMode: ActionBase<`set_dark_mode`, { value: boolean }>;
};

export type ColumnListAction = ActionBase<
  `add_column` | `remove_column` | `remove_child_columns`,
  { cell_id: CellID; field_id: string }
>;

export type FilterListAction = ActionBase<
  `add_filter` | `remove_filter` | `remove_child_filters`,
  { cell_id: CellID; field_id: string }
>;

export type CellAction =
  | ActionBase<`add_cell`, { cell_id: GenericCellID }>
  | ActionBase<
      `add_catalog_cell`,
      { catalog_id: string; cell_id: CatalogCellID }
    >
  | ActionBase<`remove_catalog_cell`, { cell_id: CatalogCellID }>
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
    }
  | {
      type: `cell`;
      cell_id: GenericCellID;
      parent_cell_id: `root`;
    };

export type CellID =
  | RootCellID
  | CatalogCellID
  | FilterCellID
  | TableCellID
  | PlotCellID
  | GenericCellID;

export type CatalogCellID = `catalog_cell_${string}`;
export type FilterCellID = `filter_cell_${number}`;
export type TableCellID = `table_cell_${number}`;
export type PlotCellID = `plot_cell_${number}`;
export type GenericCellID = `cell_${number}`;
export type RootCellID = `root`;

// ===========================================
// SCHEMA

export type QueryParameters = Pick<DataRequestBody, "count">;

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
