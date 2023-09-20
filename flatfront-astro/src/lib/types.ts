import type * as schema from "./flathub-schema";

export type FieldType =
  | `ROOT`
  | `INTEGER`
  | `FLOAT`
  | `LABELLED_ENUMERABLE_BOOLEAN`
  | `LABELLED_ENUMERABLE_INTEGER`
  | `ENUMERABLE_INTEGER`
  | `ARRAY`
  | `STRING`;

export type GlobalFilterState = Record<
  CellID.Catalog,
  Record<string, FilterValueRaw>
>;

export type CatalogMetadataWrapper = {
  response: CatalogResponse;
  hierarchy: CatalogHierarchyNode;
  depth_first: Array<CatalogHierarchyNode>;
};

export type CatalogHierarchyNode = d3.HierarchyNode<FieldMetadata>;

export namespace Cell {
  export type Catalog = {
    type: `catalog`;
    cell_id: CellID.Catalog;
  };
  export type Comparison = {
    type: `comparison`;
    cell_id: CellID.Comparison;
  };
  export type Table = {
    type: `table`;
    cell_id: CellID.Table;
    catalog_cell_id: CellID.Catalog;
  };
  export type Plot = {
    type: `plot`;
    cell_id: CellID.Plot;
    catalog_cell_id: CellID.Catalog;
  };
  export type Any = Catalog | Comparison | Table | Plot;
}

export namespace Action {
  export type AddCatalogCell = ActionBase<
    `add_catalog_cell`,
    { cell_id: CellID.Catalog }
  >;
  export type AddTableCell = ActionBase<
    `add_table_cell`,
    { cell_id: CellID.Table; catalog_cell_id: CellID.Catalog }
  >;
  export type AddPlotCell = ActionBase<
    `add_plot_cell`,
    { cell_id: CellID.Plot; catalog_cell_id: CellID.Catalog }
  >;
  export type AddComparisonCell = ActionBase<
    `add_comparison_cell`,
    { cell_id: CellID.Comparison }
  >;
  export type RemoveCell = ActionBase<`remove_cell`, { cell_id: CellID.Any }>;
  export type CellAction =
    | AddCatalogCell
    | AddComparisonCell
    | AddTableCell
    | AddPlotCell
    | RemoveCell;
  export type FilterList = ActionBase<
    `add_filter` | `remove_filter` | `remove_child_filters`,
    { cell_id: CellID.Catalog; field_id: string }
  >;
  export type SetCatalog = ActionBase<
    `set_catalog`,
    { cell_id: CellID.Catalog; catalog_id: string }
  >;
  export type Any = CellAction | FilterList | SetCatalog;
}

type ActionBase<T extends string, U> = U & {
  type: T;
};

export namespace CellID {
  export type Catalog = `catalog_cell_${number}`;
  export type Comparison = `comparison_cell_${number}`;
  export type Table = `table_cell_${number}`;
  export type Plot = `plot_cell_${number}`;
  export type Any = Catalog | Comparison | Table | Plot;
}

// ===========================================
// SCHEMA
// ===========================================

export type FilterValueRaw = Filters[string];

export type Filters = schema.components["schemas"]["Filters"];

export type FieldMetadata = schema.components["schemas"]["FieldGroup"] & {
  __hash?: string;
};

export type CatalogResponse =
  schema.components["responses"]["catalog"]["content"]["application/json"];

export type TopResponseEntry = TopResponse[number];

export type TopResponse =
  schema.components["responses"]["top"]["content"]["application/json"];
