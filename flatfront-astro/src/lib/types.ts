import type * as schema from "./flathub-schema";

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
  export type Any = CellAction;
}

export namespace CellID {
  export type Catalog = `catalog_cell_${number}`;
  export type Comparison = `comparison_cell_${number}`;
  export type Table = `table_cell_${number}`;
  export type Plot = `plot_cell_${number}`;
  export type Any = Catalog | Comparison | Table | Plot;
}

type ActionBase<T extends string, U> = U & {
  type: T;
};

// ===========================================
// SCHEMA
// ===========================================

export type TopResponseEntry = TopResponse[number];

export type TopResponse =
  schema.components["responses"]["top"]["content"]["application/json"];
