import type * as schema from "./flathub-schema";

export type { schema };

export type DataResponse = Array<DataRow>;
export type DataRow = Record<string, any>;

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

export namespace Action {
  export type AddCatalogCell = ActionBase<
    `add_catalog_cell`,
    { catalog_cell_id: CellID.Catalog }
  >;
  export type AddComparisonCell = ActionBase<
    `add_comparison_cell`,
    { comparison_cell_id: CellID.Comparison }
  >;
  export type RemoveCell = ActionBase<`remove_cell`, { cell_id: CellID.Any }>;
  export type AddFilter = ActionBase<
    `add_filter`,
    { catalog_cell_id: CellID.Catalog; field_id: string }
  >;
  export type RemoveFilter = ActionBase<
    `remove_filter`,
    { catalog_cell_id: CellID.Catalog; field_id: string }
  >;
  export type AddPlot = ActionBase<
    `add_plot`,
    { catalog_cell_id: CellID.Catalog; plot_id: PlotID }
  >;
  export type RemovePlot = ActionBase<
    `remove_plot`,
    { catalog_cell_id: CellID.Catalog; plot_id: PlotID }
  >;
  export type SetCatalog = ActionBase<
    `set_catalog`,
    { catalog_cell_id: CellID.Catalog; catalog_id: string }
  >;
  export type AddTableColumn = ActionBase<
    `add_table_column`,
    { catalog_cell_id: CellID.Catalog; field_id: string }
  >;
  export type RemoveTableColumn = ActionBase<
    `remove_table_column`,
    { catalog_cell_id: CellID.Catalog; field_id: string }
  >;
  export type SetDarkMode = ActionBase<
    `set_dark_mode`,
    { value: DarkModeValue }
  >;
  export type Any =
    | AddCatalogCell
    | AddComparisonCell
    | RemoveCell
    | AddFilter
    | RemoveFilter
    | AddPlot
    | RemovePlot
    | SetCatalog
    | AddTableColumn
    | RemoveTableColumn
    | SetDarkMode;
}

export type PlotID = `plot_${number}`;

export type DarkModeValue = `system` | `light` | `dark`;

type ActionBase<T extends string, U> = U & {
  type: T;
};

export namespace Cell {
  export type Catalog = {
    type: `catalog`;
    id: CellID.Catalog;
  };
  export type Comparison = {
    type: `comparison`;
    id: CellID.Comparison;
  };
  export type Any = Catalog | Comparison;
}

export namespace CellID {
  export type Catalog = `catalog_cell_${number}`;
  export type Comparison = `comparison_cell_${number}`;
  export type Any = Catalog | Comparison;
}

// ===========================================
// SCHEMA
// ===========================================

export type DataPostRequestBody = NonNullable<
  schema.operations["dataPOST"]["requestBody"]
>["content"]["application/json"];

export type HistogramPostRequestBody = NonNullable<
  schema.operations["histogramPOST"]["requestBody"]
>["content"]["application/json"];

export type HistogramResponse =
  schema.components["responses"]["histogram"]["content"]["application/json"];

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
