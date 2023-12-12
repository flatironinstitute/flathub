import type * as schema from "./flathub-schema";

export type { schema };

export type AppState = {
  cells?: Record<number, Cell.Any>;
  add_filter?: Record<
    CellID.Catalog,
    Record<CatalogID, Record<FieldID, boolean>>
  >;
  set_filter_value?: Record<CellID.Catalog, Record<CatalogID, Filters>>;
  set_random_sample?: Record<
    CellID.Catalog,
    Record<CatalogID, { sample?: number; seed?: number }>
  >;
  set_catalog?: Record<CellID.Catalog, CatalogID>;
  add_plot?: Record<CellID.Catalog, Record<PlotID, boolean>>;
  set_plot_type?: Record<PlotID, PlotType>;
  set_plot_control?: Record<PlotID, Record<CatalogID, Record<string, any>>>;
  show_columns?: Record<
    CellID.Catalog,
    Record<CatalogID, Record<FieldID, boolean>>
  >;
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

export type PlotType = string;
export type PlotID = `plot_${number}`;
export type CatalogID = string;
export type FieldID = string;

export type CatalogMetadataWrapper = {
  response: CatalogResponse;
  hierarchy: CatalogHierarchyNode;
  depth_first: Array<CatalogHierarchyNode>;
  get_hash_from_node: (node: CatalogHierarchyNode) => string;
  get_node_from_hash: (hash: string) => CatalogHierarchyNode;
  initial_column_ids: Set<string>;
};

export type CatalogHierarchyNode = d3.HierarchyNode<FieldMetadata>;

// ===========================================
// SCHEMA
// ===========================================

export type CountResponse =
  schema.components[`responses`][`count`][`content`][`application/json`];

export type HistogramResponse =
  schema.components[`responses`][`histogram`][`content`][`application/json`];

export type CatalogResponse =
  schema.components[`responses`][`catalog`][`content`][`application/json`];

export type TopResponseEntry = TopResponse[number];

export type TopResponse =
  schema.components[`responses`][`top`][`content`][`application/json`];

export type CountRequestBody = NonNullable<
  schema.operations[`countPOST`][`requestBody`]
>[`content`][`application/json`];

export type DataPostRequestBody = NonNullable<
  schema.operations[`dataPOST`][`requestBody`]
>[`content`][`application/json`];

export type HistogramPostRequestBody = NonNullable<
  schema.operations[`histogramPOST`][`requestBody`]
>[`content`][`application/json`];

export type HistogramList = schema.components[`schemas`][`HistogramList`];

export type HistogramField = Exclude<
  schema.components[`schemas`][`Histogram`],
  string
>;

export type FilterValueRaw = Filters[string];

export type Filters = schema.components[`schemas`][`Filters`];

export type FieldMetadata = schema.components[`schemas`][`FieldGroup`];
