import type * as schema from "./flathub-schema";

export type { schema };

export type AppState = {
  cells?: Record<CatalogCellID, CatalogCell>;
  cells_order?: Array<CatalogCellID>;
  filter_values?: Record<CatalogCellID, Filters>;
  random_sample?: Record<CatalogCellID, { sample?: number; seed?: number }>;
  plots?: Record<CatalogCellID, Record<PlotID, PlotType>>;
  plot_controls?: Record<PlotID, Record<string, any>>;
};

export type CatalogCell = {
  id?: CatalogCellID;
  catalog_id?: CatalogID;
  show_columns?: Record<FieldID, boolean>;
  show_filters?: Record<FieldID, boolean>;
};
export type CatalogCellID = `catalog_cell_${number}`;

export type PlotWrapper = {
  key: PlotType;
  label: string;
  order: number;
  Plot: React.FC;
  Controls: React.FC;
};

export type PlotType = string;
export type PlotID = `plot_${number}`;
export type CatalogID = string;
export type FieldID = string;

export type CatalogMetadataWrapper = {
  response: CatalogResponse;
  hierarchy: CatalogHierarchyNode;
  depth_first: Array<CatalogHierarchyNode>;
  get_id_from_node: (node: CatalogHierarchyNode) => string;
  get_node_from_id: (id: string) => CatalogHierarchyNode;
  initial_column_ids: Set<string>;
  initial_filter_ids: Set<string>;
};

export type CatalogHierarchyNode = d3.HierarchyNode<FieldMetadata>;

export type FieldType =
  | `ROOT`
  | `INTEGER`
  | `FLOAT`
  | `LABELLED_ENUMERABLE_BOOLEAN`
  | `LABELLED_ENUMERABLE_INTEGER`
  | `ENUMERABLE_INTEGER`
  | `ARRAY`
  | `STRING`;

export type DataResponse = Array<DataRow>;
export type DataRow = Record<string, any>;

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

export type NumericFilterValue = {
  // gte?: schema.components["schemas"]["FieldValue"];
  // lte?: schema.components["schemas"]["FieldValue"];
  gte?: number;
  lte?: number;
};

export type FilterValueRaw = Filters[string];

export type Filters = schema.components[`schemas`][`Filters`];

export type FieldMetadata = schema.components[`schemas`][`FieldGroup`];
