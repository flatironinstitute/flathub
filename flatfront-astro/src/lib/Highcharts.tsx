import type {
  Action,
  CellAction,
  CellID,
  CatalogMetadataWrapper,
  Filters,
  DataRequestBody,
  DataResponse,
  CatalogHierarchyNode,
  CatalogMetadataQuery,
  FieldGroup,
  CatalogResponse,
  Datum,
} from "./types";

import Highcharts from "highcharts";
import HighchartsReact from "highcharts-react-official";

export function Scatterplot({
  data,
  catalog_field_hierarchy,
}: {
  data: DataResponse;
  catalog_field_hierarchy: d3.HierarchyNode<FieldGroup>;
}) {
  const options: Highcharts.Options = {
    chart: {
      animation: false,
      styledMode: true,
    },
    title: {
      text: "My chart",
    },
    series: [
      {
        type: "scatter",
        animation: false,
        data: [1, 2, 3],
      },
    ],
  };

  return (
    <div>
      <HighchartsReact
        highcharts={Highcharts}
        options={options}
        containerProps={{ className: `highcharts-dark` }}
      />
    </div>
  );
}
