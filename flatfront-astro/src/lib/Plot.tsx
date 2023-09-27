import type { DataPostRequestBody, DataResponse, DataRow } from "./types";

import React from "react";
import * as hooks from "./hooks";
import {
  dispatch_action,
  assert_catalog_cell_id,
  log,
  fetch_api_post
} from "./shared";
import Highcharts from "highcharts";
import HighchartsExporting from "highcharts/modules/exporting";
import HighchartsExportData from "highcharts/modules/export-data";
import HighchartsReact from "highcharts-react-official";
import { useQuery } from "@tanstack/react-query";
import { BigButton, CellSection, CellWrapper, Placeholder } from "./Primitives";

HighchartsExporting(Highcharts);
HighchartsExportData(Highcharts);

export default function PlotSection() {
  const plot_component = <Scatterplot />;
  return (
    <CellSection label="plot" className="space-y-4">
      {plot_component}
    </CellSection>
  );
}

function Scatterplot() {
  const catalog_id = hooks.useCatalogID();
  const filters = hooks.useFilters();

  const plot_state = {
    x_axis: `Group_MassType_gas`,
    y_axis: `Group_MassType_dm`,
    count: 1e3
  };

  const x_axis_field_id = plot_state.x_axis;
  const y_axis_field_id = plot_state.y_axis;
  const count = plot_state.count;

  const request_body: DataPostRequestBody = {
    object: true,
    fields: [x_axis_field_id, y_axis_field_id],
    ...filters,
    count
    // ...query_parameters
  };

  const query_config = {
    path: `/${catalog_id}/data`,
    body: request_body
  };

  const enable_request =
    Boolean(catalog_id) && Boolean(x_axis_field_id) && Boolean(y_axis_field_id);

  const query = useQuery({
    queryKey: [`plot-data`],
    queryFn: async (): Promise<DataResponse> => {
      return fetch_api_post<DataPostRequestBody, DataResponse>(
        query_config.path,
        query_config.body
      ).then((response) => {
        log(`query response`, response);
        return response;
      });
    },
    enabled: enable_request,
    keepPreviousData: true,
    staleTime: Infinity
  });

  const data = query.data ?? [];

  const data_munged = (() => {
    if (!x_axis_field_id) return [];
    if (!y_axis_field_id) return [];
    return data.map((datum) => {
      return [datum[x_axis_field_id], datum[y_axis_field_id]];
    });
  })();

  const options: Highcharts.Options = {
    chart: {
      animation: false,
      styledMode: true
    },
    legend: {
      enabled: false
    },
    title: {
      text: undefined
    },
    credits: {
      enabled: false
    },
    xAxis: {
      title: {
        text: x_axis_field_id
      },
      gridLineWidth: 1
    },
    yAxis: {
      title: {
        text: y_axis_field_id
      }
    },
    tooltip: {
      animation: false
    },
    exporting: {
      enabled: true
    },
    series: [
      {
        type: `scatter`,
        data: data_munged
        // className: `blarm`
        // color: `red`
      }
    ],
    plotOptions: {
      scatter: {
        marker: {
          radius: 3,
          symbol: `circle`
        },
        animation: false
        // opacity: 0.5
        // color: `red`
        // opacity: 0.5,
        // colorIndex: 3
        // color: `blue`
        // className: `[&_path]:fill-white [&_path]:stroke-none`
      }
    }
  };

  const dark_mode = hooks.useIsDarkMode();

  const className = dark_mode ? `highcharts-dark` : `highcharts-light`;

  return (
    <HighchartsReact
      highcharts={Highcharts}
      options={options}
      containerProps={{ className }}
    />
  );
}
