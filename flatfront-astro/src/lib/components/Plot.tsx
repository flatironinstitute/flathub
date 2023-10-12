import type { DataPostRequestBody, DataResponse, PlotType } from "../types";

import * as hooks from "../hooks";
import { log, fetch_api_post, get_field_type } from "../shared";
import Highcharts from "highcharts";
import HighchartsExporting from "highcharts/modules/exporting";
import HighchartsExportData from "highcharts/modules/export-data";
import HighchartsReact from "highcharts-react-official";
import { useQuery } from "@tanstack/react-query";
import { CellSection, Placeholder, Select } from "./Primitives";
import * as controller from "./AppController";

HighchartsExporting(Highcharts);
HighchartsExportData(Highcharts);

function usePlotType() {
  const plot_id = hooks.usePlotID();
  const plot_type = controller.useState().set_plot_type[plot_id];
  return plot_type;
}

export default function PlotSection() {
  const plot_type = usePlotType();
  const plot_component = (() => {
    switch (plot_type) {
      case `scatterplot`:
        return <Scatterplot />;
      case `heatmap`:
        return <Placeholder>Heatmap</Placeholder>;
      default:
        return <Placeholder>Choose a plot type</Placeholder>;
    }
  })();
  return (
    <CellSection label="plot" className="space-y-4">
      <PlotControls />
      {plot_component}
    </CellSection>
  );
}

function PlotTypeSelect() {
  const plot_id = hooks.usePlotID();
  const plot_type = usePlotType();
  const plot_type_options = [
    { key: `scatterplot` as PlotType, label: `Scatterplot` },
    { key: `heatmap` as PlotType, label: `Heatmap` }
  ];
  const value = plot_type_options.find((d) => d.key === plot_type);
  const dispatch = controller.useDispatch();
  return (
    <Select
      placeholder="Choose plot type..."
      options={plot_type_options}
      getKey={(d) => d.key}
      getDisplayName={(d) => d.label}
      value={value}
      onValueChange={(d) => {
        const plot_type = d?.key;
        dispatch([`set_plot_type`, plot_id], plot_type);
      }}
    />
  );
}

function PlotControls() {
  const plot_type = usePlotType();
  const scatterplot_controls = (
    <>
      <label>X-axis</label>
      <PlotControl plot_control_key="x_axis" placeholder="Choose X-Axis..." />
      <label>Y-axis</label>
      <PlotControl plot_control_key="y_axis" placeholder="Choose Y-Axis..." />
    </>
  );
  const controls = (() => {
    switch (plot_type) {
      case `scatterplot`:
        return scatterplot_controls;
      case `heatmap`:
        return <Placeholder>Heatmap Controls</Placeholder>;
      default:
        return <Placeholder>Choose a plot type</Placeholder>;
    }
  })();
  return (
    <div className="grid grid-cols-[max-content_1fr] items-center gap-x-4 gap-y-4">
      <label className="whitespace-nowrap">Plot Type:</label>
      <PlotTypeSelect />
      {controls}
    </div>
  );
}

function PlotControl({
  plot_control_key,
  placeholder
}: {
  plot_control_key: `x_axis` | `y_axis`;
  placeholder?: string;
}) {
  const plot_id = hooks.usePlotID();
  const catalog_metadata = hooks.useCatalogMetadata();
  const all_leaf_nodes = catalog_metadata?.hierarchy?.leaves() ?? [];
  const numeric_nodes = all_leaf_nodes.filter((d) => {
    const type = get_field_type(d.data);
    return type === `INTEGER` || type === `FLOAT`;
  });

  const field_id =
    controller.useState().set_plot_control?.[plot_id]?.[plot_control_key];

  const dispatch = controller.useDispatch();

  const value = numeric_nodes.find((d) => d.data.name === field_id);

  return (
    <Select
      placeholder={placeholder}
      options={numeric_nodes}
      value={value}
      getKey={(d) => d.data.name}
      getDisplayName={(d) => d.data.name}
      onValueChange={(d) => {
        const value = d?.data.name;
        dispatch([`set_plot_control`, plot_id, plot_control_key], value);
      }}
    />
  );
}

function Scatterplot() {
  const catalog_id = hooks.useCatalogID();
  const filters = hooks.useFilters();

  const plot_id = hooks.usePlotID();
  const plot_state = controller.useState().set_plot_control?.[plot_id];

  const x_axis_field_id = plot_state.x_axis;
  const y_axis_field_id = plot_state.y_axis;
  const count = plot_state.count ?? 3e3;

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
    queryKey: [`plot-data`, query_config],
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
      }
    ],
    plotOptions: {
      scatter: {
        marker: {
          radius: 2,
          symbol: `circle`
        },
        animation: false
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
