import type {
  Action,
  DataPostRequestBody,
  DataResponse,
  PlotType
} from "../types";

import * as hooks from "../hooks";
import { log, fetch_api_post, dispatch_action } from "../shared";
import Highcharts from "highcharts";
import HighchartsExporting from "highcharts/modules/exporting";
import HighchartsExportData from "highcharts/modules/export-data";
import HighchartsReact from "highcharts-react-official";
import { useQuery } from "@tanstack/react-query";
import { CellSection, Placeholder, Select } from "./Primitives";

HighchartsExporting(Highcharts);
HighchartsExportData(Highcharts);

function usePlotActions() {
  const all_actions = hooks.useActions();
  const plot_id = hooks.usePlotID();
  const plot_actions = all_actions.filter(
    (action): action is Action.SetPlotType | Action.SetPlotControl => {
      if (
        !(action.type === `set_plot_type` || action.type === `set_plot_control`)
      )
        return false;
      return action.plot_id === plot_id;
    }
  );
  return plot_actions;
}

export default function PlotSection() {
  const plot_actions = usePlotActions();
  const plot_id = hooks.usePlotID();
  const plot_type = plot_actions
    .filter(
      (action): action is Action.SetPlotType => action.type === `set_plot_type`
    )
    .at(-1)?.plot_type;
  const plot_type_options = [
    { key: `scatterplot` as PlotType, label: `Scatterplot` },
    { key: `heatmap` as PlotType, label: `Heatmap` }
  ];
  const value = plot_type_options.find((d) => d.key === plot_type);
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
      <div className="flex items-center gap-x-4">
        <label className="whitespace-nowrap">Plot Type:</label>
        <Select
          placeholder="Choose plot type..."
          options={plot_type_options}
          getKey={(d) => d.key}
          getDisplayName={(d) => d.label}
          value={value}
          onValueChange={(d) => {
            const action: Action.SetPlotType = {
              type: `set_plot_type`,
              plot_id,
              plot_type: d?.key
            };
            dispatch_action(action);
          }}
        />
      </div>
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
