import type { DataRequestBody, DataResponse, Actions } from "../types";
import type { QueryObserver } from "@tanstack/query-core";

import React from "react";

import clsx from "clsx";

import { FieldTitles, Select, useQueryObserver } from "../shared";
import { FieldCardWrapper, QueryParameter } from "./FieldCard";

import {
  BigButton,
  create_query_observer,
  dispatch_action,
  fetch_api_post,
  hooks,
  log,
  get_field_id,
  CellSection,
  PendingBox
} from "../shared";
import * as stores from "../stores";
import Highcharts from "highcharts";
import HighchartsReact from "highcharts-react-official";

export default function PlotSections() {
  // const cell_id = hooks.useCell().cell_id;
  // const catalog_id = hooks.useStore(stores.catalog_id_by_cell_id).get(cell_id);
  // const filters = hooks.useStore(stores.filters_by_cell_id).get(cell_id);
  // const query_parameters = hooks
  //   .useStore(stores.query_parameters_by_result_id)
  //   .get(cell_id);
  // const plot_state = usePlotState();
  // const x_axis_field_id = plot_state?.x_axis;
  // const y_axis_field_id = plot_state?.y_axis;
  // const request_body: DataRequestBody = {
  //   object: true,
  //   fields: [x_axis_field_id, y_axis_field_id],
  //   count: query_parameters?.count ?? 2000,
  //   ...filters
  // };
  // const query_config = {
  //   path: `/${catalog_id}/data`,
  //   body: request_body
  // };
  // const [data_query_observer, set_data_query_observer] =
  //   React.useState<QueryObserver<DataResponse> | null>(null);
  // const fetch_data = React.useCallback(() => {
  //   const observer = create_query_observer<DataResponse>({
  //     staleTime: Infinity,
  //     queryKey: [`data`, query_config],
  //     queryFn: async (): Promise<DataResponse> => {
  //       return fetch_api_post<DataResponse>(
  //         query_config.path,
  //         query_config.body
  //       ).then((response) => {
  //         log(`query response`, response);
  //         return response;
  //       });
  //     }
  //   });
  //   set_data_query_observer(observer);
  // }, [query_config]);
  // const data_query = useQueryObserver(data_query_observer);
  // const catalog_hierarchy = hooks
  //   .useStore(stores.catalog_metadata_wrapper_by_catalog_id)
  //   .get(catalog_id)?.hierarchy;
  // const fetching = data_query?.isFetching;
  // const data = data_query?.data ?? null;
  // const has_data = data?.length && data?.length > 0;
  // const is_ready = has_data && catalog_hierarchy;
  // const plot_component = (() => {
  //   if (!is_ready) {
  //     const status_message = fetching ? `Loading Data...` : `No Data Loaded`;
  //     return <PendingBox>{status_message}</PendingBox>;
  //   }
  //   return <Plot data={data} />;
  // })();
  // return (
  //   <div className="space-y-4">
  //     <CellSection label="plot type">
  //       <PlotTypeSelect />
  //     </CellSection>
  //     <CellSection label="variables">
  //       <PlotVariables />
  //     </CellSection>
  //     <CellSection label="query parameters">
  //       <PlotQueryParameters />
  //     </CellSection>
  //     <CellSection label="fetch">
  //       <BigButton onClick={() => fetch_data()}>
  //         {fetching ? `Fetching Data...` : `Fetch Data`}
  //       </BigButton>
  //     </CellSection>
  //     <CellSection label="plot">{plot_component}</CellSection>
  //   </div>
  // );
}

function PlotTypeSelect() {
  const cell_id = hooks.useCell().cell_id;
  const plot_type = hooks
    .useStore(stores.actions_by_cell_id)
    .get(cell_id)
    .filter((d): d is Actions[`SetPlotType`] => d.type === `set_plot_type`)
    .at(-1)?.plot_type;
  return (
    <div data-type="PlotTypeSelect">
      <Select
        placeholder={`Select a plot type...`}
        options={[`scatterplot`, `heatmap`]}
        value={plot_type}
        getDisplayName={(d) => {
          switch (d) {
            case `scatterplot`:
              return `Scatterplot`;
            case `heatmap`:
              return `Heatmap`;
          }
        }}
        onValueChange={(d) => {
          dispatch_action({
            type: `set_plot_type`,
            cell_id,
            plot_type: d
          } as Actions[`SetPlotType`]);
        }}
      />
    </div>
  );
}

function PlotVariables() {
  return (
    <div className="space-y-4">
      <PlotVariable label="X Axis" plot_key="x_axis" />
      <PlotVariable label="Y Axis" plot_key="y_axis" />
    </div>
  );
}

function PlotVariable({ label, plot_key }) {
  const cell_id = hooks.useCell().cell_id;
  const catalog_id = hooks.useStore(stores.catalog_id_by_cell_id).get(cell_id);
  const catalog_metadata = hooks
    .useStore(stores.catalog_metadata_wrapper_by_catalog_id)
    .get(catalog_id);
  const all_field_nodes = catalog_metadata?.nodes_array ?? [];
  const field_nodes = all_field_nodes.filter((d) => !d.children);

  const cell_actions = hooks.useStore(stores.actions_by_cell_id).get(cell_id);
  const plot_control_actions = cell_actions.filter(
    (action): action is Actions[`SetPlotControl`] =>
      action.type === `set_plot_control` && action.cell_id === cell_id
  );

  const field_id = plot_control_actions.filter((d) => d.key === plot_key).at(-1)
    ?.value;

  const value = field_nodes.find((d) => get_field_id(d.data) === field_id);

  return (
    <div
      data-type="PlotVariable"
      className={clsx(
        FieldCardWrapper.className,
        `grid gap-x-4 desktop:grid-cols-[10ch_1fr_1fr] desktop:items-center`
      )}
    >
      <Select
        label={label}
        options={field_nodes}
        getKey={(d) => get_field_id(d.data)}
        getDisplayName={(d) => <FieldTitles node={d} />}
        placeholder="Select a variable..."
        value={value}
        onValueChange={(node) => {
          dispatch_action({
            type: `set_plot_control`,
            cell_id,
            key: plot_key,
            value: get_field_id(node.data)
          });
        }}
      />
    </div>
  );
}

function usePlotState() {
  const cell_id = hooks.useCell().cell_id;
  const cell_actions = hooks.useStore(stores.actions_by_cell_id).get(cell_id);
  const plot_control_actions = cell_actions.filter(
    (action): action is Actions[`SetPlotControl`] =>
      action.type === `set_plot_control` && action.cell_id === cell_id
  );
  const plot_state = plot_control_actions.reduce(
    (state, action) => {
      return { ...state, [action.key]: action.value };
    },
    {} as { [key: string]: string }
  );
  return plot_state;
}

// function PlotQueryParameters() {
//   return (
//     <>
//       <QueryParameter label="Count" field_id="count" min={10} max={10_000} />
//     </>
//   );
// }

function Plot({ data }) {
  const plot_state = usePlotState();

  const x_axis_field_id = plot_state.x_axis;
  const y_axis_field_id = plot_state.y_axis;

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
    xAxis: {
      title: {
        text: x_axis_field_id
      }
    },
    yAxis: {
      title: {
        text: y_axis_field_id
      }
    },
    series: [
      {
        type: "scatter",
        animation: false,
        data: data_munged
      }
    ]
  };

  return (
    <HighchartsReact
      highcharts={Highcharts}
      options={options}
      containerProps={{ className: `highcharts-dark` }}
    />
  );
}
