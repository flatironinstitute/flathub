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
  PlotControlAction,
} from "./types";

import Highcharts from "highcharts";
import HighchartsReact from "highcharts-react-official";
import * as stores from "./stores";
import {
  LabeledSelect,
  dispatch_action,
  useCellID,
  usePlotID,
  useData,
  useStore,
  log,
} from "./shared";

export function Scatterplot() {
  const cell_id = useCellID();
  const plot_id = usePlotID();
  const data = useData();
  const variable_names = Object.keys(data[0]);

  const all_actions = useStore(stores.actions);
  const plot_control_actions = all_actions.filter(
    (action): action is PlotControlAction =>
      action.type === `set_plot_control` &&
      action.cell_id === cell_id &&
      action.plot_id === plot_id
  );

  const plot_state = plot_control_actions.reduce((state, action) => {
    return { ...state, [action.key]: action.value };
  }, {} as { [key: string]: string });

  const x_axis_variable_name = plot_state.x_axis;
  const y_axis_variable_name = plot_state.y_axis;

  const data_munged = (() => {
    if (!x_axis_variable_name) return [];
    if (!y_axis_variable_name) return [];
    return data.map((datum) => {
      return [datum[x_axis_variable_name], datum[y_axis_variable_name]];
    });
  })();

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
        data: data_munged,
      },
    ],
  };

  return (
    <div>
      <LabeledSelect
        label="X Axis"
        options={variable_names}
        placeholder="Select a variable..."
        value={x_axis_variable_name}
        onValueChange={(value) => {
          dispatch_action({
            type: `set_plot_control`,
            cell_id,
            plot_id,
            key: `x_axis`,
            value,
          });
        }}
      />
      <LabeledSelect
        label="Y Axis"
        options={variable_names}
        placeholder="Select a variable..."
        value={y_axis_variable_name}
        onValueChange={(value) => {
          dispatch_action({
            type: `set_plot_control`,
            cell_id,
            plot_id,
            key: `y_axis`,
            value,
          });
        }}
      />
      <HighchartsReact
        highcharts={Highcharts}
        options={options}
        containerProps={{ className: `highcharts-dark` }}
      />
    </div>
  );
}
