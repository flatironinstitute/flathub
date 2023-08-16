import type { PlotControlAction } from "../types";

import Highcharts from "highcharts";
import HighchartsReact from "highcharts-react-official";
import * as stores from "../stores";
import { LabeledSelect, dispatch_action, hooks } from "../shared";

export function Scatterplot() {
  const cell_id = hooks.useCellID();
  const plot_id = hooks.usePlotID();
  const data = hooks.useData();
  const field_ids = Object.keys(data[0]);

  const all_actions = hooks.useStore(stores.actions);
  const plot_control_actions = all_actions.filter(
    (action): action is PlotControlAction =>
      action.type === `set_plot_control` &&
      action.cell_id === cell_id &&
      action.plot_id === plot_id
  );

  const plot_state = plot_control_actions.reduce((state, action) => {
    return { ...state, [action.key]: action.value };
  }, {} as { [key: string]: string });

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
        options={field_ids}
        placeholder="Select a variable..."
        value={x_axis_field_id}
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
        options={field_ids}
        placeholder="Select a variable..."
        value={y_axis_field_id}
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
