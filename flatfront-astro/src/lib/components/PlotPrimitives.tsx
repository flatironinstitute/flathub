import React from "react";
import { get_field_type } from "../shared";
import { useCatalogMetadata } from "../contexts/CatalogMetadataContext";
import { usePlotState, useSetPlotControl } from "../contexts/PlotContext";
import { Select, Checkbox } from "./Primitives";

export function LabelledPlotControl({
  label,
  plotControlKey: plot_control_key,
  placeholder,
  showLogSwitch = false
}: {
  label: string;
  plotControlKey: string;
  placeholder: string;
  showLogSwitch: boolean;
}) {
  return (
    <Labelled label={label}>
      <PlotControl
        plotControlkey={plot_control_key}
        placeholder={placeholder}
        showLogSwitch={showLogSwitch}
      />
    </Labelled>
  );
}

export function Labelled({
  label,
  children
}: {
  label: string;
  children: React.ReactNode;
}) {
  return (
    <div data-type="Labelled" className="space-y-2">
      <label className="block uppercase">{label}</label>
      {children}
    </div>
  );
}

function PlotControl({
  plotControlkey: plot_control_key,
  placeholder,
  showLogSwitch = false,
  debug = false
}: {
  plotControlkey: string;
  placeholder?: string;
  showLogSwitch?: boolean;
  debug?: boolean;
}) {
  const catalog_metadata = useCatalogMetadata();
  const all_leaf_nodes = catalog_metadata?.hierarchy?.leaves() ?? [];
  const numeric_nodes = all_leaf_nodes.filter((d) => {
    const type = get_field_type(d.data);
    return type === `INTEGER` || type === `FLOAT`;
  });

  const plot_state = usePlotState();
  const set_plot_control = useSetPlotControl();

  const field_id = plot_state?.[plot_control_key];

  const value = numeric_nodes.find((d) => d.data.name === field_id);

  let log_switch = null;

  if (showLogSwitch) {
    log_switch = (
      <div className="relative">
        <label className="absolute left-1/2 top-0 -translate-x-1/2 translate-y-[calc(-100%-5px)] uppercase leading-none">
          log
        </label>
        <LogModeCheckbox plotControlkey={plot_control_key} />
      </div>
    );
  }

  return (
    <div className="flex items-center gap-x-3">
      <Select
        size="small"
        placeholder={placeholder}
        options={numeric_nodes}
        value={value}
        getKey={(d) => d.data.name}
        getDisplayName={(d) => d.data.name}
        onValueChange={(d) => {
          const value = d?.data.name;
          set_plot_control(plot_control_key, value);
        }}
        triggerClassName="overflow-hidden text-ellipsis"
        valueClassName="overflow-hidden text-ellipsis"
      />
      {log_switch}
    </div>
  );
}

export function LogModeCheckbox({
  plotControlkey: plot_control_key
}: {
  plotControlkey: string;
}) {
  const plot_config = usePlotState();
  const log_mode_key = `${plot_control_key}_log_mode`;
  const is_log_mode = plot_config?.[log_mode_key] ?? false;
  const set_plot_control = useSetPlotControl();
  return (
    <Checkbox
      checked={is_log_mode}
      onCheckedChange={(checked) => {
        set_plot_control(log_mode_key, checked);
      }}
    />
  );
}
