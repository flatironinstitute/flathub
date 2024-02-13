import { useQuery } from "@tanstack/react-query";
import * as Plot from "@observablehq/plot";
import { type ColorScheme } from "@observablehq/plot";
import lodash_merge from "lodash.merge";
import { log, fetch_api_post, get_field_type, get_field_titles } from "@/utils";
import {
  useGetCurrentFilterMax,
  useGetCurrentFilterMin
} from "@/components/contexts/FiltersContext";
import { useCatalogMetadata } from "@/components/contexts/CatalogMetadataContext";
import {
  usePlotState,
  useSetPlotControl
} from "@/components/contexts/PlotContext";
import { FieldTitles } from "./FieldTitles";
import { Label } from "./ui/label";
import { useColumnIDs } from "./contexts/ColumnsContext";
import {
  Select,
  SelectContent,
  SelectGroup,
  SelectItem,
  SelectTrigger,
  SelectValue
} from "./ui/select";
import { Switch } from "./ui/switch";
import { Combobox } from "./Combobox";

export function PlotStatusWrapper({
  children,
  status: status_box
}: {
  children: React.ReactNode;
  status: React.ReactNode;
}) {
  const overlay = status_box ? (
    <div className="pointer-events-none absolute left-0 top-0 h-full w-full">
      {status_box}
    </div>
  ) : null;

  return (
    <div className="relative">
      {children}
      {overlay}
    </div>
  );
}

export function XAxisControl() {
  return (
    <PlotVariableControl
      label="X-Axis"
      plotControlKey="x_axis"
      placeholder="Choose X-Axis..."
      showLogSwitch={true}
    />
  );
}

export function YAxisControl() {
  return (
    <PlotVariableControl
      label="Y-Axis"
      plotControlKey="y_axis"
      placeholder="Choose Y-Axis..."
      showLogSwitch={true}
    />
  );
}

export function LogCountControl() {
  return (
    <LabelledThing label="Count">
      <LogModeCheckbox plotControlkey="count" />
      <Label>Count: Log Scale</Label>
    </LabelledThing>
  );
}

export function ColorSchemeControl({
  options,
  defaultScheme,
  showLogSwitch: show_log_switch = false
}: {
  options: ColorScheme[];
  defaultScheme: ColorScheme;
  showLogSwitch: boolean;
}) {
  const plot_state = usePlotState();
  const set_plot_control = useSetPlotControl();

  const label = `Color Scheme`;
  const plot_control_key = `color`;

  const color_value = plot_state?.[plot_control_key] ?? defaultScheme;

  const items = options.map((id) => {
    return (
      <SelectItem value={id} key={id}>
        {id}
      </SelectItem>
    );
  });

  return (
    <LabelledThing label={label}>
      <Select
        value={color_value}
        onValueChange={(value) => set_plot_control(plot_control_key, value)}
      >
        <SelectTrigger className="whitespace-nowrap text-[clamp(0.8rem,4.6cqi,1rem)]">
          <SelectValue placeholder={defaultScheme} />
        </SelectTrigger>
        <SelectContent position="popper">
          <SelectGroup>{items}</SelectGroup>
        </SelectContent>
      </Select>
      {show_log_switch ? <LogSwitch plotControlkey={plot_control_key} /> : null}
    </LabelledThing>
  );
}

export function PlotVariableControl({
  label,
  plotControlKey: plot_control_key,
  placeholder,
  showLogSwitch: show_log_switch = false
}: {
  label: string;
  plotControlKey: string;
  placeholder: string;
  showLogSwitch: boolean;
}) {
  const catalog_metadata = useCatalogMetadata();
  const column_ids = useColumnIDs();

  const all_leaf_nodes = catalog_metadata?.hierarchy?.leaves() ?? [];

  const plot_variable_nodes = all_leaf_nodes.filter((d) => {
    const type = get_field_type(d.data);
    const id = catalog_metadata.get_id_from_node(d);
    const is_numeric = type === `INTEGER` || type === `FLOAT`;
    return column_ids.has(id) && is_numeric;
  });

  const items = plot_variable_nodes.map((d) => {
    const field_id = catalog_metadata.get_id_from_node(d);
    return {
      key: field_id,
      value: get_field_titles(d).join(` `),
      label: (
        <div className="flex gap-x-2">
          <FieldTitles titles={get_field_titles(d)} />
        </div>
      ),
      onSelect: () => set_plot_control(plot_control_key, field_id)
    };
  });

  const plot_state = usePlotState();
  const set_plot_control = useSetPlotControl();

  const selected_field_id = plot_state?.[plot_control_key];

  return (
    <LabelledThing label={label}>
      <Combobox
        placeholder={placeholder}
        autoClose={true}
        value={
          selected_field_id ? (
            <FieldTitles
              titles={get_field_titles(
                catalog_metadata?.get_node_from_id(selected_field_id)
              )}
            />
          ) : null
        }
        items={items}
      />
      {show_log_switch ? <LogSwitch plotControlkey={plot_control_key} /> : null}
    </LabelledThing>
  );
}

function LabelledThing({
  label,
  children
}: {
  label: string;
  children: React.ReactNode;
}) {
  return (
    <div className="space-y-1">
      <Label>{label}</Label>
      <div className="flex h-10 items-center gap-x-2">{children}</div>
    </div>
  );
}

function LogSwitch({
  plotControlkey: plot_control_key
}: {
  plotControlkey: string;
}) {
  return (
    <div className="flex items-center gap-x-2">
      <LogModeCheckbox plotControlkey={plot_control_key} />
      <Label className="whitespace-nowrap">Log Scale</Label>
    </div>
  );
}

function LogModeCheckbox({
  plotControlkey: plot_control_key
}: {
  plotControlkey: string;
}) {
  const plot_config = usePlotState();
  const log_mode_key = `${plot_control_key}_log_mode`;
  const is_log_mode = plot_config?.[log_mode_key] ?? false;
  const set_plot_control = useSetPlotControl();
  return (
    <Switch
      checked={is_log_mode}
      onCheckedChange={(checked) => {
        set_plot_control(log_mode_key, checked);
      }}
    />
  );
}

export function get_observable_options(
  opts: Plot.PlotOptions = {}
): Plot.PlotOptions {
  const aspect = 640 / 400;
  const width = 700;
  const height = width / aspect;
  const base: Plot.PlotOptions = {
    className: `plot`,
    marginLeft: 60,
    marginBottom: 50,
    width,
    height,
    style: {
      overflow: `visible`,
      background: `transparent`,
      width: `100%`
    },
    grid: true,
    x: {
      tickFormat: `.4~g`
    },
    y: {
      tickFormat: `,.5~g`,
      ticks: 5
    }
  };
  return lodash_merge(base, opts);
}

export function get_highcharts_options(
  opts: Highcharts.Options = {}
): Highcharts.Options {
  const base: Highcharts.Options = {
    chart: {
      animation: false,
      styledMode: true
    },
    plotOptions: {
      series: {
        animation: false
      }
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
    tooltip: {
      animation: false
    },
    exporting: {
      enabled: true
    },
    boost: {
      enabled: true,
      useGPUTranslations: true,
      usePreallocated: true
    }
  };
  return lodash_merge(base, opts);
}

export function useAxisConfig(
  key: `x_axis` | `y_axis` | `z_axis` | `count` | `field`
) {
  const catalog_metadata = useCatalogMetadata();
  const plot_state = usePlotState();
  const is_log_allowed = useGetIsLogAllowed();
  const field_id = plot_state?.[key];
  const field_node = catalog_metadata?.get_node_from_id(field_id);
  const field_name = field_node?.data?.name ?? null;
  const log_mode_requested = plot_state?.[`${key}_log_mode`] ?? false;
  const log_mode_allowed = is_log_allowed(field_id);
  const log_mode_error = log_mode_requested && !log_mode_allowed;
  const log_mode = log_mode_requested && log_mode_allowed;
  const ready_for_request = Boolean(field_name) && !log_mode_error;
  const log_mode_error_message = log_mode_error
    ? `Log mode not allowed because "${field_name}" values cross zero.`
    : null;
  return {
    key,
    field_name,
    log_mode,
    log_mode_error_message,
    ready_for_request
  };
}

function useGetIsLogAllowed(): (field_id: string) => boolean {
  const get_current_min = useGetCurrentFilterMin();
  const get_current_max = useGetCurrentFilterMax();
  return (field_id: string): boolean => {
    if (!field_id) return true;
    if (field_id === `count`) return true;
    const current_min = get_current_min(field_id);
    const current_max = get_current_max(field_id);
    if (current_min > 0 && current_max > 0) return true;
    return false;
  };
}

export function usePlotQuery<RequestType, ResponseType>({
  path,
  body,
  label,
  enabled
}: {
  path: string;
  body: RequestType;
  label: string;
  enabled: boolean;
}) {
  const query_key = [`plot-data`, path, body];
  const query = useQuery({
    queryKey: query_key,
    queryFn: async ({ signal }): Promise<ResponseType> => {
      const response = await fetch_api_post<RequestType, ResponseType>(
        path,
        body,
        {
          signal
        }
      );
      log(`${label} query response`, response);
      return response;
    },
    enabled
  });
  return [query, query_key] as const;
}
