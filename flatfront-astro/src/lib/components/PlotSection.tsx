import type {
  DataPostRequestBody,
  DataResponse,
  HistogramPostRequestBody,
  HistogramResponse,
  PlotID,
  PlotType
} from "../types";
import React from "react";
import { useQuery } from "@tanstack/react-query";
import { log, fetch_api_post, get_field_type } from "../shared";
import { useIsDarkMode } from "../dark-mode";
import { useCatalogID } from "../contexts/CatalogContext";
import { useRandomConfig } from "../contexts/RandomContext";
import { useFilters } from "../contexts/FiltersContext";
import { useCatalogMetadata } from "../contexts/CatalogMetadataContext";
import { useAppState, useMergeState } from "../contexts/AppStateContext";
import {
  PlotIDProvider,
  useRemovePlot,
  usePlotID,
  usePlotType
} from "../contexts/PlotContext";
import {
  Select,
  Checkbox,
  Placeholder,
  SimpleLabel,
  StatusBox
} from "./Primitives";
import HighchartsPlot from "./HighchartsPlot";

export default function PlotSection({ id }: { id: PlotID }) {
  const remove_plot = useRemovePlot(id);
  return (
    <PlotIDProvider value={id}>
      <div className="flex flex-col gap-y-4 @container/plot">
        <div className="flex justify-between">
          <SimpleLabel>plot</SimpleLabel>
          <button
            className="cursor-pointer underline"
            onClick={() => remove_plot()}
          >
            Remove
          </button>
        </div>
        <PlotComponent />
        <div className="grid grid-cols-1 items-center gap-x-8 gap-y-4 @xl/plot:grid-cols-2 @3xl/plot:grid-cols-3">
          <Labelled label="Plot Type">
            <PlotTypeSelect />
          </Labelled>
          <PlotControls />
        </div>
      </div>
    </PlotIDProvider>
  );
}

function PlotComponent() {
  const plot_type = usePlotType();
  const plot_component = (() => {
    switch (plot_type) {
      case `histogram`:
        return <Histogram />;
      case `heatmap`:
        return <Heatmap />;
      case `scatterplot`:
        return <Scatterplot />;
      case `scatterplot_3d`:
        return <Scatterplot3D />;
      default:
        return <Placeholder>Choose a plot type</Placeholder>;
    }
  })();
  return plot_component;
}

function PlotControls() {
  const plot_type = usePlotType();

  const log_count_switch = (
    <Labelled label="Count: Log Scale">
      <LogModeCheckbox plotControlkey="count" />
    </Labelled>
  );

  const x_axis_control = (
    <LabelledPlotControl
      label="X-Axis"
      plotControlKey="x_axis"
      placeholder="Choose X-Axis..."
      showLogSwitch={true}
    />
  );

  const y_axis_control = (
    <LabelledPlotControl
      label="Y-Axis"
      plotControlKey="y_axis"
      placeholder="Choose Y-Axis..."
      showLogSwitch={true}
    />
  );

  const histogram_controls = (
    <>
      <LabelledPlotControl
        label="Field"
        plotControlKey="field"
        placeholder="Choose field..."
        showLogSwitch={true}
      />
      {log_count_switch}
    </>
  );

  const heatmap_controls = (
    <>
      {x_axis_control}
      {y_axis_control}
    </>
  );

  const scatterplot_controls = (
    <>
      {x_axis_control}
      {y_axis_control}
    </>
  );

  const scatterplot_3d_controls = (
    <>
      {x_axis_control}
      {y_axis_control}
      <LabelledPlotControl
        label="Z-Axis"
        plotControlKey="z_axis"
        placeholder="Choose Z-Axis..."
        showLogSwitch={true}
      />
    </>
  );

  const controls = (() => {
    switch (plot_type) {
      case `histogram`:
        return histogram_controls;
      case `heatmap`:
        return heatmap_controls;
      case `scatterplot`:
        return scatterplot_controls;
      case `scatterplot_3d`:
        return scatterplot_3d_controls;
      default:
        return null;
    }
  })();

  return controls;
}

function LabelledPlotControl({
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

function Labelled({
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

function PlotTypeSelect() {
  const plot_id = usePlotID();
  const plot_type = usePlotType();
  const plot_type_options = [
    { key: `histogram` as PlotType, label: `Histogram` },
    { key: `heatmap` as PlotType, label: `Heatmap` },
    { key: `scatterplot` as PlotType, label: `Scatterplot` },
    { key: `scatterplot_3d` as PlotType, label: `3D Scatterplot` }
  ];
  const value = plot_type_options.find((d) => d.key === plot_type);
  const merge_state = useMergeState();
  return (
    <Select
      placeholder="Choose plot type..."
      options={plot_type_options}
      getKey={(d) => d.key}
      getDisplayName={(d) => d.label}
      value={value}
      onValueChange={(d) => {
        const plot_type = d?.key;
        merge_state({
          set_plot_type: {
            [plot_id]: plot_type
          }
        });
      }}
      size="small"
    />
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
  const plot_id = usePlotID();
  const catalog_metadata = useCatalogMetadata();
  const all_leaf_nodes = catalog_metadata?.hierarchy?.leaves() ?? [];
  const numeric_nodes = all_leaf_nodes.filter((d) => {
    const type = get_field_type(d.data);
    return type === `INTEGER` || type === `FLOAT`;
  });

  const plot_config = useAppState().set_plot_control?.[plot_id];

  const field_id = plot_config?.[plot_control_key];

  const merge_state = useMergeState();

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
          merge_state({
            set_plot_control: {
              [plot_id]: {
                [plot_control_key]: value
              }
            }
          });
        }}
        triggerClassName="overflow-hidden text-ellipsis"
        valueClassName="overflow-hidden text-ellipsis"
      />
      {log_switch}
    </div>
  );
}

function LogModeCheckbox({
  plotControlkey: plot_control_key
}: {
  plotControlkey: string;
}) {
  const plot_id = usePlotID();
  const plot_config = useAppState().set_plot_control?.[plot_id];
  const log_mode_key = `${plot_control_key}_log_mode`;
  const is_log_mode = plot_config?.[log_mode_key] ?? false;
  const merge_state = useMergeState();
  return (
    <Checkbox
      checked={is_log_mode}
      onCheckedChange={(checked) => {
        merge_state({
          set_plot_control: {
            [plot_id]: {
              [log_mode_key]: checked
            }
          }
        });
      }}
    />
  );
}

function Histogram() {
  const catalog_id = useCatalogID();
  const filters = useFilters();
  const random_config = useRandomConfig();
  const plot_state = usePlotState();
  const is_log_allowed = useGetIsLogAllowed();

  const field_id = plot_state?.field;
  const log_mode = plot_state?.field_log_mode ?? false;
  const count_log_mode = plot_state?.count_log_mode ?? false;

  const can_use_log_mode = is_log_allowed(field_id);

  const log_mode_error = log_mode && !can_use_log_mode;

  const enable_request =
    Boolean(catalog_id) && Boolean(field_id) && !log_mode_error;

  const query = usePlotQuery<HistogramPostRequestBody, HistogramResponse>({
    path: `/${catalog_id}/histogram`,
    body: {
      fields: [
        {
          field: field_id,
          size: 100,
          log: log_mode
        }
      ] as any,
      ...filters,
      ...random_config
    },
    label: `Histogram`,
    enabled: enable_request
  });

  const data_munged = (() => {
    if (!field_id) return [];
    if (!query.data) return [];
    return query.data.buckets.map(({ key, count }) => {
      return [...key, count];
    });
  })();

  const options: Highcharts.Options = {
    ...get_highcharts_options(),
    xAxis: {
      type: log_mode ? `logarithmic` : `linear`,
      title: {
        text: field_id
      }
    },
    yAxis: {
      type: count_log_mode ? `logarithmic` : `linear`,
      title: {
        text: `Count`
      }
    },
    series: [
      {
        type: `column`,
        name: `Count`,
        data: data_munged,
        animation: false,
        borderRadius: 0
      }
    ]
  };

  const status = (() => {
    if (log_mode_error) {
      return `Log mode not allowed because "${field_id}" values cross zero.`;
    } else if (query.isFetching) {
      return `Loading...`;
    } else if (!(data_munged.length > 0)) {
      return `No data.`;
    } else {
      return null;
    }
  })();

  return (
    <div className="relative">
      {status && <StatusBox>{status}</StatusBox>}
      <HighchartsPlot options={options} />
    </div>
  );
}

function Heatmap() {
  const catalog_id = useCatalogID();
  const filters = useFilters();
  const random_config = useRandomConfig();
  const plot_state = usePlotState();

  const x_axis_field_id = plot_state?.x_axis;
  const y_axis_field_id = plot_state?.y_axis;

  const enable_request =
    Boolean(catalog_id) && Boolean(x_axis_field_id) && Boolean(y_axis_field_id);

  const fields: any = [
    { field: x_axis_field_id, size: 30 },
    { field: y_axis_field_id, size: 30 }
  ];

  const request_body: HistogramPostRequestBody = {
    fields,
    ...filters,
    ...random_config
  };

  const query = usePlotQuery<HistogramPostRequestBody, HistogramResponse>({
    path: `/${catalog_id}/histogram`,
    body: request_body,
    label: `Heatmap`,
    enabled: enable_request
  });

  const data = query.data;

  const data_munged = (() => {
    if (!x_axis_field_id) return [];
    if (!y_axis_field_id) return [];
    if (!data) return [];
    return data.buckets.map(({ key, count }) => {
      return [...key, count];
    });
  })();

  const is_dark_mode = useIsDarkMode();

  const options: Highcharts.Options = {
    ...get_highcharts_options(),
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
    colorAxis: {
      minColor: is_dark_mode ? `black` : `white`,
      maxColor: is_dark_mode ? `white` : `black`
    },
    series: [
      {
        type: `heatmap`,
        data: data_munged,
        colsize: data?.sizes[0],
        rowsize: data?.sizes[1],
        boostThreshold: 1
      }
    ]
  };

  const has_data = data_munged.length > 0;

  const status = query.isFetching
    ? `Loading...`
    : !has_data
    ? `No data.`
    : null;

  return (
    <div className="relative">
      {status && <StatusBox>{status}</StatusBox>}
      <HighchartsPlot options={options} />
    </div>
  );
}

function Scatterplot() {
  const catalog_id = useCatalogID();
  const filters = useFilters();
  const plot_state = usePlotState();
  const is_log_allowed = useGetIsLogAllowed();

  const x_axis_field_id = plot_state?.x_axis;
  const y_axis_field_id = plot_state?.y_axis;

  const x_axis_log_mode = plot_state?.x_axis_log_mode ?? false;
  const y_axis_log_mode = plot_state?.y_axis_log_mode ?? false;

  const x_axis_can_use_log_mode = is_log_allowed(x_axis_field_id);
  const y_axis_can_use_log_mode = is_log_allowed(y_axis_field_id);

  const x_axis_log_mode_error = x_axis_log_mode && !x_axis_can_use_log_mode;
  const y_axis_log_mode_error = y_axis_log_mode && !y_axis_can_use_log_mode;

  const count = plot_state?.count ?? 2e3;

  // TODO: What's the best way of doing this
  // const total_rows = useCatalogMetadata()?.response?.count;
  // const sample = Math.min((count * 10) / total_rows, 1);
  const sample = 0.9999;

  const enable_request =
    Boolean(catalog_id) &&
    Boolean(x_axis_field_id) &&
    Boolean(y_axis_field_id) &&
    Number.isFinite(sample) &&
    !x_axis_log_mode_error &&
    !y_axis_log_mode_error;

  const query = usePlotQuery<DataPostRequestBody, DataResponse>({
    path: `/${catalog_id}/data`,
    body: {
      object: true,
      fields: [x_axis_field_id, y_axis_field_id],
      ...filters,
      count,
      sample
    },
    label: `Scatterplot`,
    enabled: enable_request
  });

  const data_munged = (() => {
    if (!x_axis_field_id) return [];
    if (!y_axis_field_id) return [];
    if (!query.data) return [];
    return query.data.map((datum) => {
      return [+datum[x_axis_field_id], +datum[y_axis_field_id]];
    });
  })();

  const options: Highcharts.Options = {
    ...get_highcharts_options(),
    xAxis: {
      type:
        x_axis_log_mode && x_axis_can_use_log_mode ? `logarithmic` : `linear`,
      title: {
        text: x_axis_field_id
      },
      gridLineWidth: 1
    },
    yAxis: {
      type:
        y_axis_log_mode && y_axis_can_use_log_mode ? `logarithmic` : `linear`,
      title: {
        text: y_axis_field_id
      }
    },
    series: [
      {
        type: `scatter`,
        marker: {
          radius: 1
        },
        data: data_munged,
        boostThreshold: 1
      }
    ]
  };

  const status = (() => {
    if (x_axis_log_mode_error) {
      return `Log mode not allowed because "${x_axis_field_id}" values cross zero.`;
    } else if (y_axis_log_mode_error) {
      return `Log mode not allowed because "${y_axis_field_id}" values cross zero.`;
    } else if (query.isFetching) {
      return `Loading...`;
    } else if (!(data_munged.length > 0)) {
      return `No data.`;
    } else {
      return null;
    }
  })();

  return <StatusWrapper status={status} options={options} />;
}

function Scatterplot3D() {
  const catalog_id = useCatalogID();
  const filters = useFilters();
  const random_config = useRandomConfig();
  const plot_state = usePlotState();
  const is_log_allowed = useGetIsLogAllowed();

  const x_axis_field_id = plot_state?.x_axis;
  const y_axis_field_id = plot_state?.y_axis;
  const z_axis_field_id = plot_state?.z_axis;

  const x_axis_log_mode = plot_state?.x_axis_log_mode ?? false;
  const y_axis_log_mode = plot_state?.y_axis_log_mode ?? false;
  const z_axis_log_mode = plot_state?.z_axis_log_mode ?? false;

  const x_axis_can_use_log_mode = is_log_allowed(x_axis_field_id);
  const y_axis_can_use_log_mode = is_log_allowed(y_axis_field_id);
  const z_axis_can_use_log_mode = is_log_allowed(z_axis_field_id);

  const x_axis_log_mode_error = x_axis_log_mode && !x_axis_can_use_log_mode;
  const y_axis_log_mode_error = y_axis_log_mode && !y_axis_can_use_log_mode;
  const z_axis_log_mode_error = z_axis_log_mode && !z_axis_can_use_log_mode;

  const count = plot_state?.count ?? 2e3;

  // TODO: What's the best way of doing this
  // const total_rows = useCatalogMetadata()?.response?.count;
  // const sample = Math.min((count * 10) / total_rows, 1);
  const sample = 0.9999;

  const enable_request =
    Boolean(catalog_id) &&
    Boolean(x_axis_field_id) &&
    Boolean(y_axis_field_id) &&
    Boolean(z_axis_field_id) &&
    Number.isFinite(sample) &&
    !x_axis_log_mode_error &&
    !y_axis_log_mode_error &&
    !z_axis_log_mode_error;

  const query = usePlotQuery<DataPostRequestBody, DataResponse>({
    path: `/${catalog_id}/data`,
    body: {
      object: true,
      fields: [x_axis_field_id, y_axis_field_id, z_axis_field_id],
      ...filters,
      count,
      sample
    },
    label: `3D Scatterplot`,
    enabled: enable_request
  });

  const data_munged = (() => {
    if (!x_axis_field_id) return [];
    if (!y_axis_field_id) return [];
    if (!z_axis_field_id) return [];
    if (!query.data) return [];
    return query.data.map((datum) => {
      return [
        +datum[x_axis_field_id],
        +datum[y_axis_field_id],
        +datum[z_axis_field_id]
      ];
    });
  })();

  const options: Highcharts.Options = {
    ...get_highcharts_options(),
    xAxis: {
      type:
        x_axis_log_mode && x_axis_can_use_log_mode ? `logarithmic` : `linear`,
      title: {
        text: x_axis_field_id
      },
      gridLineWidth: 1
    },
    yAxis: {
      type:
        y_axis_log_mode && y_axis_can_use_log_mode ? `logarithmic` : `linear`,
      title: {
        text: y_axis_field_id
      }
    },
    zAxis: {
      type:
        z_axis_log_mode && z_axis_can_use_log_mode ? `logarithmic` : `linear`,
      title: {
        text: z_axis_field_id
      }
    },
    series: [
      {
        type: `scatter3d`,
        marker: {
          radius: 1
        },
        data: data_munged,
        turboThreshold: 0
      }
    ]
  };

  options.chart.options3d = {
    enabled: true,
    alpha: 10,
    beta: 20,
    depth: 400,
    drag: {
      enabled: true
    }
  };

  const status = (() => {
    if (x_axis_log_mode_error) {
      return `Log mode not allowed because "${x_axis_field_id}" values cross zero.`;
    } else if (y_axis_log_mode_error) {
      return `Log mode not allowed because "${y_axis_field_id}" values cross zero.`;
    } else if (z_axis_log_mode_error) {
      return `Log mode not allowed because "${z_axis_field_id}" values cross zero.`;
    } else if (query.isFetching) {
      return `Loading...`;
    } else if (!(data_munged.length > 0)) {
      return `No data.`;
    } else {
      return null;
    }
  })();

  return <StatusWrapper status={status} options={options} />;
}

function StatusWrapper({
  status,
  options
}: {
  status: React.ReactNode;
  options: Highcharts.Options;
}) {
  return (
    <div className="relative">
      {status && <StatusBox>{status}</StatusBox>}
      <HighchartsPlot options={options} />
    </div>
  );
}

function get_highcharts_options(opts?: {
  chart: Highcharts.Options["chart"];
}): Highcharts.Options {
  return {
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
}

function usePlotState() {
  const plot_id = usePlotID();
  const plot_state = useAppState()?.set_plot_control?.[plot_id];
  return plot_state;
}

function useGetIsLogAllowed(): (field_id: string) => boolean {
  const get_current_min = useGetCurrentMin();
  const get_current_max = useGetCurrentMax();
  return (field_id: string) => {
    const current_min = get_current_min(field_id);
    const current_max = get_current_max(field_id);
    if (!field_id) return true;
    if (current_min > 0 && current_max > 0) return true;
    return false;
  };
}

function useGetCurrentMin(): (field_id: string) => number | null {
  const filters = useFilters();
  const catalog_metadata = useCatalogMetadata();
  return (field_id: string) => {
    const filter_value = filters[field_id];
    const field_stats = catalog_metadata?.hierarchy?.find(
      (d) => d.data.name === field_id
    )?.data?.stats;
    if (!field_id) return null;
    if (!catalog_metadata) return null;
    if (typeof filter_value === `object` && `gte` in filter_value) {
      return Number(filter_value.gte);
    } else if (field_stats && "min" in field_stats) {
      return Number(field_stats.min);
    }
    throw new Error(`Could not get min for ${field_id}`);
  };
}

function useGetCurrentMax(): (field_id: string) => number | null {
  const filters = useFilters();
  const catalog_metadata = useCatalogMetadata();
  return (field_id: string) => {
    const filter_value = filters[field_id];
    const field_stats = catalog_metadata?.hierarchy?.find(
      (d) => d.data.name === field_id
    )?.data?.stats;
    if (!field_id) return null;
    if (!catalog_metadata) return null;
    if (typeof filter_value === `object` && `lte` in filter_value) {
      return Number(filter_value.lte);
    } else if (field_stats && "max" in field_stats) {
      return Number(field_stats.max);
    }
    throw new Error(`Could not get max for ${field_id}`);
  };
}

function usePlotQuery<RequestType, ResponseType>({
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
  return useQuery({
    queryKey: [`plot-data`, path, body],
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
}
