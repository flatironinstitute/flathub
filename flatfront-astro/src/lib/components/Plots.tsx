import type {
  DataPostRequestBody,
  DataResponse,
  HistogramPostRequestBody,
  HistogramResponse,
  PlotWrapper
} from "../types";
import React from "react";
import {
  useQuery,
  useQueryClient,
  type UseQueryOptions
} from "@tanstack/react-query";
import * as d3 from "d3";
import * as Plot from "@observablehq/plot";
import lodash_merge from "lodash.merge";
import { log, fetch_api_post } from "../shared";
import { useIsDarkMode } from "../dark-mode";
import { useCatalogID } from "../contexts/CatalogContext";
import { useRandomConfig } from "../contexts/RandomContext";
import { useFilterValues } from "../contexts/FiltersContext";
import { useCatalogMetadata } from "../contexts/CatalogMetadataContext";
import { usePlotState } from "../contexts/PlotContext";
import { StatusBox } from "./Primitives";
import {
  LabelledPlotControl,
  Labelled,
  LogModeCheckbox
} from "./PlotPrimitives";
import HighchartsPlot from "./HighchartsPlot";
import ObservablePlot from "./ObservablePlot";

export const Histogram: PlotWrapper = {
  key: `histogram`,
  label: `Histogram`,
  order: 0,
  Plot() {
    const catalog_id = useCatalogID();
    const filters = useFilterValues();
    const random_config = useRandomConfig();

    const field_config = useAxisConfig(`field`);
    const count_config = useAxisConfig(`count`);

    const enable_request =
      Boolean(catalog_id) && field_config.ready_for_request;

    const query = usePlotQuery<HistogramPostRequestBody, HistogramResponse>({
      path: `/${catalog_id}/histogram`,
      body: {
        fields: [
          {
            field: field_config.field_id,
            size: 100,
            log: field_config.log_mode
          }
        ] as any,
        ...filters,
        ...random_config
      },
      label: `Histogram`,
      enabled: enable_request
    });

    const sizes = query.data?.sizes ?? [0, 0];

    const data_munged = (() => {
      if (!query.data) return [];
      return query.data.buckets.map(({ key: [x], count }) => {
        const x1 = +x;
        const x2 = field_config.log_mode ? x1 * sizes[0] : x1 + sizes[0];
        return { x1, x2, count };
      });
    })();

    const status = (() => {
      if (field_config.log_mode_error_message) {
        return field_config.log_mode_error_message;
      } else if (query.isFetching) {
        return <LoadingBox />;
      } else if (!(data_munged.length > 0)) {
        return `No data.`;
      } else {
        return null;
      }
    })();

    const aspect = 640 / 400;

    const width = 900;

    const plot_options: Plot.PlotOptions = {
      width,
      height: width / aspect,
      style: {
        background: `transparent`,
        width: `100%`
      },
      x: {
        label: field_config.field_id,
        type: field_config.log_mode ? `log` : `linear`,
        tickFormat: field_config.log_mode ? `.3~f` : undefined,
        grid: true
      },
      y: {
        label: `Count`,
        type: count_config.log_mode ? `log` : `linear`,
        tickFormat: count_config.log_mode ? `.3~f` : undefined,
        grid: true
      },
      marks: [
        Plot.rectY(data_munged, {
          x1: `x1`,
          x2: `x2`,
          y: `count`,
          insetRight: 1,
          insetLeft: 1,
          tip: true
        })
      ]
    };

    const plot = Plot.plot(plot_options);

    return (
      <StatusWrapper status={status}>
        <ObservablePlot plot={plot} />
      </StatusWrapper>
    );
  },
  Controls() {
    return (
      <>
        <LabelledPlotControl
          label="Field"
          plotControlKey="field"
          placeholder="Choose field..."
          showLogSwitch={true}
        />
        <LogCountControl />
      </>
    );
  }
};

export const Heatmap: PlotWrapper = {
  key: `heatmap`,
  label: `Heatmap`,
  order: 2,
  Plot() {
    const catalog_id = useCatalogID();
    const filters = useFilterValues();
    const random_config = useRandomConfig();

    const x_axis = useAxisConfig(`x_axis`);
    const y_axis = useAxisConfig(`y_axis`);

    const enable_request =
      Boolean(catalog_id) &&
      x_axis.ready_for_request &&
      y_axis.ready_for_request;

    const query = usePlotQuery<HistogramPostRequestBody, HistogramResponse>({
      path: `/${catalog_id}/histogram`,
      body: {
        fields: [
          { field: x_axis.field_id, size: 40, log: x_axis.log_mode },
          { field: y_axis.field_id, size: 40, log: y_axis.log_mode }
        ] as any,
        ...filters,
        ...random_config
      },
      label: `Heatmap`,
      enabled: enable_request
    });

    const sizes = query.data?.sizes ?? [0, 0];

    const data_munged = (() => {
      if (!query.data) return [];
      return query.data.buckets.map(({ key: [x, y], count }) => {
        const x1 = +x;
        const y1 = +y;
        const x2 = x_axis.log_mode ? x1 * sizes[0] : x1 + sizes[0];
        const y2 = y_axis.log_mode ? y1 * sizes[1] : y1 + sizes[1];
        return { x1, y1, x2, y2, count };
      });
    })();

    const status = (() => {
      if (x_axis.log_mode_error_message) {
        return x_axis.log_mode_error_message;
      } else if (y_axis.log_mode_error_message) {
        return y_axis.log_mode_error_message;
      } else if (query.isFetching) {
        return <LoadingBox />;
      } else if (!(data_munged.length > 0)) {
        return `No data.`;
      } else {
        return null;
      }
    })();

    const is_dark_mode = useIsDarkMode();

    const aspect = 640 / 400;

    const width = 900;

    const plot_options: Plot.PlotOptions = {
      width,
      height: width / aspect,
      style: {
        background: `transparent`,
        width: `100%`
      },
      color: {
        type: `sequential`,
        label: `Count`,
        scheme: `Greys`,
        reverse: is_dark_mode,
        domain: d3.extent(data_munged, (d) => d.count)
      },
      x: {
        label: x_axis.field_id,
        type: x_axis.log_mode ? `log` : `linear`,
        tickFormat: x_axis.log_mode ? `.3~f` : undefined,
        grid: true
      },
      y: {
        label: y_axis.field_id,
        type: y_axis.log_mode ? `log` : `linear`,
        tickFormat: y_axis.log_mode ? `.3~f` : undefined,
        grid: true
      },
      marks: [
        Plot.rect(data_munged, {
          x1: `x1`,
          x2: `x2`,
          y1: `y1`,
          y2: `y2`,
          fill: `count`,
          stroke: `currentColor`,
          strokeOpacity: 0.2,
          tip: true
        })
      ]
    };

    const plot = Plot.plot(plot_options);

    const legend = Plot.legend({
      style: {
        background: `transparent`
      },
      color: plot_options.color
    });

    return (
      <StatusWrapper status={status}>
        <ObservablePlot plot={plot} />
        <ObservablePlot className="flex justify-center" plot={legend} />
      </StatusWrapper>
    );
  },
  Controls() {
    return (
      <>
        <XAxisControl />
        <YAxisControl />
      </>
    );
  }
};

export const BoxPlot: PlotWrapper = {
  key: `boxplot`,
  label: `Box Plot`,
  order: 3.1,
  Plot() {
    const catalog_id = useCatalogID();
    const filters = useFilterValues();
    const random_config = useRandomConfig();

    const x_axis = useAxisConfig(`x_axis`);
    const y_axis = useAxisConfig(`y_axis`);

    const enable_request =
      Boolean(catalog_id) &&
      x_axis.ready_for_request &&
      y_axis.ready_for_request;

    const query = usePlotQuery<HistogramPostRequestBody, HistogramResponse>({
      path: `/${catalog_id}/histogram`,
      body: {
        fields: [
          { field: x_axis.field_id, size: 60, log: x_axis.log_mode }
        ] as any,
        quartiles: y_axis.field_id?.toString(),
        ...filters,
        ...random_config
      },
      label: `Boxplot`,
      enabled: enable_request
    });

    const data_munged = (() => {
      if (!query.data) return [];
      return query.data.buckets.map(({ key: [x, y], count, quartiles }) => {
        const [low, q1, median, q3, high] = quartiles;
        return {
          x,
          y,
          count,
          low,
          q1,
          median,
          q3,
          high
        };
      });
    })();

    const rect_width = (() => {
      const [first, second] = data_munged;
      if (!first) return 0;
      if (!second) return 0;
      const distance = Math.abs(+first?.x - +second?.x);
      return distance * 0.25;
    })();

    const status = (() => {
      if (x_axis.log_mode_error_message) {
        return x_axis.log_mode_error_message;
      } else if (y_axis.log_mode_error_message) {
        return y_axis.log_mode_error_message;
      } else if (query.isFetching) {
        return <LoadingBox />;
      } else if (!(data_munged.length > 0)) {
        return `No data.`;
      } else {
        return null;
      }
    })();

    const aspect = 640 / 400;

    const width = 700;

    const is_dark_mode = useIsDarkMode();

    const plot_options: Plot.PlotOptions = {
      width,
      height: width / aspect,
      insetLeft: 10,
      insetRight: 10,
      insetBottom: 20,
      style: {
        background: `transparent`,
        width: `100%`
      },
      x: {
        label: x_axis.field_id,
        type: x_axis.log_mode ? `log` : `linear`,
        tickFormat: x_axis.log_mode ? `.3~f` : undefined,
        grid: true
      },
      y: {
        label: y_axis.field_id,
        type: y_axis.log_mode ? `log` : `linear`,
        tickFormat: y_axis.log_mode ? `.3~f` : undefined,
        grid: true,
        ticks: 5
      },
      marks: [
        Plot.ruleX(data_munged, {
          x: `x`,
          y1: `low`,
          y2: `high`,
          marker: `dot`,
          opacity: 0.5
        }),
        Plot.rect(data_munged, {
          x1: (d) => d.x - rect_width,
          x2: (d) => d.x + rect_width,
          y1: `q1`,
          y2: `q3`,
          fill: is_dark_mode ? `black` : `white`,
          stroke: `currentColor`,
          strokeOpacity: 0.5
        }),
        Plot.dot(data_munged, {
          x: `x`,
          y: `median`,
          tip: true
        }),
        Plot.line(data_munged, {
          x: `x`,
          y: `median`
        })
        // Plot.rect(data_munged, {
        //   x1: `x`,
        //   x2: (d) => d.x + 1,
        //   y1: `low`,
        //   y2: `high`,
        //   fill: `currentColor`,
        //   stroke: `red`,
        //   strokeOpacity: 0.2,
        //   tip: true
        // })
      ]
    };

    const plot = Plot.plot(plot_options);

    return (
      <StatusWrapper status={status}>
        <ObservablePlot plot={plot} />
      </StatusWrapper>
    );
  },
  Controls() {
    return (
      <>
        <XAxisControl />
        <YAxisControl />
      </>
    );
  }
};

export const Scatterplot: PlotWrapper = {
  key: `scatterplot`,
  label: `Scatterplot`,
  order: 5,
  Plot() {
    const catalog_id = useCatalogID();
    const filters = useFilterValues();
    const plot_state = usePlotState();

    const x_axis = useAxisConfig(`x_axis`);
    const y_axis = useAxisConfig(`y_axis`);

    const count = plot_state?.count ?? 2e3;

    // TODO: What's the best way of doing this
    // const total_rows = useCatalogMetadata()?.response?.count;
    // const sample = Math.min((count * 10) / total_rows, 1);
    const sample = 0.9999;

    const enable_request =
      Boolean(catalog_id) &&
      x_axis.ready_for_request &&
      y_axis.ready_for_request;

    const query = usePlotQuery<DataPostRequestBody, DataResponse>({
      path: `/${catalog_id}/data`,
      body: {
        object: true,
        fields: [x_axis.field_id, y_axis.field_id],
        ...filters,
        count,
        sample
      },
      label: `Scatterplot`,
      enabled: enable_request
    });

    const data_munged = (() => {
      if (!x_axis.field_id) return [];
      if (!y_axis.field_id) return [];
      if (!query.data) return [];
      return query.data.map((datum) => {
        return { x: +datum[x_axis.field_id], y: +datum[y_axis.field_id] };
      });
    })();

    const status = (() => {
      if (x_axis.log_mode_error_message) {
        return x_axis.log_mode_error_message;
      } else if (y_axis.log_mode_error_message) {
        return y_axis.log_mode_error_message;
      } else if (query.isFetching) {
        return <LoadingBox />;
      } else if (!(data_munged.length > 0)) {
        return `No data.`;
      } else {
        return null;
      }
    })();

    const aspect = 640 / 400;

    const width = 700;

    const plot_options: Plot.PlotOptions = {
      width,
      height: width / aspect,
      style: {
        background: `transparent`,
        width: `100%`
      },
      x: {
        label: x_axis.field_id,
        type: x_axis.log_mode ? `log` : `linear`,
        tickFormat: x_axis.log_mode ? `.3~f` : undefined,
        grid: true
      },
      y: {
        label: y_axis.field_id,
        type: y_axis.log_mode ? `log` : `linear`,
        tickFormat: y_axis.log_mode ? `.3~f` : undefined,
        grid: true,
        ticks: 5
      },
      marks: [
        Plot.dot(data_munged, {
          x: `x`,
          y: `y`,
          r: 1,
          fill: `currentColor`,
          stroke: `currentColor`,
          strokeOpacity: 0.2,
          tip: true
        })
      ]
    };

    const plot = Plot.plot(plot_options);

    return (
      <StatusWrapper status={status}>
        <ObservablePlot plot={plot} />
      </StatusWrapper>
    );
  },
  Controls() {
    return (
      <>
        <XAxisControl />
        <YAxisControl />
      </>
    );
  }
};

export const Scatterplot3D: PlotWrapper = {
  key: `scatterplot_3d`,
  label: `3D Scatterplot`,
  order: 6,
  Plot() {
    const catalog_id = useCatalogID();
    const filters = useFilterValues();
    const random_config = useRandomConfig();
    const plot_state = usePlotState();

    const x_axis = useAxisConfig(`x_axis`);
    const y_axis = useAxisConfig(`y_axis`);
    const z_axis = useAxisConfig(`z_axis`);

    const count = plot_state?.count ?? 2e3;

    // TODO: What's the best way of doing this
    // const total_rows = useCatalogMetadata()?.response?.count;
    // const sample = Math.min((count * 10) / total_rows, 1);
    const sample = 0.9999;

    const enable_request =
      Boolean(catalog_id) &&
      x_axis.ready_for_request &&
      y_axis.ready_for_request &&
      z_axis.ready_for_request;

    const query = usePlotQuery<DataPostRequestBody, DataResponse>({
      path: `/${catalog_id}/data`,
      body: {
        object: true,
        fields: [x_axis.field_id, y_axis.field_id, z_axis.field_id],
        ...filters,
        count,
        sample
      },
      label: `3D Scatterplot`,
      enabled: enable_request
    });

    const data_munged = (() => {
      if (!x_axis.field_id) return [];
      if (!y_axis.field_id) return [];
      if (!z_axis.field_id) return [];
      if (!query.data) return [];
      return query.data.map((datum) => {
        return [
          +datum[x_axis.field_id],
          +datum[y_axis.field_id],
          +datum[z_axis.field_id]
        ];
      });
    })();

    const status = (() => {
      if (x_axis.log_mode_error_message) {
        return x_axis.log_mode_error_message;
      } else if (y_axis.log_mode_error_message) {
        return y_axis.log_mode_error_message;
      } else if (z_axis.log_mode_error_message) {
        return z_axis.log_mode_error_message;
      } else if (query.isFetching) {
        return <LoadingBox />;
      } else if (!(data_munged.length > 0)) {
        return `No data.`;
      } else {
        return null;
      }
    })();

    return (
      <StatusWrapper status={status}>
        <HighchartsPlot
          options={get_highcharts_options({
            chart: {
              options3d: {
                enabled: true,
                alpha: 10,
                beta: 20,
                depth: 400,
                drag: {
                  enabled: true
                }
              }
            },
            xAxis: {
              type: x_axis.log_mode ? `logarithmic` : `linear`,
              title: {
                text: x_axis.field_id
              },
              gridLineWidth: 1
            },
            yAxis: {
              type: y_axis.log_mode ? `logarithmic` : `linear`,
              title: {
                text: y_axis.field_id
              }
            },
            zAxis: {
              type: z_axis.log_mode ? `logarithmic` : `linear`,
              title: {
                text: z_axis.field_id
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
          })}
        />
      </StatusWrapper>
    );
  },
  Controls() {
    return (
      <>
        <XAxisControl />
        <YAxisControl />
        <LabelledPlotControl
          label="Z-Axis"
          plotControlKey="z_axis"
          placeholder="Choose Z-Axis..."
          showLogSwitch={true}
        />
      </>
    );
  }
};

function XAxisControl() {
  return (
    <LabelledPlotControl
      label="X-Axis"
      plotControlKey="x_axis"
      placeholder="Choose X-Axis..."
      showLogSwitch={true}
    />
  );
}

function YAxisControl() {
  return (
    <LabelledPlotControl
      label="Y-Axis"
      plotControlKey="y_axis"
      placeholder="Choose Y-Axis..."
      showLogSwitch={true}
    />
  );
}

function LogCountControl() {
  return (
    <Labelled label="Count: Log Scale">
      <LogModeCheckbox plotControlkey="count" />
    </Labelled>
  );
}

function LoadingBox({
  queryKey: query_key = [`plot-data`]
}: {
  queryKey?: UseQueryOptions[`queryKey`];
}) {
  const query_client = useQueryClient();
  return (
    <div className="space-y-2">
      <div>Loading...</div>
      <button
        onClick={() => {
          query_client.cancelQueries({ queryKey: query_key });
        }}
        className="cursor-pointer underline"
      >
        Cancel
      </button>
    </div>
  );
}

function StatusWrapper({
  status,
  children
}: {
  status: React.ReactNode;
  children: React.ReactNode;
}) {
  return (
    <div className="relative">
      {status && <StatusBox>{status}</StatusBox>}
      {children}
    </div>
  );
}

function get_highcharts_options(
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

function useAxisConfig(
  key: `x_axis` | `y_axis` | `z_axis` | `count` | `field`
) {
  const plot_state = usePlotState();
  const is_log_allowed = useGetIsLogAllowed();
  const field_id = plot_state?.[key];
  const log_mode_requested = plot_state?.[`${key}_log_mode`] ?? false;
  const log_mode_allowed = is_log_allowed(field_id);
  const log_mode_error = log_mode_requested && !log_mode_allowed;
  const log_mode = log_mode_requested && log_mode_allowed;
  const ready_for_request = Boolean(field_id) && !log_mode_error;
  const log_mode_error_message = log_mode_error
    ? `Log mode not allowed because "${field_id}" values cross zero.`
    : null;
  return {
    field_id,
    log_mode,
    log_mode_error_message,
    ready_for_request
  };
}

function useGetIsLogAllowed(): (field_id: string) => boolean {
  const get_current_min = useGetCurrentMin();
  const get_current_max = useGetCurrentMax();
  return (field_id: string): boolean => {
    if (!field_id) return true;
    if (field_id === `count`) return true;
    const current_min = get_current_min(field_id);
    const current_max = get_current_max(field_id);
    if (current_min > 0 && current_max > 0) return true;
    return false;
  };
}

function useGetCurrentMin(): (field_id: string) => number | null {
  const filters = useFilterValues();
  const catalog_metadata = useCatalogMetadata();
  return (field_id: string): number | null => {
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
  const filters = useFilterValues();
  const catalog_metadata = useCatalogMetadata();
  return (field_id: string): number | null => {
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
