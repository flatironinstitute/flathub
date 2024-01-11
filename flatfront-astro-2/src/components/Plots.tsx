import * as d3 from "d3";
import * as Plot from "@observablehq/plot";
import type {
  DataPostRequestBody,
  DataResponse,
  HistogramPostRequestBody,
  HistogramResponse,
  PlotWrapper
} from "@/types";
import { useCatalogID } from "@/components/contexts/CatalogIDContext";
import { useRandomConfig } from "@/components/contexts/RandomContext";
import { useFilterValuesWithFieldNames } from "@/components/contexts/FiltersContext";
import { usePlotState } from "@/components/contexts/PlotContext";

import { HighchartsPlot } from "./HighchartsPlot";
import { ObservablePlot } from "./ObservablePlot";
import {
  get_highcharts_options,
  get_observable_options,
  LabelledPlotControl,
  LogCountControl,
  PlotStatusWrapper,
  useAxisConfig,
  usePlotQuery,
  XAxisControl,
  YAxisControl
} from "./PlotHelpers";
import { StatusBoxFromQuery } from "./StatusBox";

export const Histogram: PlotWrapper = {
  key: `histogram`,
  label: `Histogram`,
  order: 0,
  Plot() {
    const catalog_id = useCatalogID();
    const filters = useFilterValuesWithFieldNames();
    const random_config = useRandomConfig();

    const x_axis = useAxisConfig(`x_axis`);
    const count_axis = useAxisConfig(`count`);

    const [query, query_key] = usePlotQuery<
      HistogramPostRequestBody,
      HistogramResponse
    >({
      path: `/${catalog_id}/histogram`,
      body: {
        fields: [
          {
            field: x_axis.field_id,
            size: 100,
            log: x_axis.log_mode
          }
        ] as any,
        ...filters,
        ...random_config
      },
      label: `Histogram`,
      enabled: x_axis.ready_for_request
    });

    const sizes = query.data?.sizes ?? [0, 0];

    const data_munged = (() => {
      if (!query.data) return [];
      return query.data.buckets.map(({ key: [x], count }) => {
        const x1 = +x;
        const x2 = x_axis.log_mode ? x1 * sizes[0] : x1 + sizes[0];
        const mid = (x1 + x2) / 2;
        return { x1, x2, x: mid, count };
      });
    })();

    const plot_options: Plot.PlotOptions = get_observable_options({
      x: {
        label: x_axis.field_id,
        type: x_axis.log_mode ? `log` : `linear`,
        tickFormat: x_axis.log_mode ? `.2~e` : undefined
      },
      y: {
        label: `Count`,
        type: count_axis.log_mode ? `log` : `linear`
      },
      marks: [
        // Plot.rectY(data_munged, {
        //   x1: `x1`,
        //   x2: `x2`,
        //   y1: 1,
        //   y2: `count`,
        //   insetRight: 1,
        //   insetLeft: 1,
        //   tip: true
        // }),
        Plot.dot(data_munged, {
          x: `x`,
          y: `count`,
          tip: true
        }),
        Plot.line(data_munged, {
          x: `x`,
          y: `count`
        })
      ]
    });

    return (
      <PlotStatusWrapper
        status={
          <StatusBoxFromQuery
            message={!x_axis.field_id && `Choose a field`}
            axes={[x_axis]}
            query={query}
            queryKey={query_key}
            noData={data_munged.length === 0}
          />
        }
      >
        <ObservablePlot plot={Plot.plot(plot_options)} />
      </PlotStatusWrapper>
    );
  },
  Controls() {
    return (
      <>
        <LabelledPlotControl
          label="X-Axis"
          plotControlKey="x_axis"
          placeholder="Choose field..."
          showLogSwitch
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
    const filters = useFilterValuesWithFieldNames();
    const random_config = useRandomConfig();

    const x_axis = useAxisConfig(`x_axis`);
    const y_axis = useAxisConfig(`y_axis`);

    const enable_request =
      Boolean(catalog_id) &&
      x_axis.ready_for_request &&
      y_axis.ready_for_request;

    const [query, query_key] = usePlotQuery<
      HistogramPostRequestBody,
      HistogramResponse
    >({
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

    const plot_options: Plot.PlotOptions = get_observable_options({
      color: {
        type: `sequential`,
        label: `Count`,
        scheme: `Greys`,
        // reverse: is_dark_mode,
        domain: d3.extent(data_munged, (d) => d.count)
      },
      x: {
        label: x_axis.field_id,
        type: x_axis.log_mode ? `log` : `linear`
      },
      y: {
        label: y_axis.field_id,
        type: y_axis.log_mode ? `log` : `linear`
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
    });

    return (
      <PlotStatusWrapper
        status={
          <StatusBoxFromQuery
            axes={[x_axis, y_axis]}
            query={query}
            queryKey={query_key}
            noData={data_munged.length === 0}
          />
        }
      >
        <ObservablePlot plot={Plot.plot(plot_options)} />
        <ObservablePlot
          className="flex justify-center"
          plot={Plot.legend({
            style: {
              background: `transparent`
            },
            color: plot_options.color
          })}
        />
      </PlotStatusWrapper>
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
    const filters = useFilterValuesWithFieldNames();
    const random_config = useRandomConfig();

    const x_axis = useAxisConfig(`x_axis`);
    const y_axis = useAxisConfig(`y_axis`);

    const enable_request =
      Boolean(catalog_id) &&
      x_axis.ready_for_request &&
      y_axis.ready_for_request;

    const [query, query_key] = usePlotQuery<
      HistogramPostRequestBody,
      HistogramResponse
    >({
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

    const plot_options: Plot.PlotOptions = get_observable_options({
      insetLeft: 10,
      insetRight: 10,
      insetBottom: 20,
      x: {
        label: x_axis.field_id,
        type: x_axis.log_mode ? `log` : `linear`
      },
      y: {
        label: y_axis.field_id,
        type: y_axis.log_mode ? `log` : `linear`
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
          // fill: is_dark_mode ? `black` : `white`,
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
    });

    return (
      <PlotStatusWrapper
        status={
          <StatusBoxFromQuery
            axes={[x_axis, y_axis]}
            query={query}
            queryKey={query_key}
            noData={data_munged.length === 0}
          />
        }
      >
        <ObservablePlot plot={Plot.plot(plot_options)} />
      </PlotStatusWrapper>
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
    const filters = useFilterValuesWithFieldNames();
    const plot_state = usePlotState();

    const x_axis = useAxisConfig(`x_axis`);
    const y_axis = useAxisConfig(`y_axis`);

    const count = plot_state?.count ?? 2e3;

    // TODO: What's the best way of doing this
    // const total_rows = useCatalogMetadata()?.response?.count;
    // const sample = Math.min((count * 10) / total_rows, 1);
    const sample = 0.9999;

    const enable_request = x_axis.ready_for_request && y_axis.ready_for_request;

    const [query, query_key] = usePlotQuery<DataPostRequestBody, DataResponse>({
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

    const plot_options: Plot.PlotOptions = get_observable_options({
      x: {
        label: x_axis.field_id,
        type: x_axis.log_mode ? `log` : `linear`
      },
      y: {
        label: y_axis.field_id,
        type: y_axis.log_mode ? `log` : `linear`
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
    });

    return (
      <PlotStatusWrapper
        status={
          <StatusBoxFromQuery
            axes={[x_axis, y_axis]}
            query={query}
            queryKey={query_key}
            noData={data_munged.length === 0}
          />
        }
      >
        <ObservablePlot plot={Plot.plot(plot_options)} />
      </PlotStatusWrapper>
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
    const filters = useFilterValuesWithFieldNames();
    // const random_config = useRandomConfig();
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
      x_axis.ready_for_request &&
      y_axis.ready_for_request &&
      z_axis.ready_for_request;

    const [query, query_key] = usePlotQuery<DataPostRequestBody, DataResponse>({
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

    const options = get_highcharts_options({
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
    });

    return (
      <PlotStatusWrapper
        status={
          <StatusBoxFromQuery
            axes={[x_axis, y_axis, z_axis]}
            query={query}
            queryKey={query_key}
            noData={data_munged.length === 0}
          />
        }
      >
        <HighchartsPlot options={options} />
      </PlotStatusWrapper>
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
