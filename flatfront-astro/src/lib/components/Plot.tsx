import type {
  schema,
  DataPostRequestBody,
  HistogramPostRequestBody,
  DataResponse,
  HistogramResponse,
  PlotID,
  PlotType
} from "../types";

import Highcharts from "highcharts";
import HighchartsExporting from "highcharts/modules/exporting";
import HighchartsExportData from "highcharts/modules/export-data";
import HighchartsHeatmap from "highcharts/modules/heatmap";
import HighchartsReact from "highcharts-react-official";
import { useQuery } from "@tanstack/react-query";
import {
  log,
  fetch_api_post,
  get_field_type,
  create_context_helper
} from "../shared";
import * as controller from "../app-state";
import { CellSection, Placeholder, Select, Checkbox } from "./Primitives";
import { useCatalogID } from "./CatalogContext";
import { useIsDarkMode } from "../dark-mode";
import { useCatalogMetadata } from "./CatalogMetadata";
import { useFilters } from "../filters";

HighchartsExporting(Highcharts);
HighchartsExportData(Highcharts);
HighchartsHeatmap(Highcharts);

const [usePlotID, PlotIDProvider] = create_context_helper<PlotID>(`PlotID`);

function usePlotType() {
  const plot_id = usePlotID();
  const plot_type = controller.useAppState()?.set_plot_type?.[plot_id];
  return plot_type;
}

export default function PlotSection({ id }: { id: PlotID }) {
  return (
    <PlotIDProvider value={id}>
      <PlotComponent />
      <PlotControls />
    </PlotIDProvider>
  );
}

function PlotComponent() {
  const plot_type = usePlotType();
  const plot_component = (() => {
    switch (plot_type) {
      case `scatterplot`:
        return <Scatterplot />;
      case `heatmap`:
        return <Heatmap />;
      default:
        return <Placeholder>Choose a plot type</Placeholder>;
    }
  })();
  return plot_component;
}

function PlotControls() {
  const plot_type = usePlotType();

  const scatterplot_controls = (
    <>
      <LabelledControl label="X-axis">
        <PlotControl
          plotControlkey="x_axis"
          placeholder="Choose X-Axis..."
          showLogSwitch
        />
      </LabelledControl>
      <LabelledControl label="Y-axis">
        <PlotControl
          plotControlkey="y_axis"
          placeholder="Choose Y-Axis..."
          showLogSwitch
        />
      </LabelledControl>
      <LabelledControl label="Z-axis">
        <PlotControl
          plotControlkey="z_axis"
          placeholder="Choose Z-Axis..."
          showLogSwitch
        />
      </LabelledControl>
    </>
  );

  const heatmap_controls = (
    <>
      <LabelledControl label="X-axis">
        <PlotControl plotControlkey="x_axis" placeholder="Choose X-Axis..." />
      </LabelledControl>
      <LabelledControl label="Y-axis">
        <PlotControl plotControlkey="y_axis" placeholder="Choose Y-Axis..." />
      </LabelledControl>
    </>
  );

  const controls = (() => {
    switch (plot_type) {
      case `scatterplot`:
        return scatterplot_controls;
      case `heatmap`:
        return heatmap_controls;
      default:
        return null;
    }
  })();
  return (
    <div className="grid grid-cols-1 items-center gap-x-8 gap-y-4 desktop:grid-cols-3">
      <LabelledControl label="Plot Type">
        <PlotTypeSelect />
      </LabelledControl>
      {controls}
    </div>
  );
}

function LabelledControl({
  label,
  children
}: {
  label: string;
  children: React.ReactNode;
}) {
  return (
    <div data-type="LabelledControl" className="space-y-2">
      <label className="block uppercase">{label}</label>
      {children}
    </div>
  );
}

function PlotTypeSelect() {
  const plot_id = usePlotID();
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
      size="small"
    />
  );
}

function PlotControl({
  plotControlkey: plot_control_key,
  placeholder,
  showLogSwitch = false
}: {
  plotControlkey: `x_axis` | `y_axis` | `z_axis`;
  placeholder?: string;
  showLogSwitch?: boolean;
}) {
  const plot_id = usePlotID();
  const catalog_metadata = useCatalogMetadata();
  const all_leaf_nodes = catalog_metadata?.hierarchy?.leaves() ?? [];
  const numeric_nodes = all_leaf_nodes.filter((d) => {
    const type = get_field_type(d.data);
    return type === `INTEGER` || type === `FLOAT`;
  });

  const plot_config = controller.useAppState().set_plot_control?.[plot_id];

  const field_id = plot_config?.[plot_control_key];
  // const is_log_mode = plot_config?.[`${plot_control_key}_log_mode`] ?? false;

  const dispatch = controller.useDispatch();

  const value = numeric_nodes.find((d) => d.data.name === field_id);

  let log_switch = null;

  if (showLogSwitch) {
    log_switch = (
      <div className="relative">
        <label className="absolute left-1/2 top-0 -translate-x-1/2 translate-y-[calc(-100%-5px)] uppercase leading-none">
          log
        </label>
        <Checkbox />
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
          dispatch([`set_plot_control`, plot_id, plot_control_key], value);
        }}
      />
      {log_switch}
    </div>
  );
}

function Scatterplot() {
  const catalog_id = useCatalogID();
  const filters = useFilters();

  const plot_id = usePlotID();
  const plot_state = controller.useAppState()?.set_plot_control?.[plot_id];

  const x_axis_field_id = plot_state?.x_axis;
  const y_axis_field_id = plot_state?.y_axis;
  const count = plot_state?.count ?? 3e3;

  const enable_request =
    Boolean(catalog_id) && Boolean(x_axis_field_id) && Boolean(y_axis_field_id);

  const request_body: DataPostRequestBody = {
    object: true,
    fields: [x_axis_field_id, y_axis_field_id],
    ...filters,
    count,
    sample: 0.42
    // ...query_parameters
  };

  const query_config = {
    path: `/${catalog_id}/data`,
    body: request_body
  };

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

  const dark_mode = useIsDarkMode();

  const className = dark_mode ? `highcharts-dark` : `highcharts-light`;

  return (
    <HighchartsReact
      highcharts={Highcharts}
      options={options}
      containerProps={{ className }}
    />
  );
}

function Heatmap() {
  const catalog_id = useCatalogID();
  const filters = useFilters();

  const plot_id = usePlotID();
  const plot_state = controller.useAppState()?.set_plot_control?.[plot_id];

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
    ...filters
    // ...query_parameters
  };

  const query_config = {
    path: `/${catalog_id}/histogram`,
    body: request_body
  };

  const query = useQuery({
    queryKey: [`plot-data`, query_config],
    queryFn: async (): Promise<HistogramResponse> => {
      return fetch_api_post<HistogramPostRequestBody, HistogramResponse>(
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

  const data = query.data;

  const data_munged = (() => {
    if (!x_axis_field_id) return [];
    if (!y_axis_field_id) return [];
    if (!data) return [];
    return data.buckets.map(({ key, count }) => {
      return [key[0], key[1], count];
    });
  })();

  const options: Highcharts.Options = {
    chart: {
      animation: false,
      styledMode: true
    },
    tooltip: {
      animation: false
    },
    exporting: {
      enabled: true
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
    colorAxis: {
      minColor: `#FFFFFF`,
      maxColor: `#000000`
    },
    series: [
      {
        type: `heatmap`,
        data: data_munged,
        colsize: data?.sizes[0],
        rowsize: data?.sizes[1]
      }
    ]
  };

  return <HighchartsChart options={options} />;
}

function HighchartsChart({ options }: { options: Highcharts.Options }) {
  const dark_mode = useIsDarkMode();
  const className = dark_mode ? `highcharts-dark` : `highcharts-light`;
  return (
    <HighchartsReact
      highcharts={Highcharts}
      options={options}
      containerProps={{ className }}
    />
  );
}
