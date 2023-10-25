import type {
  DataPostRequestBody,
  HistogramPostRequestBody,
  DataResponse,
  HistogramResponse,
  PlotID,
  PlotType
} from "../types";
import { useQuery } from "@tanstack/react-query";
import {
  log,
  fetch_api_post,
  get_field_type,
  create_context_helper
} from "../shared";
import * as controller from "../contexts/AppStateContext";
import { useIsDarkMode } from "../dark-mode";
import { useRemovePlot } from "../plot-hooks";
import { useFilters } from "../contexts/FiltersContext";
import { useCatalogID } from "../contexts/CatalogContext";
import { useCatalogMetadata } from "../contexts/CatalogMetadataContext";
import {
  Placeholder,
  Select,
  Checkbox,
  SimpleLabel,
  StatusBox
} from "./Primitives";
import HighchartsPlot from "./HighchartsPlot";

const [usePlotID, PlotIDProvider] = create_context_helper<PlotID>(`PlotID`);

function usePlotType() {
  const plot_id = usePlotID();
  const plot_type = controller.useAppState()?.set_plot_type?.[plot_id];
  return plot_type;
}

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
        <PlotControls />
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

  const [x_axis_control, y_axis_control, z_axis_control, sort_control] = [
    [`X-Axis`, `x_axis`, `Choose X-Axis...`, true],
    [`Y-Axis`, `y_axis`, `Choose Y-Axis...`, true],
    [`Z-Axis`, `z_axis`, `Choose Z-Axis...`, true],
    [`Sort`, `sort`, `Choose sort variable...`, false]
  ].map(
    ([label, plot_control_key, placeholder, showLogSwitch]: [
      string,
      string,
      string,
      boolean
    ]) => {
      return (
        <Labelled label={label} key={plot_control_key}>
          <PlotControl
            plotControlkey={plot_control_key}
            placeholder={placeholder}
            showLogSwitch={showLogSwitch}
          />
        </Labelled>
      );
    }
  );

  const histogram_controls = <>{x_axis_control}</>;

  const scatterplot_controls = (
    <>
      {x_axis_control}
      {y_axis_control}
      {z_axis_control}
      {sort_control}
    </>
  );

  const heatmap_controls = (
    <>
      {x_axis_control}
      {y_axis_control}
    </>
  );

  const controls = (() => {
    switch (plot_type) {
      case `histogram`:
        return histogram_controls;
      case `scatterplot`:
        return scatterplot_controls;
      case `heatmap`:
        return heatmap_controls;
      default:
        return null;
    }
  })();
  return (
    <div className="grid grid-cols-1 items-center gap-x-8 gap-y-4 @xl/plot:grid-cols-2 @3xl/plot:grid-cols-3">
      <Labelled label="Plot Type">
        <PlotTypeSelect />
      </Labelled>
      {controls}
    </div>
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

  const plot_config = controller.useAppState().set_plot_control?.[plot_id];

  const field_id = plot_config?.[plot_control_key];

  const dispatch = controller.useDispatch();

  const value = numeric_nodes.find((d) => d.data.name === field_id);

  let log_switch = null;

  if (showLogSwitch) {
    const log_mode_key = `${plot_control_key}_log_mode`;
    const is_log_mode = plot_config?.[log_mode_key] ?? false;
    log_switch = (
      <div className="relative">
        <label className="absolute left-1/2 top-0 -translate-x-1/2 translate-y-[calc(-100%-5px)] uppercase leading-none">
          log
        </label>
        <Checkbox
          checked={is_log_mode}
          onCheckedChange={(checked) => {
            dispatch([`set_plot_control`, plot_id, log_mode_key], checked);
          }}
        />
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
        triggerClassName="overflow-hidden text-ellipsis"
        valueClassName="overflow-hidden text-ellipsis"
      />
      {log_switch}
    </div>
  );
}

function Histogram() {
  const catalog_id = useCatalogID();
  const filters = useFilters();

  const plot_id = usePlotID();
  const plot_state = controller.useAppState()?.set_plot_control?.[plot_id];

  const x_axis_field_id = plot_state?.x_axis;
  const x_axis_log_mode = plot_state?.x_axis_log_mode ?? false;

  const enable_request = Boolean(catalog_id) && Boolean(x_axis_field_id);

  const fields: any = [
    { field: x_axis_field_id, size: 100, log: x_axis_log_mode }
  ];

  const request_body: HistogramPostRequestBody = {
    fields,
    ...filters
  };

  const query = usePlotQuery<HistogramPostRequestBody, HistogramResponse>({
    path: `/${catalog_id}/histogram`,
    body: request_body,
    label: `Histogram`,
    enabled: enable_request
  });

  const data = query.data;

  const data_munged = (() => {
    if (!x_axis_field_id) return [];
    if (!data) return [];
    return data.buckets.map(({ key, count }) => {
      return [...key, count];
    });
  })();

  log(`data_munged`, data_munged);

  const options: Highcharts.Options = {
    ...get_highcharts_options(),
    xAxis: {
      type: x_axis_log_mode ? `logarithmic` : `linear`,
      title: {
        text: x_axis_field_id
      }
    },
    series: [
      {
        type: `column`,
        name: `Count`,
        data: data_munged,
        animation: false,
        // pointWidth: 5,
        // pointPadding: -0.1,
        borderRadius: 0
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

  const plot_id = usePlotID();
  const plot_state = controller.useAppState()?.set_plot_control?.[plot_id];

  const x_axis_field_id = plot_state?.x_axis;
  const y_axis_field_id = plot_state?.y_axis;
  // const count = plot_state?.count ?? 5e3;
  const count = plot_state?.count ?? 2e3;
  const sort = plot_state?.sort ?? null;

  const enable_request =
    Boolean(catalog_id) && Boolean(x_axis_field_id) && Boolean(y_axis_field_id);

  const request_body: DataPostRequestBody = {
    object: true,
    fields: [x_axis_field_id, y_axis_field_id],
    ...filters,
    count,
    sort: sort ? [sort] : undefined
    // sample: 0.42,
    // seed: 12345
    // ...query_parameters
  };

  const query = usePlotQuery<DataPostRequestBody, DataResponse>({
    path: `/${catalog_id}/data`,
    body: request_body,
    label: `Scatterplot`,
    enabled: enable_request
  });

  const data = query.data ?? [];

  const data_munged = (() => {
    if (!x_axis_field_id) return [];
    if (!y_axis_field_id) return [];
    return data.map((datum) => {
      return [+datum[x_axis_field_id], +datum[y_axis_field_id]];
    });
  })();

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

function get_highcharts_options(): Highcharts.Options {
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
