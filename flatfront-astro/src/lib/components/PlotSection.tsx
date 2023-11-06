import type { PlotID, PlotType } from "../types";
import { useMergeState } from "../contexts/AppStateContext";
import {
  PlotIDProvider,
  useRemovePlot,
  usePlotID,
  usePlotType
} from "../contexts/PlotContext";
import { Select, Placeholder, SimpleLabel } from "./Primitives";
import { Labelled } from "./PlotPrimitives";
import * as Plots from "./Plots";

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

function PlotTypeSelect() {
  const plot_id = usePlotID();
  const plot_type = usePlotType();
  const plot_type_options: { key: PlotType; label: string }[] = [
    { key: Plots.Histogram.key, label: Plots.Histogram.label },
    { key: Plots.Heatmap.key, label: Plots.Heatmap.label },
    { key: Plots.HeatmapObservable.key, label: Plots.HeatmapObservable.label },
    { key: Plots.BoxPlot.key, label: Plots.BoxPlot.label },
    { key: Plots.Scatterplot.key, label: Plots.Scatterplot.label },
    { key: Plots.Scatterplot3D.key, label: Plots.Scatterplot3D.label }
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

function PlotComponent() {
  const plot_type = usePlotType();
  switch (plot_type) {
    case Plots.Histogram.key:
      return <Plots.Histogram.Plot />;
    case Plots.Heatmap.key:
      return <Plots.Heatmap.Plot />;
    case Plots.HeatmapObservable.key:
      return <Plots.HeatmapObservable.Plot />;
    case Plots.BoxPlot.key:
      return <Plots.BoxPlot.Plot />;
    case Plots.Scatterplot.key:
      return <Plots.Scatterplot.Plot />;
    case Plots.Scatterplot3D.key:
      return <Plots.Scatterplot3D.Plot />;
    default:
      return <Placeholder>Choose a plot type</Placeholder>;
  }
}

function PlotControls() {
  const plot_type = usePlotType();
  switch (plot_type) {
    case Plots.Histogram.key:
      return <Plots.Histogram.Controls />;
    case Plots.Heatmap.key:
      return <Plots.Heatmap.Controls />;
    case Plots.HeatmapObservable.key:
      return <Plots.HeatmapObservable.Controls />;
    case Plots.BoxPlot.key:
      return <Plots.BoxPlot.Controls />;
    case Plots.Scatterplot.key:
      return <Plots.Scatterplot.Controls />;
    case Plots.Scatterplot3D.key:
      return <Plots.Scatterplot3D.Controls />;
    default:
      return null;
  }
}
