import React from "react";
import { Trash2 } from "lucide-react";
import * as d3 from "d3";
import {
  PlotIDProvider,
  useAddPlot,
  usePlotType,
  usePlotsArray
} from "@/components/contexts/PlotContext";
import { Button } from "@/components/ui/button";
import { Separator } from "@/components/ui/separator";
import { H4 } from "@/components/ui/h4";
import {
  Popover,
  PopoverClose,
  PopoverContent,
  PopoverTrigger
} from "@/components/ui/popover";
import * as Plots from "@/components/Plots";

const plot_wrappers = d3.sort(Object.values(Plots), (d) => d.order);

const plot_key_to_label = new Map(
  plot_wrappers.map(({ key, label }) => [key, label])
);

export function PlotSection() {
  return (
    <>
      <AddPlot />
      <PlotsList />
    </>
  );
}

function AddPlot() {
  const add_plot = useAddPlot();
  return (
    <Popover>
      <PopoverTrigger asChild>
        <Button>Add Plot</Button>
      </PopoverTrigger>
      <PopoverContent className="w-80" align="start" avoidCollisions={false}>
        <div className="grid gap-4">
          {plot_wrappers.map(({ key, label }) => {
            return (
              <PopoverClose key={key} asChild>
                <Button onClick={() => add_plot(key)}>{label}</Button>
              </PopoverClose>
            );
          })}
        </div>
      </PopoverContent>
    </Popover>
  );
}

function PlotsList() {
  const plots_array = usePlotsArray();
  const plot_components = plots_array.map(({ plot_id }, index) => {
    return (
      <React.Fragment key={plot_id}>
        {index === 0 ? null : <Separator />}
        <PlotIDProvider value={plot_id}>
          <PlotWrapper index={index} />
        </PlotIDProvider>
      </React.Fragment>
    );
  });
  return <>{plot_components}</>;
}

function PlotWrapper({ index }: { index: number }) {
  const plot_key = usePlotType();
  const label = plot_key_to_label.get(plot_key);
  const wrapper = plot_wrappers.find(({ key }) => key === plot_key);
  const { Plot, Controls } = wrapper;
  return (
    <>
      <div className="flex items-center justify-between">
        <H4>
          Plot {index + 1}: {label}
        </H4>
        <Button className="flex gap-x-2">
          <Trash2 />
          Remove
        </Button>
      </div>
      <Controls />
      <Plot />
    </>
  );
}
