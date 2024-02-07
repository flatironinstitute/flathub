import React from "react";
import { Trash2 } from "lucide-react";
import * as d3 from "d3";
import {
  PlotIDProvider,
  useAddPlot,
  usePlotID,
  usePlotType,
  usePlotsArray,
  useRemovePlot
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
      <PopoverContent className="w-40" align="start" avoidCollisions={false}>
        <div className="grid gap-4">
          {plot_wrappers.map(({ key, label }) => (
            <PopoverClose key={key} asChild>
              <Button
                variant="link"
                className="h-5 cursor-pointer justify-start p-0"
                onClick={() => add_plot(key)}
              >
                {label}
              </Button>
            </PopoverClose>
          ))}
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
  const plot_type = usePlotType();
  const label = plot_key_to_label.get(plot_type);
  const wrapper = plot_wrappers.find(({ key }) => key === plot_type);
  const { Plot, Controls } = wrapper;
  return (
    <>
      <div className="flex items-center justify-between">
        <H4>
          Plot {index + 1}: {label}
        </H4>
        <RemovePlotButton />
      </div>
      <div className="grid gap-x-4 @2xl:grid-cols-2">
        <Controls />
      </div>
      <Plot />
    </>
  );
}

function RemovePlotButton() {
  const plot_id = usePlotID();
  const remove_plot = useRemovePlot();
  return (
    <Button className="flex gap-x-2" onClick={() => remove_plot(plot_id)}>
      <Trash2 />
      Remove
    </Button>
  );
}
