import React from "react";
import * as d3 from "d3";
import {
  PlotIDProvider,
  useAddPlot,
  usePlotsArray
} from "@/components/contexts/PlotContext";
import { Button } from "@/components/ui/button";
import { Separator } from "./ui/separator";
import { H4 } from "./ui/h4";
import { Trash2 } from "lucide-react";
import * as Plots from "./Plots";
import {
  Popover,
  PopoverClose,
  PopoverContent,
  PopoverTrigger
} from "./ui/popover";

const plot_wrappers = d3.sort(Object.values(Plots), (d) => d.order);

export function PlotSection() {
  return (
    <>
      <AddPlot />
      <PlotsList />
    </>
  );
}

function AddPlot() {
  // const add_plot = useAddPlot();
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
                <Button
                  onClick={() => {
                    console.log("haha");
                  }}
                >
                  {label}
                </Button>
              </PopoverClose>
            );
          })}
        </div>
      </PopoverContent>
    </Popover>
  );
  //   <div className="space-y-2">
  //   <Label>Add Plot</Label>
  //   <div className="grid gap-4 @2xl/cell:grid-cols-5 ">
  // {plot_wrappers.map(({ key, label }) => {
  //   return <Button key={key}>{label}</Button>;
  // })}
  //   </div>
  // </div>
  // const items = plot_wrappers.map(({ key, label }) => {
  //   return (
  //     <SelectItem key={key} value={key}>
  //       {label}
  //     </SelectItem>
  //   );
  // });
  // return (
  //   <Select
  //     value={value}
  //     onValueChange={(plot_type) => {
  //       // merge_state({
  //       //   set_catalog: {
  //       //     [catalog_cell_id]: catalog_id
  //       //   }
  //       // });
  //       console.log({ plot_type });
  //       set_value(undefined);
  //     }}
  //   >
  //     <SelectTrigger className="max-w-[40ch]">
  //       <SelectValue placeholder={`Add plot`} />
  //     </SelectTrigger>
  //     <SelectContent position="popper">
  //       <SelectGroup>{items}</SelectGroup>
  //     </SelectContent>
  //   </Select>
  // );
  // return <div> add</div>;
  // return (
  //   <Select
  //     disabled={!ready}
  //     value={selected?.name}
  //     onValueChange={(catalog_id) => {
  //       merge_state({
  //         set_catalog: {
  //           [catalog_cell_id]: catalog_id
  //         }
  //       });
  //     }}
  //   >
  //     <SelectTrigger className="max-w-[40ch] disabled:cursor-wait">
  //       <SelectValue
  //         placeholder={ready ? `Select a catalog...` : `Loading catalogs...`}
  //       />
  //     </SelectTrigger>
  //     <SelectContent position="popper">
  //       <SelectGroup>{items}</SelectGroup>
  //     </SelectContent>
  //   </Select>
  // );
  // return (
  //   <Button variant="outline" onClick={() => add_plot()}>
  //     Add Plot
  //   </Button>
  // );
}

function PlotsList() {
  const plots_array = usePlotsArray();
  const plot_components = plots_array.map(({ plot_id }, index) => {
    return (
      <React.Fragment key={plot_id}>
        {index === 0 ? null : <Separator />}
        <PlotIDProvider value={plot_id}>
          <Plot index={index} />
        </PlotIDProvider>
      </React.Fragment>
    );
  });
  return <>{plot_components}</>;
}

function Plot({ index }: { index: number }) {
  return (
    <div className="flex items-center justify-between">
      <H4>Plot {index + 1}</H4>
      <Button className="flex gap-x-2">
        <Trash2 />
        Remove
      </Button>
    </div>
  );
}
