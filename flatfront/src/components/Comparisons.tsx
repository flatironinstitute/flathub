import type {
  CatalogCell,
  CatalogHierarchyNode,
  CatalogResponse,
  Comparison,
  PlotID,
  PlotInfo
} from "@/types";
import type { QueryKey } from "@tanstack/react-query";
import React from "react";
import clsx from "clsx";
import * as Plot from "@observablehq/plot";
import { useAppState } from "./contexts/AppStateContext";
import { usePlotData } from "./contexts/PlotDataContext";
import { useMergeState } from "./contexts/AppStateContext";
import { Card, CardContent, CardHeader, CardTitle } from "./ui/card";
import { Button } from "@/components/ui/button";
import {
  Popover,
  PopoverClose,
  PopoverContent,
  PopoverTrigger
} from "./ui/popover";
import { Separator } from "./ui/separator";
import { H4 } from "./ui/h4";
import { query_client } from "@/query_client";
import { Checkbox } from "./ui/checkbox";
import { PlotStatusWrapper, get_observable_options } from "./PlotHelpers";
import { ObservablePlot } from "./ObservablePlot";
import { create_catalog_hierarchy } from "./contexts/CatalogMetadataContext";
import { get_field_titles } from "@/utils";

export function ComparisonsCard() {
  return (
    <Card className={clsx(`w-[min(1200px,90dvw)]`)}>
      <CardHeader>
        <CardTitle>Comparisons</CardTitle>
      </CardHeader>
      <CardContent>
        <AddComparison />
      </CardContent>
      <Comparisons />
    </Card>
  );
}

const comparison_types = [
  {
    key: "combined_histogram",
    title: "Combined Histogram"
  },
  {
    key: `combined_scatterplot`,
    title: `Combined Scatterplot`,
    disabled: true
  }
];

function AddComparison() {
  const merge_state = useMergeState();
  const next_comparison_id = `comparison_${Date.now()}`;
  return (
    <Popover>
      <PopoverTrigger asChild>
        <Button>Add Comparison</Button>
      </PopoverTrigger>
      <PopoverContent align="start" avoidCollisions={false}>
        <div className="grid gap-4">
          {comparison_types.map(({ key, title, disabled }) => (
            <PopoverClose key={key} asChild>
              <Button
                disabled={disabled}
                variant="link"
                className="h-5 justify-start p-0 disabled:cursor-not-allowed disabled:opacity-50"
                onClick={() =>
                  merge_state({
                    comparisons: {
                      [next_comparison_id]: {
                        comparison_id: next_comparison_id,
                        comparison_type: key
                      }
                    }
                  })
                }
              >
                {title}
              </Button>
            </PopoverClose>
          ))}
        </div>
      </PopoverContent>
    </Popover>
  );
}

function Comparisons() {
  const { comparisons = {} } = useAppState();
  const comparisons_array = Object.values(comparisons);
  const comparison_components = comparisons_array.map((comparison, index) => {
    return (
      <ComparisonContext.Provider
        value={{ ...comparison, index }}
        key={comparison.comparison_id}
      >
        <Separator />
        <Comparison />
      </ComparisonContext.Provider>
    );
  });
  return <>{comparison_components}</>;
}

function Comparison() {
  const { comparison_type, index } = React.useContext(ComparisonContext);
  const title: string = comparison_types.find(
    (d) => d.key === comparison_type
  ).title;

  // TODO: More plot types
  const PlotComponent = CombinedHistogram;
  return (
    <>
      <CardHeader>
        <H4>
          Comparison {index + 1}: {title}
        </H4>
      </CardHeader>
      <CardContent>
        <ComparisonDataSelection />
        <div className="h-10" />
        <PlotComponent />
      </CardContent>
    </>
  );
}

function ComparisonDataSelection() {
  const {
    comparison_type,
    comparison_id,
    plots: comparison_plot_ids
  } = useComparison();
  const all_plots = useAllPlots();
  const merge_state = useMergeState();
  return (
    <>
      <h5>Data</h5>
      <div>
        {all_plots
          .filter((plot_meta) => {
            if (
              comparison_type === `combined_histogram` &&
              plot_meta.plot_type === `histogram`
            ) {
              return true;
            }
            return false;
          })
          .map((plot_meta) => {
            // TODO: Handling variables for other plot types?
            const variable = plot_meta.plot_controls?.x_axis;
            const node = plot_meta.catalog?.hierarchy?.find(
              (d) => d.data?.name === variable
            );
            const variable_title = get_field_titles(node).join(` `);
            const plot_id = plot_meta.plot_id as PlotID;
            const is_selected = comparison_plot_ids?.[plot_id];
            const label = `${plot_meta.catalog?.title}: ${variable_title}`;
            return (
              <div
                key={plot_meta.plot_id}
                className="flex flex-row items-center space-x-3"
              >
                <Checkbox
                  checked={is_selected}
                  onCheckedChange={(checked) => {
                    console.log(`checked`, checked);
                    merge_state({
                      comparisons: {
                        [comparison_id]: {
                          plots: {
                            [plot_id]: !!checked
                          }
                        }
                      }
                    });
                  }}
                />
                <label>{label}</label>
              </div>
            );
          })}
      </div>
    </>
  );
}

function CombinedHistogram() {
  const { plots: comparison_plot_ids } = useComparison();
  const all_plots = useAllPlots();
  const combined_data = all_plots
    .filter((d) => comparison_plot_ids?.[d.plot_id])
    .map((plot_meta) => {
      const catalog_title = plot_meta.catalog?.title;
      const variable = plot_meta.plot_controls?.x_axis;
      const node = plot_meta.catalog?.hierarchy?.find(
        (d) => d.data?.name === variable
      );
      const variable_title = get_field_titles(node).join(` `);
      const source = `${catalog_title}: ${variable_title}`;
      return (
        plot_meta.data?.map((d) => {
          return {
            ...d,
            catalog_title,
            variable_title,
            source
          };
        }) ?? []
      );
    })
    .flat(1);
  const plot_options: Plot.PlotOptions = get_observable_options({
    x: {
      label: `Value`,
      type: `log`
      // type: x_axis.log_mode ? `log` : `linear`
    },
    y: {
      label: `Count`,
      type: `log`
      // type: y_axis.log_mode ? `log` : `linear`
    },
    color: {
      legend: true
    },
    marks: [
      Plot.dot(combined_data, {
        x: `x`,
        y: `count`,
        stroke: `source`,
        tip: true
      }),
      Plot.line(combined_data, {
        x: `x`,
        y: `count`,
        z: `source`,
        stroke: `source`
      })
    ]
  });

  const plot = Plot.plot(plot_options);
  return (
    <PlotStatusWrapper status={{ title: null }}>
      <ObservablePlot plot={plot} />
    </PlotStatusWrapper>
  );
}

type AllPlotsItem = {
  cell: CatalogCell;
  catalog: CatalogResponse & { hierarchy: CatalogHierarchyNode };
  data: any[];
  plot_id: PlotID;
  index?: number;
} & PlotInfo;

/**
 * Collect all plots from all cells and join them with their data
 */
function useAllPlots(): AllPlotsItem[] {
  const plots: AllPlotsItem[] = [];
  const app_state = useAppState();
  const plot_data = usePlotData();
  const catalog_metadatas = React.useMemo(() => {
    return Object.fromEntries(
      query_client
        .getQueriesData({
          type: `all`,
          queryKey: [`catalog`],
          exact: false
        })
        .map(([key, response]: [QueryKey, CatalogResponse]) => [
          key[1],
          { ...response, hierarchy: create_catalog_hierarchy(response) }
        ])
    );
  }, [plot_data]);
  for (const cell of Object.values(app_state.cells)) {
    const catalog_metadata = catalog_metadatas[cell.catalog_id];
    if (!cell.plots) continue;
    let index = 0;
    for (const [plot_id, plot] of Object.entries(cell.plots)) {
      index += 1;
      const data = plot_data[plot_id];
      plots.push({
        ...plot,
        cell,
        catalog: catalog_metadata,
        plot_id: plot_id as PlotID,
        data,
        index
      });
    }
  }
  return plots;
}

const ComparisonContext = React.createContext<Comparison | undefined>(
  undefined
);

function useComparison() {
  const comparison = React.useContext(ComparisonContext);
  if (!comparison) {
    throw new Error(`useComparison must be used within a ComparisonProvider`);
  }
  return comparison;
}
