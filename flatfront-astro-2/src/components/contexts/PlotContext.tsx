import type { PlotID, PlotType } from "@/types";

import React from "react";
import * as d3 from "d3";
import {
  useAppState,
  useMergeState
} from "@/components/contexts/AppStateContext";
import { useCatalogCellID } from "@/components/contexts/CatalogIDContext";
import { assert_catalog_cell_id } from "@/utils";

const PlotIDContext = React.createContext<PlotID | undefined>(undefined);

export function PlotIDProvider({
  children,
  value
}: {
  children: React.ReactNode;
  value: PlotID;
}) {
  return (
    <PlotIDContext.Provider value={value}>{children}</PlotIDContext.Provider>
  );
}

export function usePlotID() {
  const plot_id = React.useContext(PlotIDContext);
  if (!plot_id) {
    throw new Error(`usePlotID must be used within a PlotIDProvider`);
  }
  return plot_id;
}

export function usePlotType() {
  const plot_id = usePlotID();
  const plot_type = useAppState()?.plots?.[plot_id];
  return plot_type;
}

export function usePlotState() {
  const plot_id = usePlotID();
  const plot_state = useAppState()?.plot_controls?.[plot_id];
  return plot_state;
}

export function useSetPlotControl() {
  const plot_id = usePlotID();
  const merge_state = useMergeState();
  return (plot_control_key: string, value: any) => {
    merge_state({
      plot_controls: {
        [plot_id]: {
          [plot_control_key]: value
        }
      }
    });
  };
}

export function useAddPlot() {
  const catalog_cell_id = useCatalogCellID();
  assert_catalog_cell_id(catalog_cell_id);
  const next_id: PlotID = `plot_${Date.now()}`;
  const merge_state = useMergeState();
  return (plot_type: PlotType) => {
    merge_state({
      plots: {
        [catalog_cell_id]: {
          [next_id]: plot_type
        }
      }
    });
  };
}

export function useRemovePlot() {
  const catalog_cell_id = useCatalogCellID();
  assert_catalog_cell_id(catalog_cell_id);
  // const merge_state = useMergeState();
  return (id: PlotID) => {
    console.log("delete plot", id);
  };
  // merge_state({ add_plot: { [catalog_cell_id]: { [id]: false } } });
}

export function usePlotsArray() {
  const catalog_cell_id = useCatalogCellID();
  assert_catalog_cell_id(catalog_cell_id);
  const plot_config = useAppState()?.plots?.[catalog_cell_id] ?? {};
  const plot_ids_unsorted: { plot_id: PlotID; plot_type: PlotType }[] =
    Object.entries(plot_config).map(
      ([plot_id, plot_type]: [PlotID, PlotType]) => ({
        plot_id,
        plot_type
      })
    );
  const sorted = d3.sort(plot_ids_unsorted, (d) => d.plot_id);
  return sorted;
}
