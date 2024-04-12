import type { PlotID, PlotInfo, PlotType } from "@/types";

import React from "react";
import * as d3 from "d3";
import {
  useAppState,
  useMergeState,
  useSetAppState
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
  const catalog_cell_id = useCatalogCellID();
  const plot_id = usePlotID();
  const plot_type =
    useAppState()?.cells?.[catalog_cell_id]?.plots?.[plot_id]?.plot_type;
  return plot_type;
}

export function usePlotState() {
  const plot_id = usePlotID();
  const catalog_cell_id = useCatalogCellID();
  const cell = useAppState()?.cells?.[catalog_cell_id];
  const plot_state = cell?.plots?.[plot_id]?.plot_controls ?? undefined;
  return plot_state;
}

export function useSetPlotControl() {
  const plot_id = usePlotID();
  const catalog_cell_id = useCatalogCellID();
  const merge_state = useMergeState();
  return (plot_control_key: string, value: any) => {
    merge_state({
      cells: {
        [catalog_cell_id]: {
          plots: {
            [plot_id]: {
              plot_controls: {
                [plot_control_key]: value
              }
            }
          }
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
      cells: {
        [catalog_cell_id]: {
          plots: {
            [next_id]: {
              plot_id: next_id,
              plot_type
            }
          }
        }
      }
    });
  };
}

export function useRemovePlot() {
  const catalog_cell_id = useCatalogCellID();
  assert_catalog_cell_id(catalog_cell_id);
  const set_app_state = useSetAppState();
  return (id: PlotID) => {
    set_app_state((prev) => {
      const catalog_plots = prev.cells?.[catalog_cell_id]?.plots ?? {};
      delete catalog_plots[id];
    });
  };
}

export function usePlotsArray() {
  const catalog_cell_id = useCatalogCellID();
  assert_catalog_cell_id(catalog_cell_id);
  const plot_config = useAppState()?.cells?.[catalog_cell_id]?.plots ?? {};
  const plot_ids_unsorted: { plot_id: PlotID; plot_type: PlotType }[] =
    Object.entries(plot_config).map(
      ([plot_id, { plot_type }]: [PlotID, PlotInfo]) => ({ plot_id, plot_type })
    );
  const sorted = d3.sort(plot_ids_unsorted, (d) => d.plot_id);
  return sorted;
}
