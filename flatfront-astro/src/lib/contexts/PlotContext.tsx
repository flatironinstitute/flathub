import type { PlotID } from "../types";

import React from "react";
import * as d3 from "d3";
import { useAppState, useMergeState } from "./AppStateContext";
import { useCatalogCellID } from "./CatalogContext";
import { assert_plot_id, assert_catalog_cell_id } from "../shared";

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
  const plot_type = useAppState()?.set_plot_type?.[plot_id];
  return plot_type;
}

export function useAddPlot() {
  const catalog_cell_id = useCatalogCellID();
  assert_catalog_cell_id(catalog_cell_id);
  const next_id: PlotID = `plot_${Date.now()}`;
  const merge_state = useMergeState();
  return () => {
    merge_state({
      add_plot: {
        [catalog_cell_id]: {
          [next_id]: true
        }
      }
    });
  };
}

export function useRemovePlot(id: PlotID) {
  const catalog_cell_id = useCatalogCellID();
  assert_catalog_cell_id(catalog_cell_id);
  const merge_state = useMergeState();
  return () =>
    merge_state({ add_plot: { [catalog_cell_id]: { [id]: false } } });
}

export function usePlotIDs() {
  const catalog_cell_id = useCatalogCellID();
  assert_catalog_cell_id(catalog_cell_id);
  const plot_config = useAppState()?.add_plot?.[catalog_cell_id] ?? {};
  const plot_ids_unsorted: PlotID[] = Object.entries(plot_config)
    .filter(([, value]) => value)
    .map(([key]) => {
      assert_plot_id(key);
      return key;
    });
  const plot_ids: PlotID[] = d3.reverse(plot_ids_unsorted);
  return plot_ids;
}
