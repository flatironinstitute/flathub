import type { PlotID } from "./types";
import * as d3 from "d3";
import { useAppState, useDispatch } from "./app-state";
import { useCatalogCellID } from "./components/CatalogContext";
import { assert_plot_id } from "./shared";

function usePlotsConfig(): Record<PlotID, boolean> {
  const catalog_cell_id = useCatalogCellID();
  const plots_config = useAppState()?.add_plot?.[catalog_cell_id] ?? {};
  return plots_config;
}

export function useAddPlot() {
  const catalog_cell_id = useCatalogCellID();
  const plots_config = usePlotsConfig();
  const number_of_plots = Object.values(plots_config).filter(Boolean).length;
  const next_id: PlotID = `plot_${number_of_plots}`;
  const dispatch = useDispatch();
  return () => {
    dispatch([`add_plot`, catalog_cell_id, next_id], true);
  };
}

export function useRemovePlot(id: PlotID) {
  const catalog_cell_id = useCatalogCellID();
  const dispatch = useDispatch();
  return () => {
    dispatch([`add_plot`, catalog_cell_id, id], false);
  };
}

export function usePlotIDs() {
  const catalog_cell_id = useCatalogCellID();
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
