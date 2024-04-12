// import { useAppState } from "./contexts/AppStateContext";
import { useAppState } from "./contexts/AppStateContext";
import { usePlotData } from "./contexts/PlotDataContext";
import { Card, CardContent, CardHeader, CardTitle } from "./ui/card";

export function Comparisons() {
  // const app_state = useAppState();
  const plots = useAllPlots();
  console.log(`all plots`, plots);
  return (
    <Card>
      <CardHeader>
        <CardTitle>Comparison</CardTitle>
      </CardHeader>
      <CardContent>[TO DO]</CardContent>
    </Card>
  );
}

/**
 * Collect all plots from all cells and join them with their data
 */
function useAllPlots() {
  const plots = [];
  const app_state = useAppState();
  const plot_data = usePlotData();
  for (const cell of Object.values(app_state.cells)) {
    for (const [plot_id, plot] of Object.entries(cell.plots)) {
      const data = plot_data[plot_id];
      plots.push({
        cell,
        plot_id,
        ...plot,
        data
      });
    }
  }
  return plots;
}
