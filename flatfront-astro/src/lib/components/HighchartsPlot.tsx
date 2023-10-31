import Highcharts from "highcharts";
import Highcharts3D from "highcharts/highcharts-3d";
import HighchartsExporting from "highcharts/modules/exporting";
import HighchartsExportData from "highcharts/modules/export-data";
import HighchartsHeatmap from "highcharts/modules/heatmap";
import HighchartsBoost from "highcharts/modules/boost";
import HighchartsReact from "highcharts-react-official";
import HighchartsDraggable3D from "highcharts-draggable-3d";

import { useIsDarkMode } from "../dark-mode";

declare module "highcharts" {
  interface Chart3dOptions {
    drag?: {
      enabled?: boolean;
      minAlpha?: number;
      maxAlpha?: number;
      minBeta?: number;
      maxBeta?: number;
      snap?: number;
      animateSnap?: boolean;
      speed?: number;
      flipAxes?: boolean;
    };
  }
}

Highcharts3D(Highcharts);
HighchartsDraggable3D(Highcharts);
HighchartsExporting(Highcharts);
HighchartsExportData(Highcharts);
HighchartsHeatmap(Highcharts);
HighchartsBoost(Highcharts);

export default function HighchartsPlot({
  options
}: {
  options: Highcharts.Options;
}) {
  const dark_mode = useIsDarkMode();
  const className = dark_mode ? `highcharts-dark` : `highcharts-light`;
  return (
    <HighchartsReact
      highcharts={Highcharts}
      options={options}
      containerProps={{ className }}
    />
  );
}
