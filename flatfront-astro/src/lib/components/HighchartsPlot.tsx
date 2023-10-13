import Highcharts from "highcharts";
import HighchartsExporting from "highcharts/modules/exporting";
import HighchartsExportData from "highcharts/modules/export-data";
import HighchartsHeatmap from "highcharts/modules/heatmap";
import HighchartsBoost from "highcharts/modules/boost";
import HighchartsReact from "highcharts-react-official";

import { useIsDarkMode } from "../dark-mode";

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
