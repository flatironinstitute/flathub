import { csvFormat } from "d3";
import { Button } from "./ui/button";

export function DownloadPlotCSV({ data }: { data: any }) {
  const handle_click = () => {
    const csv = csvFormat(data);
    const blob = new Blob([csv], { type: "text/csv" });
    const url = URL.createObjectURL(blob);
    const link = document.createElement("a");
    link.href = url;
    link.download = "plot.csv";
    link.click();
  };
  return <Button onClick={handle_click}>Download CSV</Button>;
}
