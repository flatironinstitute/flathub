import type * as schema from "../flathub-schema";
import React from "react";
import clsx from "clsx";
import { Label } from "@/components/ui/label";
import { useCatalogID } from "@/components/contexts/CatalogIDContext";
import { useFilterValuesWithFieldNames } from "./contexts/FiltersContext";
import { useColumnNames } from "./contexts/ColumnsContext";
import { FLATHUB_API_BASE_URL } from "@/utils";
import { useSort } from "./contexts/SortContext";
import {
  Select,
  SelectContent,
  SelectGroup,
  SelectItem,
  SelectTrigger,
  SelectValue
} from "./ui/select";
import { Button } from "./ui/button";

type RawDataDownloadFormat =
  schema.operations["download"]["parameters"]["path"]["format"];

export default function DownloadSection() {
  return (
    <div className="space-y-2">
      <Label>Bulk Download Raw Data</Label>
      <BulkDataDownload />
    </div>
  );
}

function BulkDataDownload() {
  const catalog_id = useCatalogID();
  const [selected_format, set_selected_format] = React.useState<
    RawDataDownloadFormat | undefined
  >(undefined);
  const formats: RawDataDownloadFormat[] = [
    `fits`,
    `fits.gz`,
    `npy`,
    `npy.gz`,
    `ecsv`,
    `ecsv.gz`,
    `ndjson`,
    `ndjson.gz`,
    `json`,
    `json.gz`,
    `csv`,
    `csv.gz`
  ];

  const filters = useFilterValuesWithFieldNames();
  const columns = useColumnNames();
  const sort = useSort();

  let url = undefined;
  if (selected_format) {
    url = new URL(
      `/api/${catalog_id}/data/${selected_format}`,
      FLATHUB_API_BASE_URL
    );
    for (const column of columns) {
      url.searchParams.append(`fields`, column);
    }
    for (const { field, order } of sort) {
      const sign = { asc: `+`, desc: `-` }[order];
      url.searchParams.append(`sort`, `${sign}${field}`);
    }
    for (const [key, value] of Object.entries(filters)) {
      let url_value = value;
      if (typeof value === `object` && (`gte` in value || `lte` in value)) {
        url_value = JSON.stringify(value);
      }
      url.searchParams.set(key, url_value);
    }
  }

  console.log(url?.searchParams?.toString());

  const items = formats.map((format) => (
    <SelectItem key={format} value={format}>
      {format}
    </SelectItem>
  ));

  return (
    <div className="flex gap-x-4 max-w-[500px]">
      <Select
        value={selected_format}
        onValueChange={(format) =>
          set_selected_format(format as RawDataDownloadFormat)
        }
      >
        <SelectTrigger className="whitespace-nowrap text-[clamp(0.8rem,4.6cqi,1rem)]">
          <SelectValue placeholder="Choose format..." />
        </SelectTrigger>
        <SelectContent position="popper">
          <SelectGroup>{items}</SelectGroup>
        </SelectContent>
      </Select>

      <Button asChild>
        <a
          href={url ? url.toString() : undefined}
          download={catalog_id}
          data-disabled={url ? undefined : true}
        >
          Download
        </a>
      </Button>
    </div>
  );
}
