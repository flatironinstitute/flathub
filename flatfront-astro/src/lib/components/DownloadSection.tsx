import type * as schema from "../flathub-schema";
import React from "react";
import { useMatchingRowsText } from "../contexts/MatchingRowsContext";
import { BigButton, Select } from "./Primitives";
import { useCatalogID } from "../contexts/CatalogContext";
import { FLATHUB_API_BASE_URL, log } from "../shared";
import { useFilterValues } from "../contexts/FiltersContext";
import { useColumns } from "../contexts/ColumnsContext";
import clsx from "clsx";
import { useSort } from "../contexts/SortContext";

type RawDataDownloadFormat =
  schema.operations["download"]["parameters"]["path"]["format"];

export default function DownloadSection() {
  return (
    <>
      <div>{useMatchingRowsText()}</div>
      <div>Bulk Download Raw Data</div>
      <BulkDataDownload />
    </>
  );
}

function BulkDataDownload() {
  const catalog_id = useCatalogID();
  const [format, set_format] = React.useState<RawDataDownloadFormat | null>(
    null
  );
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

  const filters = useFilterValues();
  const columns = useColumns();
  const sort = useSort();

  let url = undefined;
  if (format) {
    url = new URL(`/api/${catalog_id}/data/${format}`, FLATHUB_API_BASE_URL);
    for (const column of columns) {
      url.searchParams.append(`fields`, column);
    }
    for (const { field, order } of sort) {
      const sign = { asc: `+`, desc: `-` }[order];
      url.searchParams.append(`sort`, `${sign}${field}`);
    }
    for (const [key, value] of Object.entries(filters)) {
      let url_value = value;
      if (
        typeof url_value === `object` &&
        `gte` in url_value &&
        `lte` in url_value
      ) {
        url_value = [url_value.gte, url_value.lte];
      }
      url.searchParams.set(key, url_value);
    }
  }

  return (
    <div className="flex gap-x-4">
      {/* <RadixLabel.Root className="flex items-center justify-center whitespace-nowrap font-bold">
        File Format:
      </RadixLabel.Root> */}
      <Select
        placeholder="Choose format..."
        options={formats}
        value={format}
        disabled={!catalog_id}
        onValueChange={set_format}
      />
      <a
        href={url ? url.toString() : undefined}
        download={catalog_id}
        data-disabled={url ? undefined : true}
        className={clsx(
          BigButton.className,
          `data-[disabled]:cursor-not-allowed data-[disabled]:opacity-50`
        )}
      >
        Download
      </a>
    </div>
  );
}
