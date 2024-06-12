import type * as schema from "../flathub-schema";
import React from "react";
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
import { useCatalogMetadata } from "./contexts/CatalogMetadataContext";
import type { FieldID } from "@/types";

type RawDataDownloadFormat =
  schema.operations["download"]["parameters"]["path"]["format"];

type AttachmentFormat =
  schema.operations["attachments"]["parameters"]["path"]["format"];

export default function DownloadSection() {
  const metadata = useCatalogMetadata();
  const attachment_download = (() => {
    if (metadata?.attachment_field_ids.size === 0) {
      return null;
    }
    return (
      <div className="space-y-2">
        <Label>
          Bulk Download Attachments
          <BulkDownloadAttachments />
        </Label>
      </div>
    );
  })();
  return (
    <>
      <div className="space-y-2">
        <Label>
          Bulk Download Raw Data
          <BulkDownloadData />
        </Label>
      </div>
      {attachment_download}
    </>
  );
}

function BulkDownloadData() {
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

  const url = (() => {
    // let url = undefined;
    if (!selected_format) return undefined;
    const url = new URL(
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
      url.searchParams.set(key, url_value.toString());
    }
    return url;
  })();

  const items = formats.map((format) => (
    <SelectItem key={format} value={format}>
      {format}
    </SelectItem>
  ));

  return (
    <div className="grid grid-cols-[24rem_7rem] gap-x-4">
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

function BulkDownloadAttachments() {
  const catalog_id = useCatalogID();
  const metadata = useCatalogMetadata();
  const filters = useFilterValuesWithFieldNames();

  const [selected_field, set_selected_field] = React.useState<
    FieldID | undefined
  >(undefined);

  const [format, set_format] = React.useState<AttachmentFormat>("zip");

  const attachment_field_ids_set = metadata?.attachment_field_ids ?? new Set();
  const attachment_field_ids = [...attachment_field_ids_set];

  const items = attachment_field_ids.map((field_id) => (
    <SelectItem key={field_id} value={field_id}>
      {field_id}
    </SelectItem>
  ));

  const formats: AttachmentFormat[] = ["zip", "uris", "sh"];

  const url = (() => {
    if (!selected_field) return undefined;
    const url = new URL(
      `/api/${catalog_id}/attachments/${format}/${selected_field}`,
      FLATHUB_API_BASE_URL
    );
    for (const [key, value] of Object.entries(filters)) {
      let url_value = value;
      if (typeof value === `object` && (`gte` in value || `lte` in value)) {
        url_value = JSON.stringify(value);
      }
      url.searchParams.set(key, url_value.toString());
    }
    return url;
  })();

  return (
    <div className="grid grid-cols-[17rem_6rem_7rem] gap-x-4">
      <Select
        value={selected_field}
        onValueChange={(field_id) => set_selected_field(field_id)}
      >
        <SelectTrigger className="whitespace-nowrap text-[clamp(0.8rem,4.6cqi,1rem)]">
          <SelectValue placeholder="Choose field..." />
        </SelectTrigger>
        <SelectContent position="popper">
          <SelectGroup>{items}</SelectGroup>
        </SelectContent>
      </Select>
      <Select
        value={format}
        onValueChange={(format) => set_format(format as AttachmentFormat)}
      >
        <SelectTrigger className="whitespace-nowrap text-[clamp(0.8rem,4.6cqi,1rem)]">
          <SelectValue />
        </SelectTrigger>
        <SelectContent position="popper">
          <SelectGroup>
            {formats.map((format) => (
              <SelectItem key={format} value={format}>
                {format}
              </SelectItem>
            ))}
          </SelectGroup>
        </SelectContent>
      </Select>
      <Button asChild>
        <a
          href={url ? url.toString() : undefined}
          download={selected_field}
          data-disabled={url ? undefined : true}
        >
          Download
        </a>
      </Button>
    </div>
  );
}
