import type { CellID, TopResponse, TopResponseEntry } from "@/types";
import * as d3 from "d3";
import clsx from "clsx";

import { useQuery } from "@tanstack/react-query";
import { fetch_api_get, format, log } from "@/utils";
import {
  CardContent,
  Card,
  CardHeader,
  CardTitle,
  CardDescription
} from "@/components/ui/card";
import {
  Select,
  SelectContent,
  SelectGroup,
  SelectItem,
  SelectLabel,
  SelectTrigger,
  SelectValue
} from "@/components/ui/select";
import {
  Dialog,
  DialogContent,
  DialogDescription,
  DialogHeader,
  DialogTitle,
  DialogTrigger
} from "@/components/ui/dialog";
import { Separator } from "@/components/ui/separator";
import { Katex } from "@/components/ui/katex";
import { Button } from "@/components/ui/button";
import {
  CatalogCellIDProvider,
  useCatalogCellID,
  useCatalogID
} from "@/components/contexts/CatalogCellIDContext";
import { CatalogMetadataProvider } from "@/components/contexts/CatalogMetadataContext";
import { useMergeState } from "@/components/contexts/AppStateContext";
import { useCatalogMetadata } from "@/components/contexts/CatalogMetadataContext";
import { ColumnsProvider } from "@/components/contexts/ColumnsContext";
import { FiltersProvider } from "@/components/contexts/FiltersContext";
import { FieldsBrowser } from "@/components/FieldsBrowser";
import { AddFilterDropdown, FilterSection } from "@/components/FilterSection";
import { Trash2 } from "lucide-react";

export function CatalogCell({ id: catalog_cell_id }: { id: CellID.Catalog }) {
  return (
    <CatalogCellIDProvider value={catalog_cell_id}>
      <CatalogMetadataProvider>
        <ColumnsProvider>
          <FiltersProvider>
            <CatalogCellContents />
          </FiltersProvider>
        </ColumnsProvider>
      </CatalogMetadataProvider>
    </CatalogCellIDProvider>
  );
}

function CatalogCellContents() {
  const catalog_id = useCatalogID();

  return (
    <Card className="w-[min(1000px,90dvw)] @container/cell">
      <CardHeader className="flex flex-row items-center justify-between">
        <CardTitle>Catalog</CardTitle>
        <Button variant="outline" className="flex flex-row gap-x-2">
          <Trash2 /> Delete
        </Button>
      </CardHeader>
      <CardContent className="flex flex-col gap-4 @md/cell:flex-row">
        <CatalogSelect />
        <AboutThisCatalog />
      </CardContent>
      <Separator />
      <CardHeader>
        <CardTitle>Fields</CardTitle>
      </CardHeader>
      <CardContent className="space-y-4">
        <FieldsBrowser key={catalog_id} />
      </CardContent>
      <Separator />
      <CardHeader>
        <CardTitle>Filters</CardTitle>
      </CardHeader>
      <CardContent className="space-y-4">
        <AddFilterDropdown />
        <FilterSection />
      </CardContent>
    </Card>
  );
}

function CatalogSelect() {
  const catalog_cell_id = useCatalogCellID();
  const catalog_id = useCatalogID();
  const get_title = (d: TopResponseEntry) => d?.title;
  const catalog_list_query = useQuery({
    queryKey: ["top"],
    queryFn: async (): Promise<TopResponse> => fetch_api_get<TopResponse>(`/`)
  });
  const catalog_list_unsorted = catalog_list_query.data ?? [];
  const catalog_list = d3.sort(catalog_list_unsorted, get_title);
  const ready = catalog_list.length > 0;
  const selected = catalog_list.find((d) => d.name === catalog_id);
  const merge_state = useMergeState();
  const items = catalog_list.map(({ name, title }) => {
    return (
      <SelectItem key={name} value={name}>
        {title}
      </SelectItem>
    );
  });
  return (
    <Select
      disabled={!ready}
      value={selected?.name}
      onValueChange={(catalog_id) => {
        merge_state({
          set_catalog: {
            [catalog_cell_id]: catalog_id
          }
        });
      }}
    >
      <SelectTrigger className="max-w-[40ch] disabled:cursor-wait">
        <SelectValue
          placeholder={ready ? `Select a catalog...` : `Loading catalogs...`}
        />
      </SelectTrigger>
      <SelectContent position="popper">
        <SelectGroup>{items}</SelectGroup>
      </SelectContent>
    </Select>
  );
}

function AboutThisCatalog() {
  const catalog_metadata = useCatalogMetadata();
  const { title, descr, count } = catalog_metadata?.response ?? {};
  return (
    <Dialog>
      <DialogTrigger asChild disabled={!catalog_metadata}>
        <Button variant="outline">About This Catalog</Button>
      </DialogTrigger>
      <DialogContent className="max-h-[80dvh] overflow-y-scroll">
        <DialogHeader>
          <DialogTitle>{title}</DialogTitle>
        </DialogHeader>
        <Katex
          className={clsx(
            `text-sm`,
            `space-y-4 leading-[1.4]`,
            `[&_a]:underline [&_ul]:list-disc [&_ul]:space-y-4 [&_ul]:pb-3 [&_ul]:ps-10`
          )}
        >
          {descr}
        </Katex>
        <p>Rows: {format.commas(count)}</p>
        <p>Variables: {catalog_metadata?.hierarchy.leaves().length}</p>
      </DialogContent>
    </Dialog>
  );
}
