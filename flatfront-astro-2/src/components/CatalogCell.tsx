import React from "react";
import { Trash2 } from "lucide-react";
import clsx from "clsx";
import type { CellID } from "@/types";
import {
  CatalogIDProvider,
  useCatalogID
} from "@/components/contexts/CatalogIDContext";
import { CatalogMetadataProvider } from "@/components/contexts/CatalogMetadataContext";
import { useCatalogMetadata } from "@/components/contexts/CatalogMetadataContext";
import { ColumnsProvider } from "@/components/contexts/ColumnsContext";
import { FiltersProvider } from "@/components/contexts/FiltersContext";
import { RandomProvider } from "@/components/contexts/RandomContext";
import { MatchingRowsProvider } from "./contexts/MatchingRowsContext";
import { CardContent, Card, CardHeader, CardTitle } from "@/components/ui/card";
import { Separator } from "@/components/ui/separator";
import { Button } from "@/components/ui/button";
import { FieldsBrowser } from "@/components/FieldsBrowser";
import { AddFilterDropdown, FilterSection } from "@/components/FilterSection";
import { TableSection } from "@/components/TableSection";
import { PlotSection } from "@/components/PlotSection";
import { AboutThisCatalog } from "./AboutThisCatalog";
import { RandomSampleControls } from "./RandomSampleControls";

export function CatalogCell({ id: catalog_cell_id }: { id: CellID.Catalog }) {
  return (
    <CatalogIDProvider value={catalog_cell_id}>
      <CatalogMetadataProvider>
        <ColumnsProvider>
          <FiltersProvider>
            <RandomProvider>
              <MatchingRowsProvider>
                <CatalogCellContents />
              </MatchingRowsProvider>
            </RandomProvider>
          </FiltersProvider>
        </ColumnsProvider>
      </CatalogMetadataProvider>
    </CatalogIDProvider>
  );
}

function CatalogCellContents() {
  const catalog_id = useCatalogID();
  const catalog_title = useCatalogMetadata()?.response?.title ?? ``;

  const result_section_ref = React.useRef<HTMLDivElement>(null);

  const filters_section = (
    <>
      <CardHeader>
        <CardTitle>Filters</CardTitle>
      </CardHeader>
      <CardContent className="space-y-4">
        <AddFilterDropdown />
        <FilterSection />
      </CardContent>
      <Separator />
      <CardHeader>
        <CardTitle>Random Sample</CardTitle>
      </CardHeader>
      <CardContent className="space-y-4">
        <RandomSampleControls />
      </CardContent>
      <Separator />
    </>
  );

  const results_section = (
    <div ref={result_section_ref} className="grid @5xl:grid-cols-[400px_1fr]">
      <div>{filters_section}</div>
      <div>
        <CardHeader>
          <CardTitle>Results</CardTitle>
        </CardHeader>
        <CardContent className="space-y-4">
          <TableSection />
        </CardContent>
        <Separator />
        <CardHeader>
          <CardTitle>Plots</CardTitle>
        </CardHeader>
        <CardContent className="space-y-4">
          <PlotSection />
        </CardContent>
      </div>
    </div>
  );

  return (
    <>
      <Card
        className={clsx(
          `w-[min(1500px,90dvw)] transform transition-transform duration-300 ease-in-out @container/cell`
        )}
      >
        <CardHeader className="grid items-center gap-4 space-y-0 @2xl:grid-cols-[1fr_min-content_min-content]">
          <CardTitle>Catalog: {catalog_title}</CardTitle>
          <AboutThisCatalog catalog_id={catalog_id} />
          <Button variant="outline" className="flex flex-row gap-x-2">
            <Trash2 /> Delete
          </Button>
        </CardHeader>
        <Separator />
        <CardHeader>
          <CardTitle>Fields</CardTitle>
        </CardHeader>
        <CardContent className="space-y-4">
          <FieldsBrowser key={catalog_id} />
        </CardContent>
        <Separator />
        {results_section}
        <Separator />
        <CardHeader>
          <CardTitle>Python</CardTitle>
        </CardHeader>
        <CardContent className="space-y-4">
          <div>python</div>
        </CardContent>
      </Card>
    </>
  );
}
