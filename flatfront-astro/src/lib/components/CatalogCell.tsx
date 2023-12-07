import type {
  CatalogHierarchyNode,
  CellID,
  PlotID,
  TopResponse,
  TopResponseEntry
} from "../types";

import React from "react";
import * as d3 from "d3";
import { useQuery } from "@tanstack/react-query";
import { useDebounce } from "@uidotdev/usehooks";
import clsx from "clsx";
import { fetch_api_get, format, get_field_titles } from "../shared";
import {
  CatalogProvider,
  useCatalogCellID,
  useCatalogID
} from "../contexts/CatalogContext";
import { useAddPlot, usePlotIDs } from "../contexts/PlotContext";
import { FiltersProvider, useFilterNames } from "../contexts/FiltersContext";
import {
  CatalogMetadataProvider,
  useCatalogMetadata
} from "../contexts/CatalogMetadataContext";
import {
  RandomProvider,
  useRandomConfig,
  useSetRandomConfig
} from "../contexts/RandomContext";
import { useMergeState } from "../contexts/AppStateContext";
import { ColumnsProvider } from "../contexts/ColumnsContext";
import {
  MatchingRowsProvider,
  useMatchingRowsText
} from "../contexts/MatchingRowsContext";
import {
  BigButton,
  CellWrapper,
  Combobox,
  Dialog,
  FieldTitles,
  Heading,
  NumberInput,
  Placeholder,
  Select,
  Separator,
  SimpleLabel,
  SliderWithText
} from "./Primitives";
import TableSection from "./Table";
import PlotSection from "./PlotSection";
import BrowseFieldsDialog, { FieldsTable } from "./BrowseFieldsDialog";
import Katex from "./Katex";
import FilterControls from "./FilterControls";
import { ScrollArea } from "./ScrollArea";

export default function CatalogCell({
  id: catalog_cell_id
}: {
  id: CellID.Catalog;
}) {
  return (
    <CatalogProvider value={catalog_cell_id}>
      <CatalogMetadataProvider>
        <FiltersProvider>
          <RandomProvider>
            <MatchingRowsProvider>
              <ColumnsProvider>
                <CatalogCellContents />
              </ColumnsProvider>
            </MatchingRowsProvider>
          </RandomProvider>
        </FiltersProvider>
      </CatalogMetadataProvider>
    </CatalogProvider>
  );
}
function CatalogCellContents() {
  const catalog_id = useCatalogID();

  return (
    <CellWrapper className="@container/cell">
      <div className="grid gap-x-8 gap-y-4 @2xl/cell:grid-cols-6">
        <CatalogSelect className="@2xl/cell:col-span-2" />
        <AboutThisCatalog className="@2xl/cell:col-span-2" />
        <Heading className="@2xl/cell:col-span-6 @2xl/cell:col-start-1">
          Fields
        </Heading>
        <div className="@2xl/cell:col-span-6">
          <FieldsTable key={catalog_id} />
        </div>
        <div className="space-y-4 @2xl/cell:col-span-2 @2xl/cell:col-start-1">
          <Heading className="@2xl/cell:col-span-2 @2xl/cell:col-start-1">
            Filters
          </Heading>
          <AddFilterSelect className="@2xl/cell:col-span-2 @2xl/cell:col-start-1" />
          <div className="grid gap-4 @2xl/cell:col-span-2 @2xl/cell:col-start-1 @2xl/cell:grid-cols-1">
            <FilterControls />
          </div>
          <Heading className="@2xl/cell:col-span-2 @2xl/cell:col-start-1">
            Random Sample
          </Heading>
          <div className="grid gap-4 @2xl/cell:col-span-2 @2xl/cell:col-start-1 @2xl/cell:grid-cols-1">
            <RandomSampleControls />
          </div>
        </div>

        <div className="space-y-4 @2xl/cell:col-span-4 @2xl/cell:col-start-3 @2xl/cell:row-start-4">
          <Heading>Results</Heading>
          <div className="@2xl/cell:col-span-6">{useMatchingRowsText()}</div>
          <AddPlotButton className="@2xl/cell:col-span-2" />
          <div className="@2xl/cell:col-span-6">
            <Plots />
            <Separator />
            <TableSection />
            <Separator />
            <div className="space-y-4">
              <SimpleLabel>python</SimpleLabel>
              <Placeholder>TODO: Python</Placeholder>
            </div>
          </div>
        </div>
      </div>
    </CellWrapper>
  );
}

function CatalogSelect({ className }: { className?: string }) {
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
  return (
    <Select
      placeholder={ready ? "Select a catalog..." : "Loading catalogs..."}
      options={catalog_list}
      getKey={(d) => d?.name}
      getDisplayName={get_title}
      disabled={!ready}
      value={selected}
      onValueChange={(d) => {
        const catalog_id = d?.name;
        merge_state({
          set_catalog: {
            [catalog_cell_id]: catalog_id
          }
        });
      }}
      triggerClassName={className}
    />
  );
}

function AboutThisCatalog({ className }: { className?: string }) {
  const catalog_id = useCatalogID();
  const catalog_metadata = useCatalogMetadata();
  const hierarchy = catalog_metadata?.hierarchy;
  const { title, descr, count } = catalog_metadata?.response ?? {};
  const contents = (() => {
    if (!catalog_metadata) return null;
    const nodes = catalog_metadata.depth_first.length;
    const h = hierarchy.height;
    const ess = h > 1 ? `s` : ``;
    const hierarchy_info = `Hierarchy: ${nodes} nodes, nested ${h} level${ess} deep`;
    return (
      <div className="flex max-h-[80dvh] w-[min(80dvw,800px)] flex-col gap-y-4 overflow-y-scroll p-4">
        <Heading>{title}</Heading>
        <Katex className="space-y-4 leading-[1.4] [&_a]:underline [&_ul]:list-disc [&_ul]:space-y-4 [&_ul]:pb-3 [&_ul]:ps-10">
          {descr}
        </Katex>
        <p>Rows: {format.commas(count)}</p>
        <p>Variables: {hierarchy.leaves().length}</p>
        <p>{hierarchy_info}</p>
      </div>
    );
  })();
  return (
    <Dialog
      disabled={!catalog_id || !catalog_metadata}
      label="About This Catalog"
      buttonClassName={className}
    >
      {contents}
    </Dialog>
  );
}

function AddPlotButton({ className }: { className?: string }) {
  const add_plot = useAddPlot();
  return (
    <BigButton onClick={() => add_plot()} className={className}>
      Add Plot
    </BigButton>
  );
}

function Plots() {
  const plot_ids: PlotID[] = usePlotIDs();
  const plot_components = plot_ids.map((plot_id, index) => {
    return (
      <React.Fragment key={plot_id}>
        {index === 0 ? null : <Separator />}
        <div>
          <PlotSection id={plot_id} />
        </div>
      </React.Fragment>
    );
  });
  return <>{plot_components}</>;
}

function AddFilterSelect({ className }: { className: string }) {
  const catalog_cell_id = useCatalogCellID();
  const catalog_id = useCatalogID();
  const catalog_metadata = useCatalogMetadata();
  const leaves = catalog_metadata?.hierarchy?.leaves() ?? [];
  const filter_names = useFilterNames();
  const merge_state = useMergeState();
  const [value, set_value] = React.useState(undefined);
  return (
    <Combobox
      disabled={!catalog_id || !catalog_metadata}
      placeholder="Add a filter..."
      options={leaves}
      value={value}
      getKey={(node: CatalogHierarchyNode) =>
        catalog_metadata.get_hash_from_node(node)
      }
      getDisplayName={(d: CatalogHierarchyNode) => (
        <FieldTitles titles={get_field_titles(d)} />
      )}
      getDisabled={(d: CatalogHierarchyNode) => filter_names.has(d.data.name)}
      onValueChange={(d: CatalogHierarchyNode) => {
        const field_id = d.data.name;
        merge_state({
          add_filter: {
            [catalog_cell_id]: {
              [catalog_id]: {
                [field_id]: true
              }
            }
          }
        });
        set_value(undefined);
      }}
      debug
      triggerClassName={className}
    />
  );
}

function RandomSampleControls() {
  const set_random_config = useSetRandomConfig();
  const random_config = useRandomConfig();
  const [seed, set_seed] = React.useState(random_config?.seed ?? 0);
  const debounced_seed = useDebounce(seed, 500);
  React.useEffect(() => {
    set_random_config(`seed`, debounced_seed);
  }, [debounced_seed]);

  return (
    <>
      <div className="space-y-4 rounded-md bg-black/5 p-4 dark:bg-white/20">
        <div>Sample</div>
        <SliderWithText
          min={1e-9}
          max={1}
          value={random_config?.sample ?? 1}
          debounce={500}
          onValueChange={(new_value) => set_random_config(`sample`, new_value)}
        />
      </div>
      <div className="space-y-4 rounded-md bg-black/5 p-4 dark:bg-white/20">
        <div>Seed</div>
        <NumberInput
          value={seed}
          min={"0"}
          max={`18446744073709552000`}
          onNumberInput={(new_value) => set_seed(new_value)}
        />
      </div>
    </>
  );
}
