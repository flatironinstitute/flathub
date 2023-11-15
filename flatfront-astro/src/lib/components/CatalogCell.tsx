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
  SimpleLabel,
  SliderWithText
} from "./Primitives";
import TableSection from "./Table";
import PlotSection from "./PlotSection";
import BrowseFieldsDialog from "./BrowseFieldsDialog";
import Katex from "./Katex";
import FilterControls from "./FilterControls";

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
  return (
    <CellWrapper className="@container/cell">
      <div className="grid gap-x-8 gap-y-4 @2xl/cell:grid-cols-6">
        <div className="space-y-4 @2xl/cell:col-span-2 @2xl/cell:col-start-1">
          <Heading>Catalog</Heading>
          <CatalogSelect />
          <AboutThisCatalog />
          <BrowseFieldsDialog />
          <Heading>Filters</Heading>
          <AddFilterSelect />
          <FilterControls />
          <Heading>Random Sample</Heading>
          <RandomSampleControls />
        </div>
        <div className="flex flex-col gap-y-4 @2xl/cell:col-span-4 @2xl/cell:col-start-3 @2xl/cell:row-start-1">
          <Heading>Results</Heading>
          <div>{useMatchingRowsText()}</div>
          <AddPlotButton />
          <Plots />
          <div className="rounded-md p-4 ring-1 ring-black/20">
            <TableSection />
          </div>
          <div className="space-y-4 rounded-md p-4 ring-1 ring-black/20">
            <SimpleLabel>python</SimpleLabel>
            <Placeholder>TODO: Python</Placeholder>
          </div>
        </div>
      </div>
    </CellWrapper>
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
  return (
    <div data-type="CatalogSelect">
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
      />
    </div>
  );
}

function AboutThisCatalog() {
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
      <>
        <Heading>{title}</Heading>
        <Katex className="space-y-4 leading-[1.4] [&_a]:underline [&_ul]:list-disc [&_ul]:space-y-4 [&_ul]:pb-3 [&_ul]:ps-10">
          {descr}
        </Katex>
        <p>Rows: {format.commas(count)}</p>
        <p>Variables: {hierarchy.leaves().length}</p>
        <p>{hierarchy_info}</p>
      </>
    );
  })();
  return (
    <Dialog
      disabled={!catalog_id || !catalog_metadata}
      label="About This Catalog"
      className="flex max-h-[80dvh] w-[min(80dvw,800px)] flex-col gap-y-4 p-8"
    >
      {contents}
    </Dialog>
  );
}

function AddPlotButton() {
  const add_plot = useAddPlot();
  return <BigButton onClick={() => add_plot()}>Add Plot</BigButton>;
}

function Plots() {
  const plot_ids: PlotID[] = usePlotIDs();
  const plot_components = plot_ids.map((plot_id) => {
    return (
      <div
        key={plot_id}
        className="rounded-md p-4 ring-1 ring-black/20 dark:ring-white/30"
      >
        <PlotSection id={plot_id} />
      </div>
    );
  });
  return <>{plot_components}</>;
}

function AddFilterSelect() {
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
      getKey={(d: CatalogHierarchyNode) => catalog_metadata.hash_map.get(d)}
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
      <div>Sample</div>
      <SliderWithText
        min={1e-9}
        max={1}
        value={random_config?.sample ?? 1}
        debounce={500}
        onValueChange={(new_value) => set_random_config(`sample`, new_value)}
      />
      <div>Seed</div>
      <NumberInput
        value={seed}
        min={"0"}
        max={`18446744073709552000`}
        onNumberInput={(new_value) => set_seed(new_value)}
      />
    </>
  );
}
