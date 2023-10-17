import type {
  TopResponseEntry,
  TopResponse,
  CellID,
  CatalogHierarchyNode,
  PlotID
} from "../types";

import * as d3 from "d3";
import { useQuery } from "@tanstack/react-query";
import * as controller from "../app-state";
import { fetch_api_get, get_field_titles } from "../shared";
import {
  BigButton,
  CellSection,
  CellWrapper,
  Combobox,
  Placeholder,
  Dialog,
  Select,
  SimpleLabel
} from "./Primitives";
import TableSection from "./Table";
import PlotSection from "./Plot";
import FieldCard from "./FieldCard";
import CatalogMetadataProvider, { useCatalogMetadata } from "./CatalogMetadata";
import { useFilters } from "../filters";
import {
  CatalogCellIDContextProvider,
  useCatalogCellID,
  useCatalogID
} from "./CatalogContext";
import Katex from "./Katex";
import { useAddPlot, usePlotIDs } from "../plot-hooks";

export default function CatalogCell({
  id: catalog_cell_id
}: {
  id: CellID.Catalog;
}) {
  // assert_catalog_cell_id(catalog_cell_id);
  return (
    <CatalogCellIDContextProvider value={catalog_cell_id}>
      <CatalogMetadataProvider>
        <CatalogCellContents />
      </CatalogMetadataProvider>
    </CatalogCellIDContextProvider>
  );
}

function CatalogCellContents() {
  const catalog_id = useCatalogID();

  const top_sections = (
    <>
      <CellSection label="catalog" className="flex flex-col gap-y-4">
        <CatalogSelect />
        <BigButton disabled={!catalog_id}>About</BigButton>
        <BrowseFieldsDialog />
      </CellSection>
      <FilterSection />
      <CellSection label="random sample" className="flex flex-col gap-y-4">
        <div>fraction</div>
        <div>seed</div>
      </CellSection>
    </>
  );

  const plot_ids: PlotID[] = usePlotIDs();

  const plot_components = plot_ids.map((plot_id) => {
    return (
      <CellSection
        key={plot_id}
        className="space-y-4 rounded-md p-4 ring-1 ring-black/20 dark:ring-white/30"
      >
        <PlotSection id={plot_id} />
      </CellSection>
    );
  });

  return (
    <CellWrapper className="grid gap-x-8 gap-y-4 desktop:grid-cols-6">
      <div className="space-y-4 desktop:col-span-2 desktop:col-start-1">
        {top_sections}
      </div>
      <CellSection className="flex flex-col gap-y-4 desktop:col-span-4 desktop:col-start-3 desktop:row-start-1">
        <div className="flex flex-col gap-y-4">
          <SimpleLabel>plots</SimpleLabel>
          <AddPlotButton />
        </div>
        {plot_components}
        <CellSection
          label="table"
          className="space-y-4 rounded-md p-4 ring-1 ring-black/20"
        >
          <TableSection />
        </CellSection>
        <CellSection
          label="python"
          className="space-y-4 rounded-md p-4 ring-1 ring-black/20"
        >
          <Placeholder>TODO: Python</Placeholder>
        </CellSection>
        <CellSection
          label="download"
          className="space-y-4 rounded-md p-4 ring-1 ring-black/20"
        >
          <Placeholder>TODO: Download Data</Placeholder>
        </CellSection>
      </CellSection>
    </CellWrapper>
  );
}

function AddPlotButton() {
  const add_plot = useAddPlot();
  return <BigButton onClick={() => add_plot()}>Add Plot</BigButton>;
}

function CatalogSelect() {
  const catalog_cell_id = useCatalogCellID();
  const catalog_id = useCatalogID();
  const get_title = (d: TopResponseEntry) => d?.title;
  const catalog_list_query = useQuery({
    staleTime: Infinity,
    queryKey: ["top"],
    queryFn: async (): Promise<TopResponse> => fetch_api_get<TopResponse>(`/`)
  });
  const catalog_list_unsorted = catalog_list_query.data ?? [];
  const catalog_list = d3.sort(catalog_list_unsorted, get_title);
  const ready = catalog_list.length > 0;
  const selected = catalog_list.find((d) => d.name === catalog_id);
  const dispatch = controller.useDispatch();
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
          dispatch([`set_catalog`, catalog_cell_id], catalog_id);
        }}
      />
    </div>
  );
}

function BrowseFieldsDialog() {
  const catalog_cell_id = useCatalogCellID();
  // assert_catalog_cell_id(catalog_cell_id);

  const catalog_id = useCatalogID();

  const catalog_metadata = useCatalogMetadata();

  const all_field_nodes = catalog_metadata?.depth_first ?? [];

  return (
    <Dialog disabled={!catalog_id} label="Browse Fields">
      <div className="h-10 border-b border-b-black/20">
        some controls go here
      </div>
      <div className="max-h-[80dvh] w-[80dvw] max-w-[600px] space-y-3 overflow-x-visible overflow-y-scroll p-4">
        {all_field_nodes.map((node) => (
          <FieldCard fieldNode={node} key={node.data.__hash} />
        ))}
      </div>
    </Dialog>
  );
}

function FilterSection() {
  const catalog_cell_id = useCatalogCellID();
  const catalog_id = useCatalogID();

  const catalog_metadata = useCatalogMetadata();

  const leaves = catalog_metadata?.hierarchy?.leaves() ?? [];

  const filters = useFilters();

  const dispatch = controller.useDispatch();

  return (
    <CellSection label="filters" className="flex flex-col gap-y-4">
      <Combobox
        placeholder="Add a filter..."
        options={leaves}
        getKey={(d: CatalogHierarchyNode) => d.data.__hash}
        getDisplayName={(d: CatalogHierarchyNode) => {
          const titles = get_field_titles(d);
          return <FieldTitles titles={titles} />;
        }}
        getDisabled={(d: CatalogHierarchyNode) => {
          return d.data.name in filters;
        }}
        onValueChange={(d: CatalogHierarchyNode) => {
          const field_id = d.data.name;
          dispatch([`add_filter`, catalog_cell_id, catalog_id, field_id], true);
        }}
      />
      <BigButton disabled={!catalog_id} className="desktop:hidden">
        Edit Filters
      </BigButton>
      <FilterControls />
    </CellSection>
  );
}

function FieldTitles({ titles }: { titles: string[] }) {
  return (
    <div className="flex gap-x-2">
      {titles.map((title) => (
        <Katex key={title}>{title}</Katex>
      ))}
    </div>
  );
}

function FilterControls() {
  const catalog_metadata = useCatalogMetadata();
  const filters = useFilters();
  const all_field_nodes = catalog_metadata?.depth_first ?? [];
  const filter_and_ancestor_nodes = all_field_nodes.filter((node) => {
    // Exclude if is root node
    if (node.depth === 0) return false;
    // Include if this node is in the filters list
    if (node.data.name in filters) return true;
    // Include if this node is an ancestor of a node in the filter list
    if (node.leaves().some((leaf) => leaf.data.name in filters)) return true;
    // Exclude otherwise
    return false;
  });
  const debug = <pre>{JSON.stringify(filters, null, 2)}</pre>;
  return (
    <div className="space-y-3">
      {filter_and_ancestor_nodes.map((node) => (
        <FieldCard filter fieldNode={node} key={node.data.__hash} />
      ))}
      {debug}
    </div>
  );
}
