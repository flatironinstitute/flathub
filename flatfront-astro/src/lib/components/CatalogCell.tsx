import type { TopResponseEntry, TopResponse, CellID } from "../types";

import React from "react";
import * as d3 from "d3";
import { useQuery } from "@tanstack/react-query";
import * as controller from "../app-state";
import { fetch_api_get, assert_catalog_cell_id } from "../shared";
import {
  BigButton,
  CellSection,
  CellWrapper,
  Placeholder,
  Dialog,
  Select
} from "./Primitives";
import TableSection from "./Table";
import PlotSection from "./Plot";
import FieldCard from "./FieldCard";
import CatalogMetadataProvider, { useCatalogMetadata } from "./CatalogMetadata";
import { useFilters } from "../filters";

const CatalogCellIDContext = React.createContext<CellID.Catalog | undefined>(
  undefined
);

export function useCatalogCellID() {
  const catalog_cell_id = React.useContext(CatalogCellIDContext);
  if (catalog_cell_id === null) {
    throw new Error(`useCatalogCellID: value is null`);
  }
  assert_catalog_cell_id(catalog_cell_id);
  return catalog_cell_id;
}

export function useCatalogID(): string {
  const catalog_cell_id = useCatalogCellID();
  const catalog_id = controller.useAppState()?.set_catalog?.[catalog_cell_id];
  return catalog_id;
}

export default function CatalogCell({
  id: catalog_cell_id
}: {
  id: CellID.Catalog;
}) {
  assert_catalog_cell_id(catalog_cell_id);
  return (
    <CatalogCellIDContext.Provider value={catalog_cell_id}>
      <CatalogMetadataProvider>
        <CatalogCellContents />
      </CatalogMetadataProvider>
    </CatalogCellIDContext.Provider>
  );
}

function CatalogCellContents() {
  const catalog_cell_id = useCatalogCellID();

  const catalog_id = useCatalogID();

  const top_sections = (
    <>
      <CellSection label="catalog" className="flex flex-col gap-y-4">
        <CatalogSelect />
        <BigButton disabled={!catalog_id}>About</BigButton>
        <BrowseFieldsDialog />
      </CellSection>
      <CellSection label="filters" className="flex flex-col gap-y-4">
        <BigButton disabled={!catalog_id} className="desktop:hidden">
          Edit Filters
        </BigButton>
        <FilterControls />
      </CellSection>
      <CellSection label="random sample" className="flex flex-col gap-y-4">
        <div>fraction</div>
        <div>seed</div>
      </CellSection>
    </>
  );

  const plot_ids = d3.reverse(
    controller.useAppState()?.add_plot?.[catalog_cell_id] ?? []
  );

  const plot_components = plot_ids.map((plot_id) => {
    return <PlotSection key={plot_id} id={plot_id} />;
  });

  return (
    <CellWrapper className="grid gap-x-8 gap-y-4 desktop:grid-cols-6">
      <div className="space-y-4 desktop:col-span-2 desktop:col-start-1">
        {top_sections}
      </div>
      <CellSection className="flex flex-col gap-y-20 desktop:col-span-4 desktop:col-start-3 desktop:row-start-1">
        <AddPlotButton />
        {plot_components}
        <TableSection />
        <CellSection label="python" className="space-y-4">
          <Placeholder>TODO: Python</Placeholder>
        </CellSection>
        <CellSection label="download" className="space-y-4">
          <Placeholder>TODO: Download Data</Placeholder>
        </CellSection>
      </CellSection>
    </CellWrapper>
  );
}

function AddPlotButton() {
  const catalog_cell_id = useCatalogCellID();
  const number_of_plots =
    controller.useAppState()?.add_plot?.[catalog_cell_id]?.length ?? 0;
  const dispatch = controller.useDispatch();
  return (
    <BigButton
      onClick={() => {
        dispatch(
          [`add_plot`, catalog_cell_id, number_of_plots],
          `plot_${number_of_plots}`
        );
      }}
    >
      Add Plot
    </BigButton>
  );
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
  assert_catalog_cell_id(catalog_cell_id);

  const catalog_id = useCatalogID();

  const catalog_metadata = useCatalogMetadata();

  const all_field_nodes = catalog_metadata?.depth_first ?? [];

  return (
    <Dialog disabled={!catalog_id} label="Browse Fields">
      <div className="h-10 border-b border-b-black/20">
        some controls go here
      </div>
      <div className="max-h-[80dvh] w-[80dvw] max-w-[600px] space-y-3 overflow-x-visible overflow-y-scroll p-4 text-xs">
        {all_field_nodes.map((node) => (
          <FieldCard fieldNode={node} key={node.data.__hash} />
        ))}
      </div>
    </Dialog>
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
    <div className="space-y-3 text-xs">
      {filter_and_ancestor_nodes.map((node) => (
        <FieldCard filter fieldNode={node} key={node.data.__hash} />
      ))}
      {debug}
    </div>
  );
}
