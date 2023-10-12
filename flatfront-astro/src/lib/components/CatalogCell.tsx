import type {
  TopResponseEntry,
  Action,
  TopResponse,
  CatalogResponse,
  PlotID
} from "../types";

import React from "react";
import * as d3 from "d3";
import { useQuery } from "@tanstack/react-query";
import { Providers } from "../contexts";
import * as hooks from "../hooks";
import { fetch_api_get, assert_catalog_cell_id } from "../shared";
import * as stores from "../stores";
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
import * as controller from "./AppController";

export default function CatalogCell() {
  const catalog_cell_id = hooks.useCatalogCellID();
  assert_catalog_cell_id(catalog_cell_id);

  const catalog_id = hooks.useCatalogID();

  const catalog_query = useQuery({
    queryKey: [`catalog`, catalog_id],
    queryFn: (): Promise<CatalogResponse> => fetch_api_get(`/${catalog_id}`),
    enabled: !!catalog_id,
    staleTime: Infinity
  });

  React.useEffect(() => {
    if (!catalog_id) return;
    stores.catalog_query_by_catalog_id.update((old) => {
      return { ...old, [catalog_id]: catalog_query };
    });
  }, [catalog_query]);

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

  const number_of_plots =
    controller.useState()?.add_plot?.[catalog_cell_id]?.length ?? 0;

  const dispatch = controller.useDispatch();

  const add_buttons = (
    <>
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
    </>
  );

  const plot_ids = controller.useState()?.add_plot?.[catalog_cell_id] ?? [];

  const plot_components = plot_ids.map((plot_id) => {
    return (
      <Providers.PlotIDProvider key={plot_id} value={plot_id}>
        <PlotSection key={plot_id} />
      </Providers.PlotIDProvider>
    );
  });

  const bottom_sections = (
    <>
      <TableSection />
      <CellSection label="python" className="space-y-4">
        <Placeholder>TODO: Python</Placeholder>
      </CellSection>
      <CellSection label="download" className="space-y-4">
        <Placeholder>TODO: Download Data</Placeholder>
      </CellSection>
    </>
  );

  return (
    <CellWrapper className="grid gap-x-8 gap-y-4 desktop:grid-cols-6">
      <div className="space-y-4 desktop:col-span-2 desktop:col-start-1">
        {top_sections}
      </div>
      <CellSection className="flex flex-col gap-y-4 desktop:col-span-4 desktop:col-start-3 desktop:row-start-1">
        {add_buttons}
        {plot_components}
        {bottom_sections}
      </CellSection>
    </CellWrapper>
  );
}

function CatalogSelect() {
  const catalog_cell_id = hooks.useCatalogCellID();
  assert_catalog_cell_id(catalog_cell_id);
  const catalog_id = hooks.useCatalogID();
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
  const catalog_cell_id = hooks.useCatalogCellID();
  assert_catalog_cell_id(catalog_cell_id);

  const catalog_id = hooks.useCatalogID();

  const catalog_metadata = hooks.useCatalogMetadata();

  const all_field_nodes = catalog_metadata?.depth_first ?? [];

  return (
    <Dialog disabled={!catalog_id} label="Browse Fields">
      <div className="h-10 border-b border-b-black/20">
        some controls go here
      </div>
      <div className="max-h-[80dvh] w-[80dvw] max-w-[600px] space-y-3 overflow-x-visible overflow-y-scroll p-4 text-xs">
        {all_field_nodes.map((node) => (
          <Providers.FieldNodeProvider value={node} key={node.data.__hash}>
            <FieldCard />
          </Providers.FieldNodeProvider>
        ))}
      </div>
    </Dialog>
  );
}

function FilterControls() {
  const catalog_metadata = hooks.useCatalogMetadata();
  const filters = hooks.useFilters();
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
        <Providers.FieldNodeProvider value={node} key={node.data.__hash}>
          <FieldCard filter />
        </Providers.FieldNodeProvider>
      ))}
      {debug}
    </div>
  );
}
