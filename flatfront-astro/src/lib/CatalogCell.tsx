import type {
  Cell,
  TopResponseEntry,
  Action,
  TopResponse,
  CatalogResponse
} from "./types";
import React from "react";
import * as d3 from "d3";
import { useQuery } from "@tanstack/react-query";
import { Providers } from "./contexts";
import * as hooks from "./hooks";
import {
  dispatch_action,
  fetch_api_get,
  assert_catalog_cell_id
} from "./shared";
import {
  BigButton,
  CellSection,
  CellWrapper,
  Placeholder,
  Dialog,
  Select
} from "./Primitives";
import * as stores from "./stores";
import TableSection from "./Table";
import PlotSection from "./Plot";
import FieldCard from "./FieldCard";

export default function CatalogCell() {
  const catalog_cell_id = hooks.useCell().cell_id;
  assert_catalog_cell_id(catalog_cell_id);

  const catalog_id = hooks
    .useStore(stores.catalog_id_by_cell_id)
    .get(catalog_cell_id);

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

  const cells = hooks.useStore(stores.cells);

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

  const add_buttons = (
    <>
      <BigButton
        onClick={() => {
          dispatch_action({
            type: `add_plot_cell`,
            cell_id: `plot_cell_${Date.now()}`,
            catalog_cell_id
          });
        }}
      >
        Add Plot
      </BigButton>
    </>
  );

  const child_cells = cells
    .filter((cell: Cell.Any): cell is Cell.Plot => {
      return cell.type === `plot` && cell.catalog_cell_id === catalog_cell_id;
    })
    .map((cell) => {
      return (
        <Providers.CellProvider key={cell.cell_id} value={cell}>
          <PlotSection />
        </Providers.CellProvider>
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
        {child_cells}
        {bottom_sections}
      </CellSection>
    </CellWrapper>
  );
}

function CatalogSelect() {
  const cell_id = hooks.useCell().cell_id;
  assert_catalog_cell_id(cell_id);
  const catalog_id = hooks.useStore(stores.catalog_id_by_cell_id).get(cell_id);
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
          dispatch_action({
            type: `set_catalog`,
            cell_id,
            catalog_id: d?.name
          } as Action.SetCatalog);
        }}
      />
    </div>
  );
}

function BrowseFieldsDialog() {
  const catalog_cell_id = hooks.useCell().cell_id;
  assert_catalog_cell_id(catalog_cell_id);

  const catalog_id = hooks
    .useStore(stores.catalog_id_by_cell_id)
    .get(catalog_cell_id);

  const catalog_metadata = hooks.useStore(
    stores.catalog_metadata_by_catalog_id
  )?.[catalog_id];

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
  const catalog_cell_id = hooks.useCell().cell_id;
  assert_catalog_cell_id(catalog_cell_id);
  const catalog_id = hooks
    .useStore(stores.catalog_id_by_cell_id)
    .get(catalog_cell_id);
  const catalog_metadata = hooks.useStore(
    stores.catalog_metadata_by_catalog_id
  )?.[catalog_id];
  const filters = hooks
    .useStore(stores.filters_by_cell_id)
    .get(catalog_cell_id);
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
