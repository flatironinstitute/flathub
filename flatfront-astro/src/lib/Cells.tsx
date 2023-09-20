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
import {
  BigButton,
  CellSection,
  CellWrapper,
  dispatch_action,
  fetch_api_get,
  hooks,
  assert_catalog_cell_id,
  log,
  Placeholder,
  Providers
} from "./shared";
import * as stores from "./stores";
import Select from "./Select";
import TableSection from "./Table";
import PlotSection from "./Plot";
import FilterControls from "./FilterControls";

export default function Cells() {
  const cells = hooks.useStore(stores.cells);

  const is_parent_cell = (
    cell: Cell.Any
  ): cell is Cell.Catalog | Cell.Comparison => {
    return [`catalog`, `comparison`].includes(cell.type);
  };

  return (
    <>
      {cells.filter(is_parent_cell).map((cell) => {
        const component = (() => {
          switch (cell.type) {
            case `catalog`:
              return <CatalogCell />;
            case `comparison`:
              return <ComparisonCell />;
            default:
              cell satisfies never;
          }
        })();
        return (
          <Providers.CellProvider key={cell.cell_id} value={cell}>
            {component}
          </Providers.CellProvider>
        );
      })}
    </>
  );
}

function CatalogCell() {
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
        <BigButton disabled={!catalog_id}>Browse Fields</BigButton>
      </CellSection>
      <CellSection label="filters" className="flex flex-col gap-y-4">
        <BigButton disabled={!catalog_id}>Edit Filters</BigButton>
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

function ComparisonCell() {
  return <div>comparison</div>;
}
