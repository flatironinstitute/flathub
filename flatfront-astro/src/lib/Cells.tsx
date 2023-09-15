import type { Cell, TopResponseEntry } from "./types";

import * as d3 from "d3";
import {
  BigButton,
  CellSection,
  CellWrapper,
  dispatch_action,
  hooks,
  is_catalog_cell_id,
  log,
  Placeholder,
  Providers
} from "./shared";
import * as stores from "./stores";
import Select from "./Select";
import TableSection from "./Table";
import PlotSection from "./Plot";

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
  const catalog_cell_id = is_catalog_cell_id(hooks.useCell().cell_id);

  const cells = hooks.useStore(stores.cells);

  const top_sections = (
    <>
      <CellSection label="catalog" className="flex flex-col gap-y-4">
        <CatalogSelect />
        <BigButton>About</BigButton>
        <BigButton>Browse Fields</BigButton>
      </CellSection>
      <CellSection label="filters" className="flex flex-col gap-y-4">
        <BigButton>Edit Filters</BigButton>
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
  // const cell_id = hooks.useCell().cell_id;
  // const catalog_id = hooks.useStore(stores.catalog_id_by_cell_id).get(cell_id);
  const get_title = (d: TopResponseEntry) => d?.title;
  const catalog_list_unsorted = hooks.useStore(stores.top_response).data ?? [];
  const catalog_list = d3.sort(catalog_list_unsorted, get_title);
  const ready = catalog_list.length > 0;
  // const selected = catalog_list.find((d) => d.name === catalog_id);
  return (
    <div data-type="CatalogSelect">
      {/* <Select
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
          } as Actions[`SetCatalog`]);
        }}
      /> */}
      <Select
        placeholder={ready ? "Select a catalog..." : "Loading catalogs..."}
        options={catalog_list}
        getKey={(d) => d?.name}
        getDisplayName={get_title}
      />
    </div>
  );
}

function ComparisonCell() {
  return <div>comparison</div>;
}
