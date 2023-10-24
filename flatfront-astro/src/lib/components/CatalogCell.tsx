import type {
  CatalogHierarchyNode,
  CellID,
  CountRequestBody,
  CountResponse,
  PlotID,
  TopResponse,
  TopResponseEntry
} from "../types";

import React from "react";
import * as d3 from "d3";
import { useQuery } from "@tanstack/react-query";
import * as controller from "../app-state";
import {
  fetch_api_get,
  fetch_api_post,
  format,
  get_field_titles,
  log
} from "../shared";
import { useAddPlot, usePlotIDs } from "../plot-hooks";
import { useFilters } from "./FiltersContext";
import {
  BigButton,
  CellWrapper,
  Combobox,
  Dialog,
  FieldTitles,
  Heading,
  Placeholder,
  Select,
  Separator,
  SimpleLabel
} from "./Primitives";
import TableSection from "./Table";
import PlotSection from "./Plot";
import FieldCard from "./FieldCard";
import {
  CatalogProvider,
  useCatalogCellID,
  useCatalogID,
  useMatchingRows
} from "./CatalogContext";
import BrowseFieldsDialog from "./BrowseFieldsDialog";
import Katex from "./Katex";
import { useCatalogMetadata } from "./CatalogMetadataContext";

export default function CatalogCell({
  id: catalog_cell_id
}: {
  id: CellID.Catalog;
}) {
  return (
    <CatalogProvider value={catalog_cell_id}>
      <CatalogCellContents />
    </CatalogProvider>
  );
}

function CatalogCellContents() {
  return (
    <CellWrapper className="grid gap-x-8 gap-y-4 desktop:grid-cols-6">
      <div className="space-y-4 desktop:col-span-2 desktop:col-start-1">
        <Heading>Catalog</Heading>
        <CatalogSelect />
        <AboutThisCatalog />
        <BrowseFieldsDialog />
        <Heading>Filters</Heading>
        <AddFilterSelect />
        <FilterControls />
        <Heading>Random Sample</Heading>
        <div>fraction</div>
        <div>seed</div>
      </div>
      <div className="flex flex-col gap-y-4 desktop:col-span-4 desktop:col-start-3 desktop:row-start-1">
        <Heading>Results</Heading>
        <MatchingRows />
        <AddPlotButton />
        <Plots />
        <div className="rounded-md p-4 ring-1 ring-black/20">
          <TableSection />
        </div>
        <div className="space-y-4 rounded-md p-4 ring-1 ring-black/20">
          <SimpleLabel>python</SimpleLabel>
          <Placeholder>TODO: Python</Placeholder>
        </div>
        <div className="space-y-4 rounded-md p-4 ring-1 ring-black/20">
          <SimpleLabel>download</SimpleLabel>
          <Placeholder>TODO: Download Data</Placeholder>
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

function MatchingRows() {
  const catalog_metadata = useCatalogMetadata();
  const total_rows = catalog_metadata?.response?.count;
  const matching = useMatchingRows();
  const r = Number.isFinite(matching)
    ? format.commas(matching)
    : `[Loading...]`;
  const t = total_rows ? format.commas(total_rows) : `[Loading...]`;
  const text = `Filtered to ${r} out of ${t} total rows`;
  return <div>{text}</div>;
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
        className="space-y-4 rounded-md p-4 ring-1 ring-black/20 dark:ring-white/30"
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
  const filters = useFilters();
  const dispatch = controller.useDispatch();
  const [value, set_value] = React.useState(undefined);
  return (
    <Combobox
      placeholder="Add a filter..."
      options={leaves}
      value={value}
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
        set_value(undefined);
      }}
      debug
    />
  );
}

function FilterControls() {
  const catalog_metadata = useCatalogMetadata();
  const filters = useFilters();
  // const all_field_nodes = catalog_metadata?.depth_first ?? [];
  const leaves = catalog_metadata?.hierarchy?.leaves() ?? [];
  const filter_and_ancestor_nodes = leaves.filter((node) => {
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
      {filter_and_ancestor_nodes.map((node, index) => (
        <React.Fragment key={node.data.__hash}>
          {index === 0 ? null : <Separator></Separator>}
          <FieldCard fieldNode={node} />
        </React.Fragment>
      ))}
    </div>
  );
}
