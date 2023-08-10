import type {
  CatalogHierarchyNode,
  TopResponseEntry,
  CatalogCellID,
  FilterCellID,
  TableCellID,
  RootCellID,
} from "./types";

import React from "react";

import clsx from "clsx";

import { Switch } from "@headlessui/react";
import * as Dialog from "@radix-ui/react-dialog";
import { TrashIcon } from "@radix-ui/react-icons";
import * as Slider from "@radix-ui/react-slider";
import * as d3 from "d3";
import { Cross1Icon } from "@radix-ui/react-icons";

import {
  BigButton,
  CellWrapper,
  dispatch_action,
  hooks,
  is_catalog_cell_id,
  log,
  Providers,
  set_filter_value,
  filter_hierarchy,
  get_node_path,
} from "./shared";
import * as stores from "./stores";
import Table from "./Table";
import Katex from "./Katex";
import { Scatterplot } from "./Highcharts";

export default function Cells() {
  const cells = hooks.useStore(stores.cells_depth_first).map((d) => d.data);

  log(`Cells: cells`, cells);
  return (
    <>
      {cells.map((cell) => {
        if (cell.type === `root`) return null;
        const component = (() => {
          if (cell.type === `catalog`) {
            return <CatalogCell />;
          } else if (cell.type === `filter`) {
            return <FilterCell />;
          } else {
            return <TableCell />;
          }
        })();
        return (
          <Providers.CellIDProvider key={cell.cell_id} value={cell.cell_id}>
            <Providers.ParentCellIDProvider value={cell.parent_cell_id}>
              {component}
            </Providers.ParentCellIDProvider>
          </Providers.CellIDProvider>
        );
      })}
    </>
  );
}

function CatalogCell() {
  const cell_id = hooks.useCellID();

  const catalog_name = hooks.useStore(stores.catalog_name).get(cell_id);
  const catalog_metadata = hooks
    .useStore(stores.catalog_metadata)
    .get(catalog_name);
  const catalog_title = catalog_metadata?.metadata?.title;

  return (
    <CellWrapper>
      <CellTitle subtitle={cell_id}>{catalog_title} Catalog</CellTitle>
      <CellSection label="actions">
        <BigButton
          onClick={() => {
            dispatch_action({
              type: `add_filter_cell`,
              cell_id: `filter_cell_${Date.now()}`,
              parent_cell_id: cell_id as CatalogCellID,
            });
          }}
        >
          Add Filters
        </BigButton>
      </CellSection>
    </CellWrapper>
  );
}

function FilterCell() {
  const cell_id = hooks.useCellID() as FilterCellID;
  const parent_cell_id: CatalogCellID = is_catalog_cell_id(
    hooks.useParentCellID()
  );
  const catalog_name = hooks.useStore(stores.catalog_name).get(cell_id);
  const catalog_metadata = hooks
    .useStore(stores.catalog_metadata)
    .get(catalog_name);
  const catalog_title = catalog_metadata?.metadata?.title;

  const filters = hooks.useStore(stores.filter_values)?.get(cell_id);

  log(`FilterCell:`, { cell_id, filters });
  return (
    <Providers.CellFiltersProvider value={filters}>
      <Providers.CatalogMetadataProvider value={catalog_metadata}>
        <Providers.CatalogNameProvider value={catalog_name}>
          <CellWrapper>
            <CellTitle subtitle={cell_id}>{catalog_title} Filters</CellTitle>
            <CellSection label="source" small>
              {parent_cell_id}
            </CellSection>
            <CellSection label="filters">
              {catalog_metadata ? (
                <CellFiltersSection />
              ) : (
                <PendingBox>Loading Catalog Metadata...</PendingBox>
              )}
            </CellSection>
            <CellSection label="actions">
              <div className="flex flex-col gap-y-4">
                <BigButton
                  onClick={() => {
                    dispatch_action({
                      type: `add_table_cell`,
                      cell_id: `table_cell_${Date.now()}`,
                      parent_cell_id: cell_id,
                    });
                  }}
                >
                  Add Table
                </BigButton>
                <BigButton
                  onClick={() => {
                    dispatch_action({
                      type: `remove_filter_cell`,
                      cell_id: cell_id,
                    });
                  }}
                >
                  Remove
                </BigButton>
              </div>
            </CellSection>
          </CellWrapper>
        </Providers.CatalogNameProvider>
      </Providers.CatalogMetadataProvider>
    </Providers.CellFiltersProvider>
  );
}

function CellFiltersSection() {
  const filters = hooks.useCellFilters();
  const catalog_metadata = hooks.useCatalogMetadata();
  const catalog_field_hierarchy = catalog_metadata.hierarchy;

  const filter_names = Object.keys(filters);

  const filter_nodes = filter_names.map((filter_name) =>
    catalog_field_hierarchy.find((d) => d.data.name === filter_name)
  );

  const node_names_set = new Set(
    filter_nodes
      .map((node) => node.ancestors())
      .flat()
      .map((d) => d.data.name ?? `root`)
  );

  const all_nodes = catalog_metadata?.nodes ?? [];

  const filtered_nodes = all_nodes.filter((node) => {
    return node_names_set.has(node.data.name ?? `root`);
  });

  const filtered_names = filtered_nodes.map((node) => node.data.name);

  return (
    <>
      <FieldsDialog />
      <div className="h-4" />
      <div className="grid grid-cols-1 gap-x-3 gap-y-2">
        {filtered_names.map((filter_name) => (
          <Providers.FieldNameProvider key={filter_name} value={filter_name}>
            <FieldCard filterMode />
          </Providers.FieldNameProvider>
        ))}
      </div>
    </>
  );
}

function TableCell() {
  const cell_id = hooks.useCellID() as TableCellID;
  const parent_cell_id = hooks.useParentCellID();
  const catalog_name = hooks.useStore(stores.catalog_name).get(cell_id);
  const catalog_metadata = hooks
    .useStore(stores.catalog_metadata)
    .get(catalog_name);
  const catalog_title = catalog_metadata?.metadata?.title;

  // const query_config = hooks.useStore(stores.query_config).get(cell_id);

  const data_query = hooks.useStore(stores.data_queries)?.[cell_id];

  const fetching = data_query.isFetching;
  const data = data_query?.data ?? null;

  const has_data = data?.length && data?.length > 0;

  const is_ready = data_query?.data && data_query?.data?.length > 0;

  return (
    <CellWrapper>
      <CellTitle subtitle={cell_id}>{catalog_title} Table</CellTitle>
      <CellSection label="source" small>
        {parent_cell_id}
      </CellSection>
      <CellSection label="query">
        {/* <pre>{JSON.stringify(query_config, null, 2)}</pre> */}
      </CellSection>
      <CellSection label="table">
        {is_ready ? (
          <Table
            data={data}
            catalog_field_hierarchy={catalog_metadata.hierarchy}
          />
        ) : (
          <PendingBox>No Data Loaded</PendingBox>
        )}
      </CellSection>
      <CellSection label="actions">
        <BigButton onClick={() => data_query.refetch()}>
          {fetching ? `Fetching Data...` : `Fetch Data`}
        </BigButton>
        <div>status: {data_query.status}</div>
        <div>fetch status: {data_query.fetchStatus}</div>
        <BigButton
          onClick={() => {
            dispatch_action({
              type: `remove_table_cell`,
              cell_id: cell_id,
            });
          }}
        >
          Remove
        </BigButton>
      </CellSection>
    </CellWrapper>
  );
}

function FieldsDialog() {
  const catalog_metadata = hooks.useCatalogMetadata();
  const fields_list = catalog_metadata?.nodes ?? [];
  const field_cards = fields_list
    .filter((d) => "name" in d.data)
    .map((field) => {
      return (
        <Providers.FieldNameProvider
          value={field.data.name}
          key={field.data.name}
        >
          <FieldCard></FieldCard>
        </Providers.FieldNameProvider>
      );
    });
  return (
    <Dialog.Root>
      <Dialog.Trigger className={BigButton.className}>
        Show All Fields
      </Dialog.Trigger>
      <Dialog.Portal>
        <Dialog.Overlay className="fixed inset-0 z-20 bg-black/50" />
        <Dialog.Content
          className={[
            `fixed z-50 w-[95vw] max-w-3xl rounded-lg p-4`,
            `top-[50%] left-[50%] -translate-x-[50%] -translate-y-[50%]`,
            `bg-white dark:bg-slate-700`,
            `focus:outline-none focus-visible:ring focus-visible:ring-purple-500 focus-visible:ring-opacity-75`,
          ].join(" ")}
        >
          <Dialog.Title className="text-sm font-medium text-light-text dark:text-dark-text">
            All Fields
          </Dialog.Title>
          <div className="h-4" />
          <Dialog.Description />
          <div className="h-4" />
          <input
            className="w-full dark:bg-slate-900 rounded-lg text-lg leading-5 py-2 px-3 focus:ring-2 focus:ring-slate-50 focus:outline-none"
            type="text"
            placeholder="search"
          />
          <div className="h-4" />
          <div className="h-[600px] overflow-y-scroll overflow-x-visible grid grid-cols-1 gap-4">
            {field_cards}
          </div>
          <Dialog.Close
            className={[
              "absolute top-3.5 right-3.5 inline-flex items-center justify-center rounded-full p-1",
              "focus:outline-none focus-visible:ring focus-visible:ring-purple-500 focus-visible:ring-opacity-75",
            ].join(" ")}
          >
            <Cross1Icon className="h-4 w-4 text-slate-500 hover:text-slate-700 dark:text-slate-500 dark:hover:text-slate-400" />
          </Dialog.Close>
        </Dialog.Content>
      </Dialog.Portal>
    </Dialog.Root>
  );
}

function FieldCard({
  filterMode = false,
}: {
  filterMode?: boolean;
}): React.JSX.Element {
  const [expanded, setExpanded] = React.useState(!filterMode);
  const cell_id = hooks.useCellID();
  const field_name = hooks.useFieldName();
  const filters = hooks.useCellFilters();
  const nodes_by_name = hooks.useCatalogMetadata()?.nodes_by_name;
  const field_node = nodes_by_name?.get(field_name);

  const node_depth = (field_node?.depth ?? 0) - 1;

  // const column_names = hooks.useStore(stores.column_names)?.get(cell_id);

  if (!field_node) {
    throw new Error(`Could not find node for ${field_name}`);
  }

  const is_leaf = field_node.children === undefined;
  const is_required = field_node.data.required;

  // const field_title = <FieldTitles node={field_node}></FieldTitles>;
  const field_title = <Katex>{field_node.data.title}</Katex>;

  const units = (() => {
    if (!expanded) return null;
    if (!field_node.data.units) return null;
    return (
      <div className="text-sm text-slate-500 dark:text-slate-300">
        <Katex>{field_node.data.units}</Katex>
      </div>
    );
  })();

  const field_description = (() => {
    if (!expanded) return null;
    if (!field_node.data.descr) return null;
    return (
      <div className="text-sm text-slate-500 dark:text-slate-300 overflow-hidden">
        <Katex>{field_node.data.descr}</Katex>
      </div>
    );
  })();

  const field_control = (() => {
    if (!filterMode) return null;
    const metadata = field_node?.data;
    // log(`field control`, metadata);
    if (!metadata) {
      throw new Error(`Could not find metadata for ${field_name}`);
    }
    if (metadata.sub && metadata.sub.length > 0) {
      return null;
    } else if (metadata.type === `float`) {
      return <ConnectedRangeSlider />;
    }
  })();

  const remove_filter_button = (() => {
    if (!filterMode) return null;
    if (is_required) return null;
    if (!is_leaf) return null;
    return (
      <TrashIcon
        className="w-5 h-5 cursor-pointer"
        onClick={() => {
          dispatch_action({
            type: `remove_filter`,
            cell_id,
            filter_name: field_name,
          });
        }}
      />
    );
  })();

  // const column_toggle = (() => {
  //   if (!is_leaf) return null;
  //   const is_active_column = column_names.has(field_name);
  //   const on_change = (checked: boolean) => {
  //     log(`column toggle`, checked);
  //     if (checked) {
  //       dispatch_action({
  //         type: `add_column`,
  //         cell_id,
  //         column_name: field_name,
  //       });
  //     } else {
  //       dispatch_action({
  //         type: `remove_column`,
  //         cell_id,
  //         column_name: field_name,
  //       });
  //     }
  //   };
  //   return (
  //     <Switch
  //       checked={is_active_column}
  //       onChange={on_change}
  //       disabled={is_required}
  //       className="disabled:opacity-50 disabled:cursor-not-allowed"
  //     >
  //       {({ checked }) => (
  //         <div className="flex gap-x-2 items-center">
  //           <div
  //             className={`h-3 w-3 outline outline-2 dark:outline-slate-50 rounded-xl ${
  //               checked ? `bg-slate-50` : `bg-transparent`
  //             }`}
  //           ></div>
  //           <div>Column</div>
  //         </div>
  //       )}
  //     </Switch>
  //   );
  // })();

  const filter_toggle = (() => {
    if (!is_leaf) return null;
    const is_active_filter = field_name in filters;
    const on_click = () => {
      log(`filter toggle`, is_active_filter);
      if (is_active_filter) {
        dispatch_action({
          type: `remove_filter`,
          cell_id,
          filter_name: field_name,
        });
      } else {
        dispatch_action({
          type: `add_filter`,
          cell_id,
          filter_name: field_name,
        });
      }
    };
    return (
      <button
        disabled={is_required}
        className="underline decoration-dashed disabled:opacity-50 disabled:cursor-not-allowed"
        onClick={() => on_click()}
      >
        {is_active_filter ? `Remove Filter` : `Add Filter`}
      </button>
    );
  })();

  const controls = filterMode ? (
    <>
      <div className="col-span-6">{field_control}</div>
      <div className="col-start-12 justify-self-end">
        {remove_filter_button}
      </div>
    </>
  ) : (
    <>
      {/* <div className="col-start-9 col-span-2 justify-self-center">
        {column_toggle}
      </div> */}
      <div className="col-start-11 col-span-2 justify-self-end">
        {filter_toggle}
      </div>
    </>
  );

  return (
    <div
      data-type="FieldCard"
      className={clsx(
        `flex flex-col gap-y-4 rounded-md text-md px-4`,
        expanded ? `py-4` : `py-1`,
        `bg-light-3 dark:bg-dark-3 text-light-text dark:text-dark-text`
      )}
      style={{
        marginLeft: `${node_depth * 2}ch`,
      }}
    >
      <div className="grid grid-cols-12 items-center ">
        <div className="col-span-5">{field_title}</div>
        {controls}
      </div>
      {units}
      {field_description}
      {expanded && <div className="h-1" />}
    </div>
  );
}

function ConnectedRangeSlider() {
  const cell_id = hooks.useCellID();
  const catalog_name = hooks.useCatalogName();
  const field_name = hooks.useFieldName();
  const metadata = hooks
    .useStore(stores.field_metadata)
    ?.get(catalog_name, field_name)?.data;
  const min = metadata.stats?.min;
  const max = metadata.stats?.max;
  if (
    !Number.isFinite(min) ||
    !Number.isFinite(max) ||
    min === null ||
    max === null ||
    min === undefined ||
    max === undefined
  ) {
    log(`Error meta:`, metadata);
    throw new Error(
      `Could not find min/max stats for ${field_name} of type ${metadata.type}`
    );
  }
  const filters = hooks.useCellFilters();
  const filter_state = filters[field_name];
  if (!(typeof filter_state === `object`)) {
    throw new Error(`Expected filter state to be an object for ${field_name}`);
  }
  if (!(`gte` in filter_state) || !(`lte` in filter_state)) {
    log(filters, filter_state);
    throw new Error(`Expected filter state to have gte/lte for ${field_name}`);
  }
  const low = filter_state.gte;
  const high = filter_state.lte;
  if (typeof low !== `number` || typeof high !== `number`) {
    throw new Error(
      `Expected filter state to have gte/lte numbers for ${field_name}`
    );
  }
  const value = [low, high] as [number, number];
  return (
    <RangeSlider
      min={min}
      max={max}
      value={value}
      onValueChange={([low, high]) => {
        // log(`on value change`, low, high);
        set_filter_value(cell_id, field_name, {
          gte: low,
          lte: high,
        });
      }}
    />
  );
}

function RangeSlider({
  min,
  max,
  value,
  onValueChange,
}: {
  min: number;
  max: number;
  value: [number, number];
  onValueChange: (value: [number, number]) => void;
}) {
  const [low, high] = value;
  const format = (d: number) => d3.format(`,.4~g`)(d);
  const range = Math.abs(max - min);
  const step = range / 100;
  const thumb_class = `block h-4 w-4 rounded-full bg-slate-700 dark:bg-slate-50 focus:outline-none focus-visible:ring focus-visible:ring-purple-500 focus-visible:ring-opacity-75`;
  return (
    <div
      className="grid gap-x-2 items-center justify-items-center"
      style={{ gridTemplateColumns: `10ch 1fr 10ch` }}
    >
      <div>{format(low)}</div>
      <Slider.Root
        min={min}
        max={max}
        value={value}
        className="cursor-pointer relative flex h-5 w-full touch-none items-center"
        onValueChange={onValueChange}
        step={step}
      >
        <Slider.Track className="relative h-1 w-full grow rounded-full bg-slate-400 dark:bg-slate-800">
          <Slider.Range className="absolute h-full rounded-full bg-slate-600 dark:bg-white" />
        </Slider.Track>
        <Slider.Thumb className={thumb_class} />
        <Slider.Thumb className={thumb_class} />
      </Slider.Root>
      <div>{format(high)}</div>
    </div>
  );
}

function CellColumnsSection({ column_names }: { column_names: Set<string> }) {
  const cell_id = hooks.useCellID();

  const names_order =
    hooks.useCatalogMetadata()?.nodes.map((d) => d.data.name) ?? [];

  const column_names_raw = Array.from(column_names);

  const sorted = d3.sort(column_names_raw, (d) => names_order.indexOf(d));
  return (
    <div className="grid grid-cols-3 gap-x-3 gap-y-2">
      {sorted.map((column_name) => {
        return (
          <div
            key={column_name}
            className="flex items-center justify-between text-xs text-slate-700 dark:text-slate-200 px-2 py-1 rounded bg-slate-50 dark:bg-slate-600"
          >
            <div>{column_name}</div>
            <TrashIcon
              className="w-3 h-3 cursor-pointer"
              onClick={() => {
                dispatch_action({
                  type: `remove_column`,
                  cell_id,
                  column_name: column_name,
                });
              }}
            />
          </div>
        );
      })}
    </div>
  );
}

function PendingBox({ children }: { children: React.ReactNode }) {
  return (
    <div className="h-40 rounded-lg p-4 outline-2 outline-dashed outline-slate-700 dark:outline-slate-50 grid place-items-center opacity-50">
      {children}
    </div>
  );
}

function CellTitle({
  children,
  subtitle,
}: {
  children: React.ReactNode;
  subtitle?: React.ReactNode;
}): React.JSX.Element {
  return (
    <div>
      <div className="text-2xl font-bold">{children}</div>
      {subtitle && <div className="text-slate-400">{subtitle}</div>}
    </div>
  );
}

function CellSection({
  label,
  children = null,
  small = false,
}: {
  label: string;
  children?: React.ReactNode;
  small?: boolean;
}): React.JSX.Element {
  return (
    <div className="flex flex-col">
      <SimpleLabel>{label}</SimpleLabel>
      <div className={small ? `h-1` : `h-4`}></div>
      {children}
    </div>
  );
}

function SimpleLabel({
  children,
}: {
  children: React.ReactNode;
}): React.JSX.Element {
  return (
    <div className="text-slate-400 dark:text-slate-400 uppercase text-sm">
      {children}
    </div>
  );
}
