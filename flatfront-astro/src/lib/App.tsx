import type {
  CatalogHierarchyNode,
  TopResponseEntry,
  CatalogCellID,
  FilterCellID,
  TableCellID,
} from "./types";

import React from "react";

import { Switch } from "@headlessui/react";
import * as Dialog from "@radix-ui/react-dialog";
import { TrashIcon } from "@radix-ui/react-icons";
import * as Slider from "@radix-ui/react-slider";
import * as d3 from "d3";
import { Cross1Icon } from "@radix-ui/react-icons";

import {
  CatalogMetadataProvider,
  CatalogNameProvider,
  CellFiltersProvider,
  CellIDProvider,
  DataProvider,
  dispatch_action,
  FieldNameProvider,
  LabeledSelect,
  log,
  ParentCellIDProvider,
  PlotIDProvider,
  set_filter_value,
  useCatalogMetadata,
  useCatalogName,
  useCellFilters,
  useCellID,
  useFieldName,
  useParentCellID,
  useStore,
} from "./shared";
import * as stores from "./stores";
import Table from "./Table";
import Katex from "./Katex";
import { Scatterplot } from "./Highcharts";

export default function App() {
  return (
    <main
      className="ms-auto me-auto flex flex-col gap-y-10"
      style={{ width: `min(900px, 90vw)` }}
    >
      <div className="h-10" />
      <GlobalControls />
      <Cells />
      <div className="h-10" />
    </main>
  );
}

function GlobalControls(): React.JSX.Element {
  return (
    <CellWrapper>
      <div className="grid gap-y-4 items-center">
        <CatalogSelect />
        <DarkModeToggle />
      </div>
    </CellWrapper>
  );
}

function CatalogSelect() {
  const [selected, set_selected] = React.useState<TopResponseEntry | undefined>(
    undefined
  );
  const get_title = (d: TopResponseEntry) => d?.title;
  const catalog_list_unsorted = useStore(stores.top_response).data ?? [];
  const ready = catalog_list_unsorted.length > 0;
  const catalog_list = d3.sort(catalog_list_unsorted, get_title);
  log(`CatalogSelect: catalog_list`, catalog_list, ready);
  return (
    <div data-type="CatalogSelect">
      <LabeledSelect
        disabled={!ready}
        label="Add Catalog"
        placeholder={ready ? "Select a catalog..." : "Loading catalogs..."}
        options={catalog_list}
        getKey={(d) => d?.name}
        getDisplayName={get_title}
        value={selected}
        onValueChange={(d) => {
          set_selected(d);
        }}
        button
        buttonText="Add"
        onClick={() => {
          if (!selected) return;
          dispatch_action({
            type: `add_catalog_cell`,
            cell_id: `catalog_cell_${selected.name}`,
            catalog_name: selected.name,
          });
        }}
      />
    </div>
  );
}

function DarkModeToggle() {
  const [dark_mode, set_dark_mode] = React.useState(true);

  React.useEffect(() => {
    if (dark_mode) {
      document.documentElement.classList.add("dark");
    } else {
      document.documentElement.classList.remove("dark");
    }
  }, [dark_mode]);

  return (
    <div data-type="DarkModeToggle" className="flex items-center gap-x-4">
      <div>Dark Mode</div>
      <div>
        <Switch
          checked={dark_mode}
          onChange={set_dark_mode}
          className={`bg-slate-50 dark:bg-slate-900 relative inline-flex h-8 w-14 items-center rounded-full`}
        >
          <span
            className={`${
              dark_mode ? "translate-x-7" : "translate-x-1"
            } inline-block h-6 w-6 transform rounded-full bg-slate-500 dark:bg-slate-50 transition`}
          />
        </Switch>
      </div>
    </div>
  );
}

function Cells() {
  const cells = useStore(stores.cells_depth_first).map((d) => d.data);

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
            // throw new Error(`Unknown cell type: ${cell.type}`);
          }
        })();
        return (
          <CellIDProvider key={cell.cell_id} value={cell.cell_id}>
            <ParentCellIDProvider value={cell.parent_cell_id}>
              {component}
            </ParentCellIDProvider>
          </CellIDProvider>
        );
      })}
    </>
  );
}
function CatalogCell() {
  const cell_id = useCellID();

  const catalog_name = useStore(stores.catalog_name).get(cell_id);
  const catalog_metadata = useStore(stores.catalog_metadata).get(catalog_name);
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
  const cell_id = useCellID() as FilterCellID;
  const parent_cell_id = useParentCellID();
  const catalog_name = useStore(stores.catalog_name).get(cell_id);
  const catalog_metadata = useStore(stores.catalog_metadata).get(catalog_name);
  const catalog_title = catalog_metadata?.metadata?.title;
  return (
    <CellWrapper>
      <CellTitle subtitle={cell_id}>{catalog_title} Filters</CellTitle>
      <CellSection label="source" small>
        {parent_cell_id}
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
  );
}

function TableCell() {
  const cell_id = useCellID() as TableCellID;
  const parent_cell_id = useParentCellID();
  const catalog_name = useStore(stores.catalog_name).get(cell_id);
  const catalog_metadata = useStore(stores.catalog_metadata).get(catalog_name);
  const catalog_title = catalog_metadata?.metadata?.title;
  return (
    <CellWrapper>
      <CellTitle subtitle={cell_id}>{catalog_title} Table</CellTitle>
      <CellSection label="source" small>
        {parent_cell_id}
      </CellSection>
      <CellSection label="actions">
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

// function FilterCell() {
//   const cell_id = useCellID();

//   const catalog_name = useCatalogName();

//   const catalog_metadata_query = useStore(
//     stores.catalog_metadata_queries_by_catalog_name
//   )?.[catalog_name];

//   const catalog_metadata = catalog_metadata_query.data;

//   const catalog_title = catalog_metadata?.metadata?.title;

//   const filters = useStore(stores.fitler_values)?.get(cell_id);

//   return (
//     <CatalogMetadataProvider value={catalog_metadata}>
//       <CellFiltersProvider value={filters}>
//         <CellWrapper>
//           <CellTitle subtitle={cell_id}>{catalog_title} Filters</CellTitle>
//           <FieldsDialog />
//           <CellSection label="filters">
//             <CellFiltersSection />
//           </CellSection>
//           <CellSection label="actions">
//             <BigButton
//               onClick={() => {
//                 dispatch_action({
//                   type: `add_table_cell`,
//                   cell_id: `table_cell_${Date.now()}`,
//                   parent_cell_id: cell_id,
//                 });
//               }}
//             >
//               Add Table
//             </BigButton>
//           </CellSection>
//         </CellWrapper>
//       </CellFiltersProvider>
//     </CatalogMetadataProvider>
//   );
// }

function FieldsDialog() {
  const catalog_metadata = useCatalogMetadata();
  const fields_list = catalog_metadata?.nodes ?? [];
  const field_cards = fields_list
    .filter((d) => "name" in d.data)
    .map((field) => {
      return (
        <FieldNameProvider value={field.data.name} key={field.data.name}>
          <FieldCard></FieldCard>
        </FieldNameProvider>
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
          <Dialog.Title className="text-sm font-medium text-slate-900 dark:text-slate-100">
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

function CellFiltersSection() {
  const filters = useCellFilters();

  const filter_names = Object.keys(filters);

  const names_order = useCatalogMetadata()?.nodes.map((d) => d.data.name) ?? [];

  const sorted = d3.sort(filter_names, (d) => names_order.indexOf(d));

  return (
    <div className="grid grid-cols-1 gap-x-3 gap-y-2">
      {sorted.map((filter_name) => (
        <FieldNameProvider key={filter_name} value={filter_name}>
          <FieldCard filterMode />
        </FieldNameProvider>
      ))}
    </div>
  );
}

function FieldCard({
  filterMode = false,
}: {
  filterMode?: boolean;
}): React.JSX.Element {
  const [expanded, setExpanded] = React.useState(!filterMode);
  const cell_id = useCellID();
  const field_name = useFieldName();
  const filters = useCellFilters();
  const nodes_by_name = useCatalogMetadata()?.nodes_by_name;
  const field_node = nodes_by_name?.get(field_name);

  const column_names = useStore(stores.column_names)?.get(cell_id);

  if (!field_node) {
    throw new Error(`Could not find node for ${field_name}`);
  }

  const is_leaf = field_node.children === undefined;
  const is_required = field_node.data.required;

  const field_title = <FieldTitles node={field_node}></FieldTitles>;

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
    if (metadata.type === `float`) {
      return <ConnectedRangeSlider />;
    }
  })();

  const remove_filter_button = (() => {
    if (!filterMode) return null;
    if (is_required) return null;
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

  const column_toggle = (() => {
    if (!is_leaf) return null;
    const is_active_column = column_names.has(field_name);
    const on_change = (checked: boolean) => {
      log(`column toggle`, checked);
      if (checked) {
        dispatch_action({
          type: `add_column`,
          cell_id,
          column_name: field_name,
        });
      } else {
        dispatch_action({
          type: `remove_column`,
          cell_id,
          column_name: field_name,
        });
      }
    };
    return (
      <Switch
        checked={is_active_column}
        onChange={on_change}
        disabled={is_required}
        className="disabled:opacity-50 disabled:cursor-not-allowed"
      >
        {({ checked }) => (
          <div className="flex gap-x-2 items-center">
            <div
              className={`h-3 w-3 outline outline-2 dark:outline-slate-50 rounded-xl ${
                checked ? `bg-slate-50` : `bg-transparent`
              }`}
            ></div>
            <div>Column</div>
          </div>
        )}
      </Switch>
    );
  })();

  const filter_toggle = (() => {
    if (!is_leaf) return null;
    const is_active_filter = field_name in filters;
    const on_change = (checked: boolean) => {
      log(`filter toggle`, checked);
      if (checked) {
        dispatch_action({
          type: `add_filter`,
          cell_id,
          filter_name: field_name,
        });
      } else {
        dispatch_action({
          type: `remove_filter`,
          cell_id,
          filter_name: field_name,
        });
      }
    };
    return (
      <Switch
        checked={is_active_filter}
        onChange={on_change}
        disabled={is_required}
        className="disabled:opacity-50 disabled:cursor-not-allowed"
      >
        {({ checked }) => (
          <div className="flex gap-x-2 items-center">
            <div
              className={`h-3 w-3 outline outline-2 dark:outline-slate-50 rounded-xl ${
                checked ? `bg-slate-50` : `bg-transparent`
              }`}
            ></div>
            <div>Filter</div>
          </div>
        )}
      </Switch>
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
      <div className="col-start-9 col-span-2 justify-self-center">
        {column_toggle}
      </div>
      <div className="col-start-11 col-span-2 justify-self-center">
        {filter_toggle}
      </div>
    </>
  );

  return (
    <div
      data-type="FieldCard"
      className="rounded-md bg-slate-50 dark:bg-slate-600 text-md text-slate-700 dark:text-slate-200 px-4 py-1 flex flex-col gap-y-4"
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

function FieldTitles({
  node,
}: {
  node: CatalogHierarchyNode;
}): React.JSX.Element {
  const title_strings = get_field_titles(node);
  const titles = title_strings.map((title, i, arr) => {
    const is_last = i === arr.length - 1;
    return (
      <React.Fragment key={title}>
        {i > 0 ? <span>:</span> : ``}
        <Katex className={is_last ? `opacity-100` : `opacity-40`}>
          {title}
        </Katex>
      </React.Fragment>
    );
  });
  return (
    <div data-type="field-titles" className="text-md flex gap-x-2">
      {titles}
    </div>
  );
}

function get_field_titles<T extends { title?: string }>(
  node: d3.HierarchyNode<T>
): string[] {
  const titles: string[] = [];
  let current_node: d3.HierarchyNode<T> | null = node;
  while (current_node !== null) {
    if (current_node.data.title?.length ?? 0 > 0) {
      titles.push(current_node.data.title ?? `unknown`);
    }
    current_node = current_node.parent;
  }
  return titles.reverse();
}

function ConnectedRangeSlider() {
  const cell_id = useCellID();
  const catalog_name = useCatalogName();
  const field_name = useFieldName();
  const metadata = useStore(stores.field_metadata)?.get(
    catalog_name,
    field_name
  )?.data;
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
    throw new Error(
      `Could not find min/max stats for ${field_name} of type ${metadata.type}`
    );
  }
  const filters = useCellFilters();
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

function get_field_metadata(
  field_name: string,
  catalog_field_hierarchy?: CatalogHierarchyNode
): CatalogHierarchyNode | undefined {
  if (!catalog_field_hierarchy) {
    return undefined;
  }
  const field_metadata = catalog_field_hierarchy.find(
    (node) => node.data.name === field_name
  );
  if (!field_metadata) {
    throw new Error(`Could not find field metadata for ${field_name}`);
  }
  return field_metadata;
}

function CellColumnsSection({ column_names }: { column_names: Set<string> }) {
  const cell_id = useCellID();

  const names_order = useCatalogMetadata()?.nodes.map((d) => d.data.name) ?? [];

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

function BigButton({
  children,
  onClick,
}: {
  children: React.ReactNode;
  onClick?: () => void;
}): React.JSX.Element {
  return (
    <button className={BigButton.className} onClick={onClick}>
      {children}
    </button>
  );
}

BigButton.className =
  "bg-slate-500 dark:bg-slate-600 rounded-lg py-4 text-white font-bold text-xl";

function CellWrapper({
  children,
}: {
  children: React.ReactNode;
}): React.JSX.Element {
  return (
    <div className="rounded font-mono bg-light-1 dark:bg-dark-1 p-6 shadow-lg shadow-black dark:shadow-lg dark:shadow-black w-full transition-all flex flex-col gap-y-10">
      {children}
    </div>
  );
}
