import type { Readable } from "svelte/store";
import type {
  QueryObserverResult,
  QueryObserverOptions,
} from "@tanstack/query-core";
import type {
  Action,
  Cell,
  CellID,
  CatalogCellID,
  FilterCellID,
  TableCellID,
  PlotCellID,
  CatalogMetadataWrapper,
  Filters,
  Datum,
  FilterValueRaw,
  FieldMetadata,
  CatalogResponse,
  ColumnListAction,
  FilterListAction,
} from "./types";

import React from "react";
import * as d3 from "d3";
import clsx from "clsx";
import { get } from "svelte/store";
import { ChevronDownIcon } from "@radix-ui/react-icons";
import { produce } from "immer";
import { Listbox } from "@headlessui/react";
import { QueryClient, QueryObserver } from "@tanstack/query-core";
import * as stores from "./stores";

const FLATHUB_API_BASE_URL = `https://flathub.flatironinstitute.org`;

const query_client = new QueryClient();

const [useCell, CellProvider] = useContextHelper<Cell>(`Cell`);
const [usePlotID, PlotIDProvider] = useContextHelper<string>(`PlotID`);
const [useFieldID, FieldIDProvider] = useContextHelper<string>(`FieldID`);
const [useData, DataProvider] = useContextHelper<Datum[]>(`Data`);

export const Providers = {
  CellProvider,
  PlotIDProvider,
  FieldIDProvider,
  DataProvider,
};

export const hooks = {
  useStore,
  useCell,
  usePlotID,
  useFieldID,
  useData,
};

export function get_catalog_hierarchy(metadata: CatalogResponse) {
  const catalog_fields_raw = metadata.fields ?? null;
  const root = {
    sub: catalog_fields_raw,
    __is_root: true,
    __id: `root`,
  } as FieldMetadata;
  const hierarchy: d3.HierarchyNode<FieldMetadata> =
    d3.hierarchy<FieldMetadata>(root, (d) => d?.sub ?? []);
  return hierarchy;
}

export function get_filter_ids(
  hierarchy: d3.HierarchyNode<FieldMetadata>,
  actions: Action[]
) {
  const nodes = hierarchy.descendants();
  const initial_filter_ids = nodes
    .filter((node) => node.height === 0 && `required` in node.data)
    .map((node) => get_field_id(node.data));
  const filter_ids_set: Set<string> = new Set(initial_filter_ids);
  const filter_list_actions = actions.filter(
    (action): action is FilterListAction => {
      return action.type === `add_filter` || action.type === `remove_filter`;
    }
  );
  for (const action of filter_list_actions) {
    if (action.type === `remove_filter`) {
      filter_ids_set.delete(action.filter_id);
    } else if (action.type === `add_filter`) {
      filter_ids_set.add(action.filter_id);
    } else {
      throw new Error(`unknown filter action type: ${action.type}`);
    }
  }
  return filter_ids_set;
}

export function get_column_ids(
  hierarchy: d3.HierarchyNode<FieldMetadata>,
  actions: Action[]
): Set<string> {
  const nodes = hierarchy.descendants();
  const initial_column_ids = nodes
    .filter((node) => node.height === 0 && node.data.disp === true)
    .map((node) => get_field_id(node.data));
  const column_ids_set: Set<string> = new Set(initial_column_ids);
  const column_list_actions = actions.filter(
    (action): action is ColumnListAction =>
      action.type === `add_column` || action.type === `remove_column`
  );
  for (const action of column_list_actions) {
    if (action.type === `remove_column`) {
      column_ids_set.delete(action.column_id);
    } else if (action.type === `add_column`) {
      column_ids_set.add(action.column_id);
    } else {
      throw new Error(`unknown filter action type: ${action.type}`);
    }
  }
  return column_ids_set;
}

export function get_initial_cell_filters(
  filter_ids: Set<string>,
  catalog_field_hierarchy?: d3.HierarchyNode<FieldMetadata>
): Filters {
  if (!catalog_field_hierarchy) return {};
  const initial_filter_object: Filters = Object.fromEntries(
    Array.from(filter_ids).map((filter_id) => {
      const metadata = catalog_field_hierarchy.find(
        (node) => get_field_id(node.data) === filter_id
      )?.data;
      if (!metadata) {
        throw new Error(`Could not find metadata for filter ${filter_id}`);
      }
      const initial_value: Filters[string] = (() => {
        const type = metadata.type;
        if (type === `boolean`) {
          return false;
        } else if (metadata.terms) {
          return 0;
        } else if (type === `byte`) {
          return 0;
        } else if ([`float`, `short`].includes(type)) {
          if (!metadata.stats) {
            throw new Error(
              `Trying to use float filter without stats: ${filter_id}`
            );
          }
          if (
            metadata.stats.min === null ||
            metadata.stats.max === null ||
            !Number.isFinite(metadata.stats.min) ||
            !Number.isFinite(metadata.stats.max)
          ) {
            log(`meta`, metadata);
            throw new Error(`Missing min/max for float filter: ${filter_id}`);
          }
          return {
            gte: metadata.stats.min,
            lte: metadata.stats.max,
          };
        } else {
          return `unknown`;
          log(`meta`, metadata);
          throw new Error(`Unexpected filter type: ${type}`);
        }
      })();
      return [filter_id, initial_value];
    })
  );
  return initial_filter_object;
}

export function get_final_filters(
  initial_filters: Filters,
  filter_state_for_cell: Filters
) {
  const filters: Filters = {};
  for (const [field_id, initial_value] of Object.entries(initial_filters)) {
    const filter_state = filter_state_for_cell[field_id];
    const value = filter_state ?? initial_value;
    filters[field_id] = value;
  }
  return filters;
}

export const format = {
  concise: (d) => {
    if (d < 1e4) return d3.format(`,.4~g`)(d);
    return d3.format(`.2~e`)(d);
  },
  commas: (d) => {
    return d3.format(`,`)(d);
  },
};

export function get_field_id(metadata: FieldMetadata): string {
  if (metadata.__id) {
    return metadata.__id;
  } else {
    log(`Node is missing ID:`, metadata);
    throw new Error(`Node is missing ID`);
  }
}

export function set_field_id(metadata: FieldMetadata) {
  if (metadata.__id) return;
  if (metadata.name && !metadata.sub) {
    metadata.__id = metadata.name;
    return;
  }
  const hash = tiny_json_hash(metadata);
  let label = `unknown`;
  if (metadata.name && metadata.name.length > 0) label = metadata.name;
  else if (metadata.title && metadata.title.length > 0) label = metadata.title;
  metadata.__id = `${label}_${hash}`;
}

function tiny_json_hash(object) {
  const text = JSON.stringify(object);

  let hash = 5381;
  let index = text.length;

  while (index) {
    hash = (hash * 33) ^ text.charCodeAt(--index);
  }

  return (hash >>> 0).toString(16);
}

export function log(...args: any[]) {
  console.log(`🌔`, ...args);
}

export function dispatch_action(action: Action) {
  stores.actions.update(($actions) => [...$actions, action]);
}

export function is_catalog_cell_id(cell_id: CellID): CatalogCellID {
  if (!cell_id.match(/^catalog_cell_/)) {
    throw new Error(`${cell_id} is not a catalog cell id`);
  }
  return cell_id as CatalogCellID;
}
export function is_filter_cell_id(cell_id: CellID): FilterCellID {
  if (!cell_id.match(/^filter_cell_/)) {
    throw new Error(`${cell_id} is not a filter cell id`);
  }
  return cell_id as FilterCellID;
}
export function is_table_cell_id(cell_id: CellID): TableCellID {
  if (!cell_id.match(/^table_cell_/)) {
    throw new Error(`${cell_id} is not a table cell id`);
  }
  return cell_id as TableCellID;
}

export function is_plot_cell_id(cell_id: CellID): PlotCellID {
  if (!cell_id.match(/^plot_cell_/)) {
    throw new Error(`${cell_id} is not a table cell id`);
  }
  return cell_id as PlotCellID;
}

export function set_filter_value(
  cell_id: CellID,
  filter_id: string,
  filter_value: FilterValueRaw
) {
  stores.filter_state.update((fitler_state_object) => {
    return produce(fitler_state_object, (draft) => {
      draft[cell_id] = draft[cell_id] || {};
      draft[cell_id][filter_id] = filter_value;
    });
  });
}

export function useContextHelper<T>(debug: string) {
  const context = React.createContext<T | null>(null);

  const useContext = (): T => {
    const value: T | null = React.useContext(context);
    if (value === null) {
      throw new Error(`useContextHelper: ${debug}: value is null`);
    }
    return value;
  };

  return [useContext, context.Provider] as const;
}

export function useStore<T>(store: Readable<T>) {
  const [state, setState] = React.useState<T>(get(store));

  React.useEffect(
    () =>
      store.subscribe((value) => {
        setState(value);
      }),
    [store]
  );

  return state;
}

export function LabeledSelect<T>({
  button,
  buttonText,
  onClick,
  disabled,
  ...select_props
}: {
  button?: boolean;
  buttonText?: string;
  onClick?: () => void;
} & Parameters<typeof Select<T>>[0]): React.JSX.Element {
  return (
    <div className="grid gap-x-4 gap-y-2 items-center grid-cols-1 md:grid-cols-[max-content_1fr_max-content]">
      <Select {...select_props} disabled={disabled} />
      {button && (
        <button
          className="bg-light-3 dark:bg-dark-3 px-4 py-2 rounded-lg disabled:opacity-50"
          onClick={onClick}
          disabled={disabled}
        >
          {buttonText}
        </button>
      )}
    </div>
  );
}

// function Select({
//   label,
//   placeholder,
//   options,
//   value = undefined,
//   onValueChange = undefined,
// }: {
//   label: string;
//   placeholder: SelectValueProps["placeholder"];
//   options: string[];
//   value?: SelectProps["value"];
//   onValueChange?: SelectProps["onValueChange"];
// }): React.JSX.Element {
//   return (
//     <SelectPrimitive.Root value={value} onValueChange={onValueChange}>
//       <SelectPrimitive.Trigger
//         aria-label={label}
//         className="bg-light-2 dark:bg-dark-2 rounded-lg py-2"
//       >
//         <SelectPrimitive.Value placeholder={placeholder} aria-label={label}>
//           {value}
//         </SelectPrimitive.Value>
//       </SelectPrimitive.Trigger>
//       <SelectPrimitive.Portal>
//         <SelectPrimitive.Content position="popper">
//           <SelectPrimitive.Viewport className="bg-light-3 dark:bg-dark-3 p-2 rounded-lg shadow-lg">
//             {options.map((option) => {
//               return (
//                 <SelectPrimitive.Item
//                   value={option}
//                   key={option}
//                   className={clsx(
//                     `relative flex items-center`,
//                     `px-4 py-2 rounded-md`,
//                     `text-sm text-light-text dark:text-dark-text font-medium`,
//                     `focus:bg-light-0 dark:focus:bg-dark-0 focus:outline-none select-none`
//                   )}
//                 >
//                   <SelectPrimitive.ItemText>{option}</SelectPrimitive.ItemText>
//                 </SelectPrimitive.Item>
//               );
//             })}
//           </SelectPrimitive.Viewport>
//         </SelectPrimitive.Content>
//       </SelectPrimitive.Portal>
//     </SelectPrimitive.Root>
//   );
// }

export function Select<T>({
  label,
  placeholder,
  options,
  getKey = (d) => d.toString(),
  getDisplayName = (d) => d?.toString(),
  disabled = false,
  value = undefined,
  onValueChange = undefined,
  buttonClassName,
  optionsClassName,
  optionClassName,
}: {
  label?: string;
  placeholder?: string;
  options: T[];
  getKey?: (option: T) => string;
  getDisplayName?: (option: T) => string;
  disabled?: boolean;
  value?: T;
  onValueChange?: (value: T) => void;
  buttonClassName?: string;
  optionsClassName?: string;
  optionClassName?: string;
}) {
  return (
    <Listbox
      value={value ?? ({} as T)}
      onChange={onValueChange}
      disabled={disabled}
    >
      {label && <Listbox.Label>{label}</Listbox.Label>}
      <div className="relative">
        <Listbox.Button
          className={clsx(
            `relative w-full cursor-pointer rounded-md shadow-md`,
            `py-2 pl-3 pr-10 text-left disabled:opacity-50 disabled:cursor-wait`,
            buttonClassName
          )}
        >
          <span className="block">{getDisplayName(value) ?? placeholder}</span>
          <span className="pointer-events-none absolute inset-y-0 right-0 flex items-center pr-2">
            <ChevronDownIcon className="h-5 w-5 " aria-hidden="true" />
          </span>
        </Listbox.Button>
        <Listbox.Options
          className={clsx(
            `absolute z-50 w-full mt-1 py-1 shadow-lg overflow-auto rounded-md`,
            optionsClassName
          )}
        >
          {options.map((option) => (
            <Listbox.Option
              key={getKey(option)}
              value={option}
              className={clsx(
                `relative cursor-pointer select-none py-2 pl-3 pr-4`,
                optionClassName
              )}
            >
              {getDisplayName(option)}
            </Listbox.Option>
          ))}
        </Listbox.Options>
      </div>
    </Listbox>
  );
}

export function BigButton({
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
  "bg-light-4 dark:bg-dark-4 rounded-lg py-4 text-white font-bold text-xl";

export function CellWrapper({
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

export function find_parent_node_by_filter<T>(
  node: d3.HierarchyNode<T>,
  filter: (node: d3.HierarchyNode<T>) => boolean
): d3.HierarchyNode<T> | undefined {
  if (filter(node)) {
    return node;
  } else {
    return node.parent
      ? find_parent_node_by_filter(node.parent, filter)
      : undefined;
  }
}

export function create_query_observer<T>(
  options: QueryObserverOptions<T>
): QueryObserver<T> {
  log(`🐛 Creating Query Observer:`, options.queryKey, options);
  const defaulted_options = query_client.defaultQueryOptions(options);
  return new QueryObserver(query_client, defaulted_options);
}

export async function fetch_api_get<T>(path: string): Promise<T> {
  const url = new URL(`/api${path}`, FLATHUB_API_BASE_URL);
  log(`💥 Fetching`, url.toString());
  const response = await fetch(url.toString(), {
    method: `GET`,
    headers: new Headers({
      "Content-Type": `application/json`,
    }),
  });
  if (!response.ok) {
    throw new Error(`API Fetch Error: ${response.status}`);
  }
  const json: T = await response.json();
  return json;
}

export async function fetch_api_post<T>(
  path: string,
  body?: Record<string, any>
): Promise<T> {
  const url = new URL(`/api${path}`, FLATHUB_API_BASE_URL);
  log(`💥 fetching`, url.toString());
  const response = await fetch(url.toString(), {
    method: `POST`,
    headers: new Headers({
      "Content-Type": `application/json`,
    }),
    body: JSON.stringify(body),
  });
  if (!response.ok) {
    throw new Error(`API Fetch Error: ${response.status}`);
  }
  const json: T = await response.json();
  return json;
}

export function wrap_catalog_response(
  metadata: CatalogResponse
): CatalogMetadataWrapper {
  const catalog_fields_raw = metadata.fields ?? null;
  const root = {
    sub: catalog_fields_raw,
    __is_root: true,
    __id: `root`,
  } as FieldMetadata;
  const hierarchy: d3.HierarchyNode<FieldMetadata> =
    d3.hierarchy<FieldMetadata>(root, (d) => d?.sub ?? []);
  const nodes_array = get_nodes_depth_first<FieldMetadata>(hierarchy)
    .filter((d) => !d.data.__is_root)
    .map((d) => {
      set_field_id(d.data);
      return d;
    });
  const nodes_by_id = new Map<string, d3.HierarchyNode<FieldMetadata>>();
  for (const node of nodes_array) {
    const id = get_field_id(node.data);
    if (nodes_by_id.has(id)) {
      log(`Node:`, node);
      console.error(`Duplicate node ID: ${id}`);
    }
    nodes_by_id.set(id, node);
  }
  const initial_filter_ids = nodes_array
    .filter((node) => node.height === 0 && `required` in node.data)
    .map((node) => get_field_id(node.data));
  const initial_column_ids = nodes_array
    .filter((node) => node.height === 0 && node.data.disp === true)
    .map((node) => get_field_id(node.data));

  const field_metadata: Map<string, FieldMetadata> = new Map(
    nodes_array.map((node) => [get_field_id(node.data), node.data])
  );
  return {
    metadata,
    hierarchy,
    nodes_array,
    nodes_by_id,
    initial_filter_ids,
    initial_column_ids,
    get_field_metadata: (id: string) => field_metadata.get(id),
  };
}

export function get_nodes_depth_first<T>(
  root: d3.HierarchyNode<T>
): d3.HierarchyNode<T>[] {
  const nodes: d3.HierarchyNode<T>[] = [];
  root.eachBefore((d) => nodes.push(d));
  return nodes;
}

export function field_is_enum(metadata: FieldMetadata): boolean {
  const type_check = metadata.type === `boolean` || metadata.type === `byte`;
  const has_enum = `enum` in metadata && Array.isArray(metadata.enum);
  const has_terms = metadata.terms === true;
  const has_stats_terms = Array.isArray(metadata.stats?.terms);
  return type_check && has_enum && has_terms && has_stats_terms;
}

export function get_catalog_id(
  cells_hierarchy: d3.HierarchyNode<Cell>,
  cell_id: CellID
) {
  const found = cells_hierarchy.find((d) => d.data.cell_id === cell_id);
  if (found === undefined) {
    log(`No cell found for cell_id: ${cell_id}`, { cells_hierarchy });
    throw new Error(`No cell found for cell_id: ${cell_id}`);
  }
  let catalog_id = undefined;
  if (found.data.type === `catalog`) {
    catalog_id = found.data.catalog_id;
  } else {
    // Traverse parents until a catalog name is found
    const ancestor: d3.HierarchyNode<Cell> = find_parent_node_by_filter(
      found,
      (d) => d.data.type === `catalog`
    );
    if (!ancestor) {
      throw new Error(`No catalog ancestor found for cell_id: ${cell_id}`);
    }
    if (ancestor.data.type !== `catalog`) {
      throw new Error(
        `Expected ancestor to be a catalog, but was ${ancestor.data.type}`
      );
    }
    catalog_id = ancestor.data.catalog_id;
  }
  if (catalog_id === undefined) {
    throw new Error(`No catalog ID for cell_id: ${cell_id}`);
  }
  return catalog_id;
}

// export const field_metadata = {
//   cache: new Map<string, FieldMetadata>(),
//   get(
//     catalog_field_hierarchy: d3.HierarchyNode<FieldMetadata>,
//     field_id: string
//   ): FieldMetadata {},
// };

// export function get_field_metadata(catalog_field_hierarchy: d3.HierarchyNode<FieldMetadata>, field_id: FieldID): FieldMetadata {
