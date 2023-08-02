"use client";

import type * as schema from "./flathub-schema";
import type { Context, Dispatch, SetStateAction } from "react";
import type {
  ColumnDef,
  GroupColumnDef,
  AccessorColumnDef,
  HeaderGroup,
} from "@tanstack/react-table";
import "highcharts/css/highcharts.css";
import React from "react";
import {
  useQuery,
  useMutation,
  useQueryClient,
  QueryClient,
  QueryClientProvider,
} from "@tanstack/react-query";
import {
  createColumnHelper,
  flexRender,
  getCoreRowModel,
  getPaginationRowModel,
  useReactTable,
} from "@tanstack/react-table";
import * as uuid from "uuid";
import { Listbox, Switch } from "@headlessui/react";
import { ChevronUpDownIcon, CheckCircleIcon } from "@heroicons/react/20/solid";
import * as d3 from "d3";
import KaTeX from "katex";
import renderMathInElement from "katex/contrib/auto-render";
import Highcharts from "highcharts";
import HighchartsReact from "highcharts-react-official";
import { produce } from "immer";
import memoize from "fast-memoize";
import { createReducerContext } from "react-use";
import * as Slider from "@radix-ui/react-slider";

export default function App() {
  const [cells, cells_dispatch] = React.useReducer<
    React.Reducer<Cell[], CellAction>
  >(
    produce((draft, action) => {
      console.log(`cells reducer`, action);
    }),
    [
      {
        type: `query`,
        id: `query_1`,
        catalog_name: `camels`,
      },
      {
        type: `query`,
        id: `query_2`,
        catalog_name: `camels`,
      },
    ]
  );

  return (
    <QueryClientProvider client={query_client}>
      <main
        className="ms-auto me-auto flex flex-col gap-y-10"
        style={{ width: `min(900px, 90vw)` }}
      >
        <div className="h-10" />
        {cells.map((cell) => {
          return (
            <QueryCell
              key={cell.id}
              id={cell.id}
              catalog_name={cell.catalog_name}
            />
          );
        })}
        <div className="h-10" />
      </main>
    </QueryClientProvider>
  );
}

const query_client = new QueryClient();

function QueryCell({
  id: cell_id,
  catalog_name,
}: {
  id: string;
  catalog_name: string;
}) {
  const catalog_metadata_query = useCatalogMetadata(catalog_name);

  const catalog_field_hierarchy: CatalogHierarchyNode | undefined =
    catalog_metadata_query.data?.hierarchy;

  const initial_filters: Filters = get_initial_cell_filters(
    catalog_metadata_query
  );

  const [filter_actions, set_filter_actions] = React.useState<FilterAction[]>(
    []
  );

  const dispatch_filter_action = (action: FilterAction) => {
    set_filter_actions((prev) => [...prev, action]);
  };

  const filters = React.useMemo(() => {
    let filters = initial_filters;
    for (const action of filter_actions) {
      filters = produce(filters, (draft) => {
        if (action.type === `set_filter_value`) {
          draft[action.filter_name] = action.value;
        } else {
          throw new Error(`unknown filter action type: ${action.type}`);
        }
      });
    }
    return filters;
  }, [filter_actions, initial_filters]);

  log(`filters`, filters, initial_filters);

  const field_names = get_cell_field_names(catalog_metadata_query);

  const request_body: DataRequestBody = {
    object: true,
    count: 100,
    fields: field_names,
    ...filters,
  };

  const query_config = {
    path: `/camels/data`,
    body: request_body,
  };

  const data_query = useQuery({
    queryKey: ["data", query_config],
    queryFn: (): Promise<DataResponse> => {
      return fetch_from_api<DataResponse>(
        query_config.path,
        query_config.body
      ).then((response_wrapper) => {
        log(`query response`, response_wrapper);
        return response_wrapper;
      });
    },
    enabled: false,
  });

  const fetching = data_query.isFetching;
  const query_data = data_query.data ?? null;

  const ready_to_render = catalog_field_hierarchy && query_data;

  return (
    <CellWrapper>
      <CellTitle>Query</CellTitle>
      <BigButton onClick={() => {}}>Show All Fields</BigButton>
      <CellSection label="filters">
        <CellFiltersSection
          filters={filters}
          render={(filter_name) => (
            <FieldCard
              key={filter_name}
              field_name={filter_name}
              filters={filters}
              catalog_field_hierarchy={catalog_field_hierarchy!}
              renderSlider={({ min, max, value }) => {
                return (
                  <RangeSlider
                    min={min}
                    max={max}
                    value={value}
                    onValueChange={([low, high]) => {
                      log(`on value change`, low, high);
                      dispatch_filter_action({
                        type: `set_filter_value`,
                        filter_name,
                        value: {
                          gte: low,
                          lte: high,
                        },
                      });
                    }}
                  />
                );
              }}
              minimal
            />
          )}
        />
      </CellSection>
      <CellSection label="fields">
        <CellFieldsSection field_names={field_names} />
      </CellSection>
      <CellSection label="fetch">
        <BigButton onClick={() => data_query.refetch()}>
          {fetching ? `Fetching Data...` : `Fetch Data`}
        </BigButton>
        <div>status: {data_query.status}</div>
        <div>fetch status: {data_query.fetchStatus}</div>
      </CellSection>
      <CellSection label="table">
        {ready_to_render ? (
          <Table
            data={query_data}
            catalog_field_hierarchy={catalog_field_hierarchy}
          />
        ) : (
          <PendingBox>No Results</PendingBox>
        )}
      </CellSection>
      <CellSection label="scatterplot">
        {ready_to_render ? (
          <Scatterplot
            data={query_data}
            catalog_field_hierarchy={catalog_field_hierarchy}
          />
        ) : (
          <PendingBox>No Results</PendingBox>
        )}
      </CellSection>
    </CellWrapper>
  );
}

function CellFiltersSection({
  filters,
  render,
}: {
  filters: Filters;
  render: (filter_name: string) => React.JSX.Element;
}) {
  const filter_names = Object.keys(filters);
  return (
    <div className="grid grid-cols-1 gap-x-3 gap-y-2">
      {filter_names.map((filter_name) => render(filter_name))}
    </div>
  );
}

function FieldCard({
  field_name,
  filters,
  catalog_field_hierarchy,
  renderSlider,
  minimal = true,
}: {
  field_name: string;
  filters: Filters;
  catalog_field_hierarchy: CatalogHierarchyNode;
  renderSlider: (props: {
    min: number;
    max: number;
    value: [number, number];
  }) => React.JSX.Element;
  minimal?: boolean;
}): React.JSX.Element {
  const field_metadata = get_field_metadata(
    field_name,
    catalog_field_hierarchy
  );

  const filter_state = filters[field_name];

  if (typeof filter_state === `undefined`) {
    log(filters);
    throw new Error(`Could not find filter state for ${field_name}`);
  }

  // log(`field card`, field_name, field_metadata, filter_state);

  const field_control = (() => {
    const metadata = field_metadata?.data;
    log(`field control`, metadata);
    if (!metadata) {
      throw new Error(`Could not find metadata for ${field_name}`);
    }
    if (metadata.type === `float`) {
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
      if (!(typeof filter_state === `object`)) {
        throw new Error(
          `Expected filter state to be an object for ${field_name}`
        );
      }
      if (!(`gte` in filter_state) || !(`lte` in filter_state)) {
        log(filters, filter_state);
        throw new Error(
          `Expected filter state to have gte/lte for ${field_name}`
        );
      }
      const low = filter_state.gte;
      const high = filter_state.lte;
      if (typeof low !== `number` || typeof high !== `number`) {
        throw new Error(
          `Expected filter state to have gte/lte numbers for ${field_name}`
        );
      }
      const value = [low, high] as [number, number];
      return renderSlider({ min, max, value });
      // return (
      //   <RangeSlider
      //     min={min}
      //     max={max}
      //     value={[low, high]}
      //     onValueChange={([low, high]) => {
      //       log(`on value change`, low, high);
      //     }}
      //   />
      // );
    }
  })();

  return (
    <div className="text-xs text-slate-200 px-2 rounded dark:bg-slate-600">
      {field_name}
      {field_control}
    </div>
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
  log(`range slider`, min, max);
  const range = Math.abs(max - min);
  const step = range / 100;
  const thumb_class = `cursor-pointer block h-5 w-5 rounded-full dark:bg-white focus:outline-none focus-visible:ring focus-visible:ring-purple-500 focus-visible:ring-opacity-75`;
  return (
    <Slider.Root
      min={min}
      max={max}
      value={value}
      className="relative flex h-5 w-64 touch-none items-center"
      onValueChange={onValueChange}
      step={step}
    >
      <Slider.Track className="relative h-1 w-full grow rounded-full bg-white dark:bg-gray-800">
        <Slider.Range className="absolute h-full rounded-full bg-purple-600 dark:bg-white" />
      </Slider.Track>
      <Slider.Thumb className={thumb_class} />
      <Slider.Thumb className={thumb_class} />
    </Slider.Root>
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

function CellFieldsSection({ field_names }: { field_names: string[] }) {
  return (
    <div className="grid grid-cols-3 gap-x-3 gap-y-2">
      {field_names.map((field_name) => {
        return (
          <div
            key={field_name}
            className="text-xs text-slate-200 px-2 rounded dark:bg-slate-600"
          >
            {field_name}
          </div>
        );
      })}
    </div>
  );
}

// function useFilterState(field_name: string) {}

// function useFieldMetadata(field_name: string): d3.HierarchyNode<FieldGroup> {
//   const catalog_metadata_query = useCatalogMetadata();
//   if (!catalog_metadata_query.data) {
//     throw new Error(`Trying to get field metadata before catalog loaded`);
//   }
//   const field_metadata = catalog_metadata_query.data?.hierarchy.find(
//     (node) => node.data.name === field_name
//   );
//   if (!field_metadata) {
//     throw new Error(`Could not find metadata for field ${field_name}`);
//   }
//   return field_metadata;
// }

function get_initial_cell_filters(
  catalog_metadata_query: ReturnType<typeof useCatalogMetadata>
): Filters {
  if (!catalog_metadata_query.data) {
    return {};
  }
  const initial_filter_names =
    catalog_metadata_query.data?.initial_filter_names;
  log(`initial_filters:`, initial_filter_names);
  const initial_filter_object: Filters = Object.fromEntries(
    initial_filter_names.map((filter_name) => {
      const metadata = catalog_metadata_query.data?.hierarchy.find(
        (node) => node.data.name === filter_name
      )?.data;
      if (!metadata) {
        throw new Error(`Could not find metadata for filter ${filter_name}`);
      }
      const initial_value: Filters[string] = (() => {
        const type = metadata.type;
        if (type === `boolean`) {
          return true;
        } else if (type === `byte`) {
          return 0;
        } else if ([`float`, `short`].includes(type)) {
          if (!metadata.stats) {
            throw new Error(
              `Trying to use float filter without stats: ${filter_name}`
            );
          }
          if (
            metadata.stats.min === null ||
            metadata.stats.max === null ||
            !Number.isFinite(metadata.stats.min) ||
            !Number.isFinite(metadata.stats.max)
          ) {
            throw new Error(`Missing min/max for float filter: ${filter_name}`);
          }
          return {
            gte: metadata.stats.min,
            lte: metadata.stats.max,
          };
        } else {
          log(`meta`, metadata);
          throw new Error(`Unexpected filter type: ${type}`);
        }
      })();
      return [filter_name, initial_value];
    })
  );
  return initial_filter_object;
}

function get_cell_field_names(catalog_metadata_query: CatalogMetadataQuery) {
  if (!catalog_metadata_query.data) {
    return [];
  }
  const initial_field_names = catalog_metadata_query.data?.initial_field_names;
  const current_field_names = initial_field_names;
  return current_field_names;
}

// function useCellFields(): string[] {
//   const catalog_metadata_query = useCatalogMetadata();
//   if (!catalog_metadata_query.data) {
//     return [];
//   }
//   const initial_field_names = catalog_metadata_query.data?.initial_field_names;
//   const current_field_names = initial_field_names;
//   return current_field_names;
// }

// function useCatalogName(): string {
//   const cell_id = useCellID();
//   const [cells] = useCells();
//   const cell = cells.find((cell) => cell.id === cell_id);
//   if (!cell) {
//     throw new Error(`Cell not found: ${cell_id}`);
//   }
//   const catalog_name = cell.catalog;
//   return catalog_name;
// }

// function useCellID(): string {
//   const cell_id = React.useContext(CellIDContext);
//   if (!cell_id) {
//     throw new Error(`useCellID must be used within a CellIDContext.Provider`);
//   }
//   return cell_id;
// }

// const CellIDContext = React.createContext<string | undefined>(undefined);

// const [useCells, CellsProvider] = createReducerContext<
//   React.Reducer<Cell[], CellAction>
// >(
//   produce((draft, action) => {
//     console.log(`cells reducer`, action);
//   }),
//   [
//     {
//       type: `query`,
//       id: `query_1`,
//       catalog: `camels`,
//     },
//     {
//       type: `query`,
//       id: `query_2`,
//       catalog: `camels`,
//     },
//   ]
// );

function useCatalogMetadata(catalog_name: string) {
  return useQuery({
    queryKey: ["catalog_metadata", { catalog: catalog_name }],
    queryFn: async (): Promise<{
      metadata: CatalogResponse;
      hierarchy: d3.HierarchyNode<FieldGroup>;
      nodes: d3.HierarchyNode<FieldGroup>[];
      initial_filter_names: string[];
      initial_field_names: string[];
    }> => {
      const path = `/${catalog_name}`;
      const FLATHUB_API_BASE_URL = `https://flathub.flatironinstitute.org`;
      const url = new URL(`/api${path}`, FLATHUB_API_BASE_URL);
      log(`💥 fetching`, url.toString());
      try {
        const response = await fetch(url.toString(), {
          method: `GET`,
          headers: new Headers({
            "Content-Type": `application/json`,
          }),
        });
        if (!response.ok) {
          throw new Error(`API Fetch Error: ${response.status}`);
        }
        const metadata = (await response.json()) as CatalogResponse;
        log(`💥 metadata`, metadata);
        const catalog_fields_raw = metadata.fields ?? null;
        const root = { sub: catalog_fields_raw } as FieldGroup;
        const hierarchy: d3.HierarchyNode<FieldGroup> =
          d3.hierarchy<FieldGroup>(root, (d) => d?.sub ?? []);
        const nodes = get_nodes_depth_first<FieldGroup>(hierarchy).filter(
          (d) => `name` in d.data
        );
        const initial_filter_names = nodes
          .filter((node) => node.height === 0 && `required` in node.data)
          .map((node) => node.data.name);
        const initial_field_names = nodes
          .filter((node) => node.height === 0 && node.data.disp === true)
          .map((node) => node.data.name);
        return {
          metadata,
          hierarchy,
          nodes,
          initial_filter_names,
          initial_field_names,
        };
      } catch (err: any) {
        throw new Error(`API Fetch Error: ${err.toString()}`);
      }
    },
  });
}

function get_nodes_depth_first<T>(
  root: d3.HierarchyNode<T>
): d3.HierarchyNode<T>[] {
  const nodes: d3.HierarchyNode<T>[] = [];
  root.eachBefore((d) => nodes.push(d));
  return nodes;
}

function Scatterplot({
  data,
  catalog_field_hierarchy,
}: {
  data: DataResponse;
  catalog_field_hierarchy: d3.HierarchyNode<FieldGroup>;
}) {
  const options: Highcharts.Options = {
    chart: {
      animation: false,
      styledMode: true,
    },
    title: {
      text: "My chart",
    },
    series: [
      {
        type: "scatter",
        animation: false,
        data: [1, 2, 3],
      },
    ],
  };

  return (
    <div>
      <HighchartsReact
        highcharts={Highcharts}
        options={options}
        containerProps={{ className: `highcharts-dark` }}
      />
    </div>
  );
}

function Table({
  data,
  catalog_field_hierarchy,
}: {
  data: DataResponse;
  catalog_field_hierarchy: d3.HierarchyNode<FieldGroup>;
}) {
  const columns = construct_table_columns(data, catalog_field_hierarchy);

  const table = useReactTable({
    data,
    columns,
    getCoreRowModel: getCoreRowModel(),
    getPaginationRowModel: getPaginationRowModel(),
  });

  const header_groups_raw = table.getHeaderGroups();

  const header_groups = fix_header_groups(header_groups_raw);

  const skip_rendering = new Set();

  return (
    <div className="overflow-x-scroll">
      <table className="text-xs">
        <thead>
          {header_groups.map((headerGroup) => (
            <tr key={headerGroup.id}>
              {headerGroup.headers.map((header_initial) => {
                let header = header_initial;
                let row_span = 1;
                if (skip_rendering.has(header.column.id)) return null;
                // If it's a placeholder:
                // - Skip rendering any future headers
                // - Set row span based on the depth of non-placeholder header
                if (header.isPlaceholder) {
                  skip_rendering.add(header.column.id);
                  const leaves = header.getLeafHeaders();
                  const non_placeholder_header = leaves.find(
                    (leaf) => !leaf.isPlaceholder
                  );
                  if (!non_placeholder_header) {
                    throw new Error(
                      `No non-placeholder header found for ${header.column.id}`
                    );
                  }
                  row_span = 1 + non_placeholder_header.depth - header.depth;
                }
                return (
                  <th
                    key={header.id}
                    colSpan={header.colSpan}
                    rowSpan={row_span}
                  >
                    {
                      <div>
                        {flexRender(
                          header.column.columnDef.header,
                          header.getContext()
                        )}
                      </div>
                    }
                  </th>
                );
              })}
            </tr>
          ))}
        </thead>
        <tbody>
          {table.getRowModel().rows.map((row) => (
            <tr key={row.id}>
              {row.getVisibleCells().map((cell) => (
                <td key={cell.id}>
                  {flexRender(cell.column.columnDef.cell, cell.getContext())}
                </td>
              ))}
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  );
}

function fix_header_groups<T>(raw: HeaderGroup<T>[]): HeaderGroup<T>[] {
  log(`FIX HEADER GROUPS`, raw);
  return raw;
}

/**
 * We have a list of **leaf** fields that we want to display in a table.
 * We need to get their ancestors for the column headers.
 *
 * @param data Table data
 * @param catalog_field_hierarchy The complete field hierarchy for this catalog
 * @returns An array of column definitions for use with react-table
 */
function construct_table_columns<T>(
  data: DataResponse,
  catalog_field_hierarchy: CatalogHierarchyNode
) {
  // Get field names based on keys in data
  const field_names = Object.keys(data[0]);

  // Find the field nodes in the catalog field hierarchy
  const field_nodes = field_names
    .map((field_name) => {
      return catalog_field_hierarchy.find(
        (d: any) => d.data.name === field_name
      );
    })
    .filter((d): d is CatalogHierarchyNode => typeof d !== "undefined");

  // Get unix-like paths for nodes
  const get_path = (node: CatalogHierarchyNode) => {
    return node
      .ancestors()
      .map((d) => d.data.name ?? `ROOT`)
      .reverse()
      .join(`/`);
  };

  const field_paths = field_nodes.map(get_path);

  const next = (
    node: CatalogHierarchyNode
  ): GroupColumnDef<Datum> | AccessorColumnDef<Datum> | null => {
    // Get the path for this node
    const path = get_path(node);

    const include = field_paths.some((d) => d.startsWith(path));

    // Only include this node if its path is a partial match of one of the field_paths above
    if (!include) return null;

    const child_columns: ColumnDef<Datum>[] = [];

    for (const child of node.children ?? []) {
      const child_column = next(child);
      if (child_column) {
        child_columns.push(child_column);
      }
    }

    const column_base: ColumnDef<Datum> = {
      id: node.data.name,
      header: () => <Katex>{node.data.title ?? node.data.name}</Katex>,
    };

    if (child_columns.length === 0) {
      const column: AccessorColumnDef<Datum> = {
        ...column_base,
        accessorKey: node.data.name,
        cell: (row) => row.getValue(),
      };
      return column;
    } else {
      const column: GroupColumnDef<Datum> = {
        ...column_base,
        columns: child_columns,
      };
      return column;
    }
  };

  const root = next(catalog_field_hierarchy);

  if (root === null) {
    throw new Error(`root is null`);
  }

  if (!("columns" in root)) {
    throw new Error(`root.columns is not defined`);
  }

  const columns = root?.columns ?? [];

  log(`columns`, columns);

  return columns;
}

function PendingBox({ children }: { children: React.ReactNode }) {
  return (
    <div className="h-40 rounded-lg p-4 outline-2 outline-dashed outline-slate-50 grid place-items-center opacity-50">
      {children}
    </div>
  );
}

function CellTitle({
  children,
}: {
  children: React.ReactNode;
}): React.JSX.Element {
  return <div className="text-2xl font-bold">{children}</div>;
}

function CellSection({
  label,
  children = null,
}: {
  label: string;
  children?: React.ReactNode;
}): React.JSX.Element {
  return (
    <div className="flex flex-col">
      <SimpleLabel>{label}</SimpleLabel>
      <div className="h-4"></div>
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
    <button
      className="bg-slate-500 dark:bg-slate-600 rounded-lg py-4 text-white font-bold text-xl"
      onClick={onClick}
    >
      {children}
    </button>
  );
}

function CellWrapper({
  children,
}: {
  children: React.ReactNode;
}): React.JSX.Element {
  return (
    <div className="rounded font-mono bg-slate-200 dark:bg-slate-700 p-6 shadow-lg shadow-black dark:shadow-lg dark:shadow-black w-full transition-all flex flex-col gap-y-10">
      {children}
    </div>
  );
}

function Katex({
  children,
  className,
}: {
  children: React.ReactNode;
  className?: string;
}): React.JSX.Element {
  const ref = React.useRef<HTMLSpanElement>(null);
  React.useEffect(() => {
    if (ref.current !== null) {
      renderMathInElement(ref.current, {
        output: "mathml",
        throwOnError: false,
        trust: true,
        delimiters: [
          { left: "$$", right: "$$", display: false },
          { left: "$", right: "$", display: false },
          { left: "\\(", right: "\\)", display: false },
        ],
      });
    }
  }, [ref]);
  return (
    <span ref={ref} className={className}>
      {children}
    </span>
  );
}

function useAppControllerFactory() {
  // const data_responses = useStateObject<
  //   Record<string, ResponseWrapper<DataResponse>>
  // >({});
  // const get_query_config = React.useCallback((query_id: string) => {
  //   return {
  //     path: `/camels/data`,
  //     body: {
  //       count: 100,
  //       object: true,
  //       fields: [
  //         "simulation_set",
  //         "simulation_set_id",
  //         "simulation_suite",
  //         "params_Omega_m",
  //       ],
  //       params_Omega_m: {
  //         gte: 0.4,
  //       },
  //     },
  //   };
  // }, []);
  // return {
  //   get_query_config,
  //   get_data_response: (query_id: string) => {
  //     return data_responses.value[query_id] ?? null;
  //   },
  //   fetch_data: (query_id: string) => {
  //     const query = get_query_config(query_id);
  //     return fetch_from_api<DataResponse>(query.path, query.body).then(
  //       (response_wrapper) => {
  //         log(`query response`, response_wrapper);
  //         data_responses.set((d) => ({ ...d, [query_id]: response_wrapper }));
  //         return response_wrapper;
  //       }
  //     );
  //   },
  // };
}

function useStateObject<T>(initial_value: T): {
  value: T;
  set: Dispatch<SetStateAction<T>>;
} {
  const [value, set] = React.useState<T>(initial_value);
  return { value, set };
}

function fetch_from_api<T>(
  path: string,
  body?: Record<string, any>
): Promise<T> {
  const FLATHUB_API_BASE_URL = `https://flathub.flatironinstitute.org`;
  const url = new URL(`/api${path}`, FLATHUB_API_BASE_URL);
  log(`💥 fetching`, url.toString());
  return fetch(url.toString(), {
    method: `POST`,
    headers: new Headers({
      "Content-Type": `application/json`,
    }),
    body: JSON.stringify(body),
  }).then((response) => {
    if (!response.ok) {
      throw new Error(`API Fetch Error: ${response.status}`);
    }
    return response.json() as T;
  });
}

function log(...args: any[]) {
  console.log(`🌔`, ...args);
}

type CatalogHierarchyNode = d3.HierarchyNode<FieldGroup>;

type CatalogMetadataQuery = ReturnType<typeof useCatalogMetadata>;

type Cell = any;
type CellAction = any;

type Field = any;
type Filters = schema.components["schemas"]["Filters"];

type FilterAction = {
  type: `set_filter_value`;
  filter_name: string;
  value: { gte: number; lte: number };
};

// type DataQueryBody = schema.operations["dataPOST"]["requestBody"];

type DataRequestBody = NonNullable<
  schema.operations["dataPOST"]["requestBody"]
>["content"]["application/json"];

type FieldGroup = schema.components["schemas"]["FieldGroup"];

type CatalogResponse =
  schema.components["responses"]["catalog"]["content"]["application/json"];

type DataResponse = Array<Datum>;
type Datum = Record<string, any>;
