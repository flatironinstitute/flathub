"use client";

import type { components } from "./flathub-schema";
import type { Context, Dispatch, SetStateAction } from "react";
import type {
  ColumnDef,
  GroupColumnDef,
  AccessorColumnDef,
  HeaderGroup,
} from "@tanstack/react-table";
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
import { produce } from "immer";
import memoize from "fast-memoize";

export default function App() {
  // const app_controller = useAppControllerFactory();

  const [cells, set_cells] = React.useState<any[]>([
    {
      type: `query`,
      id: `query_1`,
      catalog: `camels`,
      query_config: {
        count: 100,
        object: true,
        fields: [
          "simulation_set",
          "simulation_set_id",
          "simulation_suite",
          "params_Omega_m",
          "params_sigma_8",
          "Group_BHMdot",
        ],
        params_Omega_m: {
          gte: 0.4,
        },
      },
    },
    {
      type: `query`,
      id: `query_2`,
      catalog: `camels`,
      query_config: {
        count: 100,
        object: true,
        fields: [
          "simulation_set",
          "simulation_set_id",
          "simulation_suite",
          "params_Omega_m",
          "params_sigma_8",
        ],
        params_sigma_8: {
          gte: 0.7,
        },
      },
    },
  ]);

  const app_controller = React.useMemo(() => {
    return {
      get_query_config: (id: string) => {
        const cell = cells.find((cell) => cell.id === id);
        if (cell.type !== `query`) {
          throw new Error(`not a query cell`);
        }
        return cell.query_config;
      },
    };
  }, []);

  return (
    <QueryClientProvider client={query_client}>
      <AppControllerContext.Provider value={app_controller}>
        <main
          className="ms-auto me-auto flex flex-col gap-y-10"
          style={{ width: `min(900px, 90vw)` }}
        >
          <div className="h-10" />
          {cells.map((cell) => {
            return <QueryCell key={cell.id} {...cell} />;
          })}
          <div className="h-10" />
        </main>
      </AppControllerContext.Provider>
    </QueryClientProvider>
  );
}

const query_client = new QueryClient();

function QueryCell({ id: query_id, catalog }: { id: string; catalog: string }) {
  const catalog_metadata_query = useQuery({
    queryKey: ["catalog_metadata", { catalog: `camels` }],
    queryFn: (): Promise<CatalogResponse> => {
      const path = `/${catalog}`;
      const FLATHUB_API_BASE_URL = `https://flathub.flatironinstitute.org`;
      const url = new URL(`/api${path}`, FLATHUB_API_BASE_URL);
      log(`💥 fetching`, url.toString());
      return fetch(url.toString(), {
        method: `GET`,
        headers: new Headers({
          "Content-Type": `application/json`,
        }),
      }).then((response) => {
        if (!response.ok) {
          throw new Error(`API Fetch Error: ${response.status}`);
        }
        return response.json() as Promise<CatalogResponse>;
      });
    },
  });

  const catalog_fields_raw = catalog_metadata_query.data?.fields ?? null;

  const catalog_field_hierarchy = React.useMemo(() => {
    if (!catalog_fields_raw) return null;
    const root = { sub: catalog_fields_raw } as FieldGroup;
    const hierarchy = d3.hierarchy<FieldGroup>(root, (d) => d?.sub ?? []);
    return hierarchy;
  }, [catalog_fields_raw]);

  log(`catalog_field_hierarchy: ${catalog}`, catalog_field_hierarchy);

  const query_config = {
    path: `/camels/data`,
    body: {
      count: 100,
      object: true,
      fields: [
        "simulation_set",
        "simulation_set_id",
        "simulation_suite",
        "params_Omega_m",
        "params_sigma_8",
        "Group_CM_y",
        "Group_BHMdot",
      ],
      params_Omega_m: {
        gte: 0.4,
      },
    },
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
      <BigButton onClick={() => data_query.refetch()}>
        {fetching ? `Fetching Data...` : `Fetch Data`}
      </BigButton>
      <div>status: {data_query.status}</div>
      <div>fetch status: {data_query.fetchStatus}</div>
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
    </CellWrapper>
  );
}

function get_nodes_depth_first<T>(
  root: d3.HierarchyNode<T>
): d3.HierarchyNode<T>[] {
  const nodes: d3.HierarchyNode<T>[] = [];
  root.eachBefore((d) => nodes.push(d));
  return nodes;
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
    <div>
      <table>
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
  catalog_field_hierarchy: d3.HierarchyNode<FieldGroup>
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
    .filter((d): d is d3.HierarchyNode<FieldGroup> => typeof d !== "undefined");

  // Get unix-like paths for nodes
  const get_path = (node: d3.HierarchyNode<FieldGroup>) => {
    return node
      .ancestors()
      .map((d) => d.data.name ?? `ROOT`)
      .reverse()
      .join(`/`);
  };

  const field_paths = field_nodes.map(get_path);

  const next = (
    node: d3.HierarchyNode<FieldGroup>
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

function useAppController(): AppController {
  return React.useContext(AppControllerContext);
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

const AppControllerContext: Context<AppController> = React.createContext(
  {} as AppController
);

// type AppController = ReturnType<typeof useAppControllerFactory>;
type AppController = any;

type Field = any;

type FieldGroup = components["schemas"]["FieldGroup"];

type CatalogResponse =
  components["responses"]["catalog"]["content"]["application/json"];

type DataResponse = Array<Datum>;
type Datum = Record<string, any>;
