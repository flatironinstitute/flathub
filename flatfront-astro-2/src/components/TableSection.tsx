import React from "react";

import {
  useQuery,
  useQueryClient,
  type UseQueryOptions,
  type UseQueryResult
} from "@tanstack/react-query";

import type {
  CatalogHierarchyNode,
  CatalogMetadataWrapper,
  DataPostRequestBody,
  DataResponse,
  DataRow
} from "@/types";

import {
  FLATHUB_API_BASE_URL,
  cn,
  fetch_api_post,
  format,
  get_field_type,
  is_leaf_node,
  is_root_node,
  log
} from "@/utils";

import { useCatalogID } from "@/components/contexts/CatalogCellIDContext";
import {
  useColumnNames,
  useColumnIDs
} from "@/components/contexts/ColumnsContext";
import {
  useFilterValues,
  useFilterValuesWithFieldNames
} from "@/components/contexts/FiltersContext";
import { useRandomConfig } from "@/components/contexts/RandomContext";
import { useMatchingRows } from "@/components/contexts/MatchingRowsContext";

import { ScrollArea } from "./ui/scroll-area";
import { ArrowDownAZ, ArrowUpAZ, Download, Info } from "lucide-react";
import { Alert, AlertDescription, AlertTitle } from "./ui/alert";
import {
  useReactTable,
  type AccessorColumnDef,
  type ColumnDef,
  type GroupColumnDef,
  getCoreRowModel,
  flexRender,
  type RowData
} from "@tanstack/react-table";
import { Katex } from "@/components/ui/katex";
import {
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableHeader,
  TableRow
} from "@/components/ui/table";
import { useCatalogMetadata } from "./contexts/CatalogMetadataContext";
import { Button } from "./ui/button";
import {
  Select,
  SelectContent,
  SelectGroup,
  SelectItem,
  SelectLabel,
  SelectTrigger,
  SelectValue
} from "./ui/select";

export function TableSection() {
  return (
    <SortProvider>
      <div className="space-y-4">
        <TableParent />
        {/* <DownloadSection /> */}
      </div>
    </SortProvider>
  );
}

function TableParent() {
  const catalog_id = useCatalogID();

  const column_names = useColumnNames();

  const filters = useFilterValuesWithFieldNames();
  const random_config = useRandomConfig();

  const [rows_per_page, set_rows_per_page] = React.useState<number>(10);
  const [offset, set_offset] = React.useState<number>(0);

  const sort = useSort();
  const set_sort = useSetSort();

  React.useEffect(() => {
    set_offset(0);
  }, [JSON.stringify(filters), sort, catalog_id]);

  React.useEffect(() => {
    set_sort([]);
  }, [catalog_id]);

  const request_body: DataPostRequestBody = {
    object: true,
    fields: [`_id`, ...column_names],
    ...filters,
    count: rows_per_page,
    offset,
    sort: sort as any,
    ...random_config
  };

  const query_config = {
    path: `/${catalog_id}/data`,
    body: request_body
  };

  const query_key = [`table-data`, query_config];

  const enable_request = !!catalog_id && column_names.size > 0;

  const query = useQuery({
    queryKey: query_key,
    queryFn: async ({ signal }): Promise<DataResponse> => {
      return fetch_api_post<DataPostRequestBody, DataResponse>(
        query_config.path,
        query_config.body,
        { signal }
      );
    },
    enabled: enable_request
  });

  const content =
    query.data && query.data.length > 0 ? (
      <TablePrimitive
        data={query.data}
        isSorted={(id) => {
          if (sort[0]?.field === id) return sort[0]?.order;
          return false;
        }}
        onSortChange={(id) => {
          set_sort((previous_sort) => {
            const index = previous_sort.findIndex((d) => d.field === id);
            if (index === -1) {
              // Add it
              return [{ field: id, order: `asc` }, ...previous_sort];
            } else if (index === 0) {
              // Flip the order
              const [head, ...tail] = previous_sort;
              const new_order = head.order === `asc` ? `desc` : `asc`;
              return [{ ...head, order: new_order }, ...tail];
            } else if (index > 0) {
              // Move it to the front
              const found = previous_sort[index];
              const filtered = previous_sort.filter((d) => d.field !== id);
              return [found, ...filtered];
            }
            return previous_sort;
          });
        }}
      />
    ) : (
      <StatusBox query={query} queryKey={query_key} />
    );

  const rows_select = (
    <Select
      value={String(rows_per_page)}
      onValueChange={(value) => set_rows_per_page(Number(value))}
    >
      <SelectTrigger className="h-8 px-2 py-1">
        <SelectValue />
      </SelectTrigger>
      <SelectContent>
        <SelectGroup>
          {[10, 25, 50, 100].map((value) => (
            <SelectItem key={value} value={String(value)}>
              {value}
            </SelectItem>
          ))}
        </SelectGroup>
      </SelectContent>
    </Select>
  );

  const matching = useMatchingRows();

  const from = query?.data?.length === 0 ? 0 : offset + 1;
  const to = offset + (query?.data?.length ?? 0);

  const info_text = (() => {
    if (query?.status === `pending`) return `Loading...`;
    const total = Number.isFinite(matching)
      ? format.commas(matching)
      : `[Loading...]`;
    return `Showing ${from} to ${to} of ${total} rows`;
  })();

  const prev_next = (
    <div className="flex gap-x-4">
      <Button
        variant="outline"
        onClick={() => set_offset(offset - rows_per_page)}
        disabled={offset === 0}
      >
        Previous
      </Button>
      <Button
        variant="outline"
        onClick={() => set_offset(offset + rows_per_page)}
        disabled={to >= matching}
      >
        Next
      </Button>
    </div>
  );

  return (
    <>
      <ScrollArea
        className="w-full rounded-md border p-4"
        orientation="horizontal"
      >
        {content}
      </ScrollArea>
      <div className="flex items-center justify-between">
        <div className="flex items-center gap-x-2">Show {rows_select} rows</div>
        <div>{info_text}</div>
        {prev_next}
      </div>
    </>
  );
}

function TablePrimitive({
  data,
  onSortChange: on_sort_change,
  isSorted: check_if_sorted
}: {
  data: DataRow[];
  onSortChange?: (id: string) => void;
  isSorted?: (id: string) => `asc` | `desc` | false;
}) {
  const column_ids = useColumnIDs();
  const catalog_metadata = useCatalogMetadata();
  const columns = construct_table_columns({ catalog_metadata, column_ids });
  const table = useReactTable({
    data,
    columns,
    getCoreRowModel: getCoreRowModel()
  });

  const header_groups = table.getHeaderGroups();
  const skip_rendering = new Set();

  const headers = header_groups.map((headerGroup) => (
    <TableRow key={headerGroup.id} className="hover:bg-[unset]">
      {headerGroup.headers.map((header) => {
        if (skip_rendering.has(header.column.id)) return null;
        // Get the row span, accounting for placeholders
        // If it's a placeholder:
        // - Skip rendering any future headers
        // - Set row span based on the depth of non-placeholder header
        let row_span = 1;
        let is_leaf = header.subHeaders.length === 0;
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
          is_leaf = true;
        }
        const field_name = header.column.columnDef.meta.name;
        const set_sort = () => on_sort_change?.(field_name);
        const is_sorted = check_if_sorted?.(field_name);
        const sort_icon = {
          asc: <ArrowUpAZ className="inline-block" />,
          desc: <ArrowDownAZ className="inline-block" />
        }[is_sorted ? is_sorted : undefined];
        const on_click = is_leaf ? set_sort : undefined;
        return (
          <TableHead
            className={cn("border-2 px-2 py-1", is_leaf && `cursor-pointer`)}
            key={header.id}
            colSpan={header.colSpan}
            rowSpan={row_span}
            onClick={on_click}
          >
            {
              <div className="flex items-center gap-x-[0.5rem] whitespace-nowrap">
                {flexRender(
                  header.column.columnDef.header,
                  header.getContext()
                )}
                {sort_icon}
              </div>
            }
          </TableHead>
        );
      })}
    </TableRow>
  ));

  const rows = table.getRowModel().rows.map((row) => (
    <TableRow key={row.id} className="odd:bg-gray-100 dark:odd:bg-white/20">
      {row.getVisibleCells().map((cell) => (
        <TableCell
          key={cell.id}
          className="whitespace-nowrap px-2 py-1 text-right"
        >
          {flexRender(cell.column.columnDef.cell, cell.getContext())}
        </TableCell>
      ))}
    </TableRow>
  ));

  return (
    <Table>
      <TableHeader>{headers}</TableHeader>
      <TableBody>{rows}</TableBody>
    </Table>
  );
}

/**
 * We have a list of **leaf** field IDs that we want to display in a table.
 * We need to get their ancestors for the column headers.
 *
 * @returns An array of column definitions for use with react-table
 */
function construct_table_columns({
  catalog_metadata,
  column_ids
}: {
  catalog_metadata: CatalogMetadataWrapper;
  column_ids: Set<string>;
}): ColumnDef<DataRow>[] {
  const catalog_id = catalog_metadata.response.name;
  // Recursively construct column definitions
  const next = (
    node: CatalogHierarchyNode
  ): GroupColumnDef<DataRow> | AccessorColumnDef<DataRow> | null => {
    const field_id = catalog_metadata.get_id_from_node(node);
    const metadata = node.data;
    const field_name = metadata.name;
    // Include this node if:
    // - It is a leaf node, and is one of the fields in field_ids_set
    // - It is the ancestor of one of the fields in field_ids_set
    const is_visible_leaf_field =
      is_leaf_node(node) && column_ids.has(field_id);
    const is_ancestor_of_field = node
      .leaves()
      .some((child) =>
        column_ids.has(catalog_metadata.get_id_from_node(child))
      );
    const include = is_visible_leaf_field || is_ancestor_of_field;
    if (!include) return null;

    const child_columns: ColumnDef<DataRow>[] = [];

    for (const child of node.children ?? []) {
      const child_column = next(child);
      if (child_column) {
        child_columns.push(child_column);
      }
    }

    let column_id = field_id;

    if (column_id?.length === 0) {
      column_id = metadata.title;
    }

    if (is_root_node(node)) {
      column_id = `root`;
    }

    if (!column_id || column_id.length === 0) {
      console.error(
        `construct_table_columns: column_id is empty for node:`,
        node
      );
      throw new Error(`column_id is empty`);
    }

    const column_base: ColumnDef<DataRow> = {
      id: column_id,
      header: () => <Katex>{metadata.title ?? metadata.name}</Katex>,
      meta: {
        node,
        name: metadata.name
      }
    };

    if (child_columns.length === 0) {
      const column: AccessorColumnDef<DataRow> = {
        ...column_base,
        accessorFn: (row) => {
          const value = row[field_name];
          const field_type = get_field_type(metadata);
          if (metadata.attachment && value === true) {
            const url = new URL(
              `/api/${catalog_id}/attachment/${field_name}/${row._id}`,
              FLATHUB_API_BASE_URL
            );
            return (
              <a
                className="flex items-center justify-center"
                href={url.toString()}
              >
                <Download />
              </a>
            );
          } else if (
            field_type === `LABELLED_ENUMERABLE_INTEGER` ||
            field_type === `LABELLED_ENUMERABLE_BOOLEAN`
          ) {
            const index = typeof value === "number" ? value : Number(value);
            const text = metadata.enum[index];
            return text;
          }
          return value;
        },
        cell: (row) => row.getValue()
      };
      return column;
    } else {
      const column: GroupColumnDef<DataRow> = {
        ...column_base,
        columns: child_columns
      };
      return column;
    }
  };
  const root = next(catalog_metadata.hierarchy);
  if (root === null) {
    console.error(`construct_table_columns: root is null`);
    return null;
  }
  if (!("columns" in root)) throw new Error(`root.columns is not defined`);
  const columns = root?.columns ?? [];
  return columns;
}

function StatusBox({
  query,
  queryKey: query_key
}: {
  query: UseQueryResult;
  queryKey: UseQueryOptions[`queryKey`];
}) {
  const query_client = useQueryClient();
  const title = query.isFetching ? `Loading...` : `No Data`;
  const description = query.isFetching ? (
    <button
      onClick={() => {
        query_client.cancelQueries({ queryKey: query_key });
      }}
      className="cursor-pointer underline"
    >
      Cancel
    </button>
  ) : (
    `There is no data to display.`
  );
  return (
    <div className="grid h-[200px] items-center justify-center bg-secondary">
      <Alert>
        <Info className="h-4 w-4" />
        <AlertTitle>{title}</AlertTitle>
        <AlertDescription>{description}</AlertDescription>
      </Alert>
    </div>
  );
}

type TableSort = { field: string; order: `asc` | `desc` };

const SortContext = React.createContext<TableSort[]>([]);
const SetSortContext =
  React.createContext<React.Dispatch<React.SetStateAction<TableSort[]>>>(null);

function SortProvider({ children }: { children: React.ReactNode }) {
  const [sort, set_sort] = React.useState<TableSort[]>([]);
  return (
    <SortContext.Provider value={sort}>
      <SetSortContext.Provider value={set_sort}>
        {children}
      </SetSortContext.Provider>
    </SortContext.Provider>
  );
}

function useSort(): TableSort[] {
  const sort = React.useContext(SortContext);
  return sort;
}

function useSetSort(): React.Dispatch<React.SetStateAction<TableSort[]>> {
  const set_sort = React.useContext(SetSortContext);
  return set_sort;
}

declare module "@tanstack/table-core" {
  interface ColumnMeta<TData extends RowData, TValue> {
    node: CatalogHierarchyNode;
    name: string;
  }
}
