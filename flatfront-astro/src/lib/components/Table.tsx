import type {
  CatalogHierarchyNode,
  DataPostRequestBody,
  DataResponse,
  DataRow
} from "../types";

import React from "react";
import { useQuery } from "@tanstack/react-query";
import {
  type AccessorColumnDef,
  type ColumnDef,
  type GroupColumnDef,
  flexRender,
  getCoreRowModel,
  useReactTable
} from "@tanstack/react-table";
import * as RadixIcons from "@radix-ui/react-icons";
import {
  fetch_api_post,
  FLATHUB_API_BASE_URL,
  format,
  get_field_type,
  is_leaf_node,
  is_root_node,
  log
} from "../shared";
import { useCatalogID, useMatchingRows } from "../contexts/CatalogContext";
import { useFilters } from "../contexts/FiltersContext";
import { useCatalogMetadata } from "../contexts/CatalogMetadataContext";
import { useColumns } from "../contexts/ColumnsContext";
import { CollapsibleSection, Placeholder } from "./Primitives";
import Katex from "./Katex";
import BrowseFieldsDialog from "./BrowseFieldsDialog";
import { useRandomConfig } from "../contexts/RandomContext";
import clsx from "clsx";

export default function TableSection() {
  return (
    <CollapsibleSection label="table">
      <div className="space-y-4">
        <BrowseFieldsDialog label="Select Columns" />
        <Table />
      </div>
    </CollapsibleSection>
  );
}

function Table() {
  const catalog_id = useCatalogID();

  const column_ids = useColumns();

  const filters = useFilters();
  const random_config = useRandomConfig();

  const [rows_per_page, set_rows_per_page] = React.useState<number>(25);
  const [offset, set_offset] = React.useState<number>(0);
  const [sort, set_sort] = React.useState<
    { field: string; order: `asc` | `desc` }[]
  >([]);

  React.useEffect(() => {
    set_offset(0);
  }, [JSON.stringify(filters), sort, catalog_id]);

  React.useEffect(() => {
    set_sort([]);
  }, [catalog_id]);

  const request_body: DataPostRequestBody = {
    object: true,
    fields: [`_id`, ...column_ids],
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

  const enable_request = !!catalog_id && column_ids.size > 0;

  const query = useQuery({
    queryKey: [`table-data`, query_config],
    queryFn: async ({ signal }): Promise<DataResponse> => {
      return fetch_api_post<DataPostRequestBody, DataResponse>(
        query_config.path,
        query_config.body,
        { signal }
      );
    },
    enabled: enable_request
  });

  const component = (() => {
    if (!query.data) {
      return <Placeholder className="h-[400px]">Loading...</Placeholder>;
    } else if (query.data && query.data.length === 0) {
      return <Placeholder className="h-[400px]">Empty response.</Placeholder>;
    } else if (query.data && query.data.length > 0) {
      return (
        <div className="overflow-x-scroll">
          <TablePrimitive
            data={query.data}
            columnIDs={column_ids}
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
        </div>
      );
    }
  })();

  const rows_select = (
    <select
      onChange={(event) => {
        const value = event.target.value;
        set_rows_per_page(Number(value));
      }}
    >
      <option value={25}>25</option>
      <option value={50}>50</option>
      <option value={100}>100</option>
    </select>
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

  const button_class = `disabled:opacity-50 disabled:cursor-not-allowed cursor-pointer ring-1 ring-black/80 rounded-sm px-2 py-1`;

  const prev_next = (
    <div className="flex gap-x-4">
      <button
        onClick={() => set_offset(offset - rows_per_page)}
        disabled={offset === 0}
        className={button_class}
      >
        Previous
      </button>
      <button
        onClick={() => set_offset(offset + rows_per_page)}
        disabled={to >= matching}
        className={button_class}
      >
        Next
      </button>
    </div>
  );

  return (
    <>
      {component}
      <div className="flex justify-between">
        <div>Show {rows_select} rows</div>
        <div>{info_text}</div>
        {prev_next}
      </div>
    </>
  );
}

function TablePrimitive({
  data,
  columnIDs: column_ids,
  onSortChange: on_sort_change,
  isSorted: check_if_sorted
}: {
  data: Array<DataRow>;
  columnIDs: Set<string>;
  onSortChange?: (id: string) => void;
  isSorted?: (id: string) => `asc` | `desc` | false;
}) {
  const columns = useTableColumns(column_ids);

  const table = useReactTable({
    data,
    columns,
    getCoreRowModel: getCoreRowModel()
  });

  const header_groups = table.getHeaderGroups();
  const skip_rendering = new Set();
  return (
    <table>
      <thead>
        {header_groups.map((headerGroup) => (
          <tr key={headerGroup.id}>
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
              const set_sort = () => on_sort_change?.(header.column.id);
              const is_sorted = check_if_sorted?.(header.column.id);
              return (
                <th
                  className={clsx(
                    "border-2 px-2 py-1",
                    is_leaf && `cursor-pointer`
                  )}
                  key={header.id}
                  colSpan={header.colSpan}
                  rowSpan={row_span}
                  onClick={is_leaf ? set_sort : undefined}
                >
                  {
                    <div>
                      {flexRender(
                        header.column.columnDef.header,
                        header.getContext()
                      )}
                      {
                        {
                          asc: (
                            <RadixIcons.TriangleUpIcon className="inline-block" />
                          ),
                          desc: (
                            <RadixIcons.TriangleDownIcon className="inline-block" />
                          )
                        }[is_sorted ? is_sorted : undefined]
                      }
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
          <tr key={row.id} className="odd:bg-gray-100 dark:odd:bg-white/20">
            {row.getVisibleCells().map((cell) => (
              <td
                key={cell.id}
                className="whitespace-nowrap px-2 py-1 text-right"
              >
                {flexRender(cell.column.columnDef.cell, cell.getContext())}
              </td>
            ))}
          </tr>
        ))}
      </tbody>
    </table>
  );
}

/**
 * We have a list of **leaf** fields that we want to display in a table.
 * We need to get their ancestors for the column headers.
 *
 * @param data Table data
 * @param catalog_field_hierarchy The complete field hierarchy for this catalog
 * @returns An array of column definitions for use with react-table
 */
function useTableColumns(column_ids: Set<string>): ColumnDef<DataRow>[] {
  const catalog_id = useCatalogID();
  const catalog_hierarchy = useCatalogMetadata()?.hierarchy;
  if (!catalog_hierarchy) throw new Error(`catalog_field_hierarchy is null`);

  // Recursively construct column definitions
  const next = (
    node: CatalogHierarchyNode
  ): GroupColumnDef<DataRow> | AccessorColumnDef<DataRow> | null => {
    const metadata = node.data;
    // Include this node if:
    // - It is a leaf node, and is one of the fields in field_ids_set
    // - It is the ancestor of one of the fields in field_ids_set
    const is_visible_leaf_field =
      is_leaf_node(node) && column_ids.has(metadata.name);
    const is_ancestor_of_field = node
      .leaves()
      .some((child) => column_ids.has(child.data.name));
    const include = is_visible_leaf_field || is_ancestor_of_field;
    if (!include) return null;

    const child_columns: ColumnDef<DataRow>[] = [];

    for (const child of node.children ?? []) {
      const child_column = next(child);
      if (child_column) {
        child_columns.push(child_column);
      }
    }

    let field_id = metadata.name;

    if (field_id?.length === 0) {
      field_id = metadata.title;
    }

    if (is_root_node(node)) {
      field_id = `root`;
    }

    if (!field_id || field_id.length === 0) {
      console.error(
        `construct_table_columns: field_id is empty for node:`,
        node
      );
      throw new Error(`field_id is empty`);
    }

    const column_base: ColumnDef<DataRow> = {
      id: field_id,
      header: () => <Katex>{metadata.title ?? metadata.name}</Katex>
    };

    if (child_columns.length === 0) {
      const column: AccessorColumnDef<DataRow> = {
        ...column_base,
        accessorFn: (row) => {
          const value = row[field_id];
          const field_type = get_field_type(metadata);
          if (metadata.attachment && value === true) {
            const url = new URL(
              `/api/${catalog_id}/attachment/${field_id}/${row._id}`,
              FLATHUB_API_BASE_URL
            );
            return (
              <a
                className="flex items-center justify-center"
                href={url.toString()}
              >
                <RadixIcons.DownloadIcon />
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
  const root = next(catalog_hierarchy);
  if (root === null) {
    console.error(`construct_table_columns: root is null`);
    return null;
  }
  if (!("columns" in root)) throw new Error(`root.columns is not defined`);
  const columns = root?.columns ?? [];
  return columns;
}
