import type {
  DataResponse,
  CatalogHierarchyNode,
  FieldMetadata,
  Datum,
} from "../types";

import type {
  ColumnDef,
  GroupColumnDef,
  AccessorColumnDef,
} from "@tanstack/react-table";

import {
  flexRender,
  getCoreRowModel,
  getPaginationRowModel,
  useReactTable,
} from "@tanstack/react-table";

import Katex from "./Katex";
import { field_is_enum, get_field_id, log } from "../shared";

export default function Table({
  data,
  catalog_field_hierarchy,
}: {
  data: DataResponse;
  catalog_field_hierarchy: d3.HierarchyNode<FieldMetadata>;
}) {
  const columns = construct_table_columns(data, catalog_field_hierarchy);

  const table = useReactTable({
    data,
    columns,
    getCoreRowModel: getCoreRowModel(),
    // getPaginationRowModel: getPaginationRowModel(),
  });

  const header_groups = table.getHeaderGroups();

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
                    className="py-1 px-2 dark:bg-dark-2 border-2 dark:border-dark-5"
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
                <td
                  key={cell.id}
                  className="whitespace-nowrap text-right py-1 px-2"
                >
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
): ColumnDef<Datum>[] {
  if (data.length === 0) return [];
  // Get field IDs based on keys in data
  const field_ids_set = new Set(Object.keys(data[0] ?? {}));

  // Recursively construct column definitions
  const next = (
    node: CatalogHierarchyNode
  ): GroupColumnDef<Datum> | AccessorColumnDef<Datum> | null => {
    // Include this node if:
    // - It is one of the fields in field_ids_set
    // - It is the ancestor of one of the fields in field_ids_set
    const is_field = field_ids_set.has(get_field_id(node.data));
    const is_ancestor_of_field = node
      .leaves()
      .some((child) => field_ids_set.has(get_field_id(child.data)));
    const include = is_field || is_ancestor_of_field;
    if (!include) return null;

    const child_columns: ColumnDef<Datum>[] = [];

    for (const child of node.children ?? []) {
      const child_column = next(child);
      if (child_column) {
        child_columns.push(child_column);
      }
    }

    const field_id = get_field_id(node.data);

    const column_base: ColumnDef<Datum> = {
      id: field_id,
      header: () => <Katex>{node.data.title ?? node.data.name}</Katex>,
    };

    if (child_columns.length === 0) {
      const column: AccessorColumnDef<Datum> = {
        ...column_base,
        // accessorKey: get_field_id(node.data),
        accessorFn: (row) => {
          const value = row[field_id];
          if (field_is_enum(node.data)) {
            const index = typeof value === "number" ? value : Number(value);
            const text = node.data.enum[index];
            return text;
          }
          return value;
        },
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

  return columns;
}
