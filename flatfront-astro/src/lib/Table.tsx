import type {
  Action,
  CatalogHierarchyNode,
  DataPostRequestBody,
  DataResponse,
  DataRow,
  FieldMetadata
} from "./types";
import type {
  AccessorColumnDef,
  ColumnDef,
  GroupColumnDef
} from "@tanstack/react-table";

import React from "react";
import { useQuery } from "@tanstack/react-query";
import {
  flexRender,
  getCoreRowModel,
  useReactTable
} from "@tanstack/react-table";
import {
  fetch_api_post,
  get_field_type,
  hooks,
  is_leaf_node,
  log
} from "./shared";
import { BigButton, CellSection, Placeholder } from "./Primitives";
import Katex from "./Katex";

export default function TableSection() {
  return (
    <CellSection label="table" className="space-y-4">
      <div className="grid">
        <BigButton className="w-full">Select Columns</BigButton>
      </div>
      <Table />
    </CellSection>
  );
}

function Table() {
  const catalog_id = hooks.useCatalogID();

  const filters = hooks.useFilters();

  const catalog_metadata_wrapper = hooks.useCatalogMetadata();
  const catalog_hierarchy = catalog_metadata_wrapper?.hierarchy;
  const cell_actions = hooks.useCellActions();

  const [rows_per_page, set_rows_per_page] = React.useState(25);
  const [offset, set_offset] = React.useState(0);

  const column_ids_set = catalog_hierarchy
    ? get_column_ids(catalog_hierarchy, cell_actions)
    : new Set<string>();

  const fields = Array.from(column_ids_set);

  log(`column_ids_set`, column_ids_set);

  // const query_parameters = {
  //   seed: 243523423
  // }

  const request_body: DataPostRequestBody = {
    object: true,
    fields: fields,
    ...filters,
    count: rows_per_page,
    offset
    // ...query_parameters
  };

  const query_config = {
    path: `/${catalog_id}/data`,
    body: request_body
  };

  const enable_request = !!catalog_id && fields.length > 0;

  const query = useQuery({
    queryKey: [`table-data`, query_config],
    queryFn: async (): Promise<DataResponse> => {
      return fetch_api_post<DataPostRequestBody, DataResponse>(
        query_config.path,
        query_config.body
      ).then((response) => {
        log(`query response`, response);
        return response;
      });
    },
    enabled: enable_request,
    keepPreviousData: true,
    staleTime: Infinity
  });

  if (query.data) {
    log(`query data`, query.data);
  }

  const component =
    query.data && query.data.length > 0 && catalog_hierarchy ? (
      <TablePrimitive data={query.data} />
    ) : query.data && query.data.length === 0 ? (
      <Placeholder className="m-2">Empty Response</Placeholder>
    ) : (
      <Placeholder className="m-2">Loading...</Placeholder>
    );

  return (
    <>
      <div className="max-w-[80dvw] overflow-x-scroll desktop:max-w-none">
        {component}
      </div>
      <div>offset: {offset}</div>
      <button
        onClick={() => {
          set_offset(offset + rows_per_page);
        }}
      >
        add {rows_per_page} to offset
      </button>
    </>
  );
}

function get_column_ids(
  hierarchy: d3.HierarchyNode<FieldMetadata>,
  actions: Action.Any[]
): Set<string> {
  const nodes = hierarchy.descendants();
  const initial_column_ids = nodes
    .filter((node) => node.height === 0 && node.data.disp === true)
    .map((node) => node.data.name);
  const column_ids_set: Set<string> = new Set(initial_column_ids);
  const column_list_actions = actions.filter(
    (action): action is Action.TableColumnAction =>
      action.type === `add_table_column` ||
      action.type === `remove_table_column`
  );
  for (const action of column_list_actions) {
    switch (action.type) {
      case `remove_table_column`:
        column_ids_set.delete(action.field_id);
        break;
      case `add_table_column`:
        column_ids_set.add(action.field_id);
        break;
      // case `remove_child_columns`:
      //   const node = nodes.find(
      //     (node) => get_field_id(node.data) === action.field_id
      //   );
      //   if (!node) {
      //     throw new Error(
      //       `Could not find node for column ${action.field_id} in hierarchy`
      //     );
      //   }
      //   const to_remove = node.leaves().map((node) => get_field_id(node.data));
      //   for (const id of to_remove) {
      //     column_ids_set.delete(id);
      //   }
      //   break;
      default:
        action satisfies never;
    }
  }
  return column_ids_set;
}

function TablePrimitive({ data }: { data: Array<DataRow> }) {
  const catalog_metadata_wrapper = hooks.useCatalogMetadata();
  const catalog_hierarchy = catalog_metadata_wrapper?.hierarchy;

  const columns = construct_table_columns(data, catalog_hierarchy);

  const table = useReactTable({
    data,
    columns,
    getCoreRowModel: getCoreRowModel()
  });

  const header_groups = table.getHeaderGroups();
  const skip_rendering = new Set();
  return (
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
                  className="border-2 px-2 py-1 "
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
function construct_table_columns<T>(
  data: DataResponse,
  catalog_field_hierarchy: CatalogHierarchyNode
): ColumnDef<DataRow>[] {
  if (data.length === 0) return [];
  if (!catalog_field_hierarchy)
    throw new Error(`catalog_field_hierarchy is null`);
  // Get field IDs based on keys in data
  const field_ids_set = new Set(Object.keys(data[0] ?? {}));

  // Recursively construct column definitions
  const next = (
    node: CatalogHierarchyNode
  ): GroupColumnDef<DataRow> | AccessorColumnDef<DataRow> | null => {
    // Include this node if:
    // - It is a leaf node, and is one of the fields in field_ids_set
    // - It is the ancestor of one of the fields in field_ids_set
    const is_field = is_leaf_node(node) && field_ids_set.has(node.data.name);
    const is_ancestor_of_field = node
      .leaves()
      .some((child) => field_ids_set.has(child.data.name));
    const include = is_field || is_ancestor_of_field;
    if (!include) return null;

    const child_columns: ColumnDef<DataRow>[] = [];

    for (const child of node.children ?? []) {
      const child_column = next(child);
      if (child_column) {
        child_columns.push(child_column);
      }
    }

    const field_id = node.data.name;

    const column_base: ColumnDef<DataRow> = {
      id: field_id,
      header: () => <Katex>{node.data.title ?? node.data.name}</Katex>
    };

    if (child_columns.length === 0) {
      const column: AccessorColumnDef<DataRow> = {
        ...column_base,
        accessorFn: (row) => {
          const value = row[field_id];
          const field_type = get_field_type(node.data);
          if (
            field_type === `LABELLED_ENUMERABLE_INTEGER` ||
            field_type === `LABELLED_ENUMERABLE_BOOLEAN`
          ) {
            const index = typeof value === "number" ? value : Number(value);
            const text = node.data.enum[index];
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
  const root = next(catalog_field_hierarchy);
  if (root === null) throw new Error(`root is null`);
  if (!("columns" in root)) throw new Error(`root.columns is not defined`);
  const columns = root?.columns ?? [];
  return columns;
}
