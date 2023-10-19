import React from "react";
import * as RadixIcons from "@radix-ui/react-icons";
import { Dialog } from "./Primitives";
import FieldCard from "./FieldCard";
import { useCatalogMetadata } from "./CatalogMetadata";
import { useCatalogCellID, useCatalogID } from "./CatalogContext";
import {
  type Column,
  type Table,
  type ExpandedState,
  type ColumnDef,
  useReactTable,
  getCoreRowModel,
  getPaginationRowModel,
  getFilteredRowModel,
  getExpandedRowModel,
  flexRender
} from "@tanstack/react-table";
import type { CatalogHierarchyNode } from "../types";
import clsx from "clsx";
import { is_leaf_node, log } from "../shared";
import Katex from "./Katex";
import { useAddFilter, useFilters, useRemoveFilter } from "../filters";

export default function BrowseFieldsDialog() {
  const catalog_id = useCatalogID();
  return (
    <Dialog disabled={!catalog_id} label="Browse Fields" className="p-8">
      <FieldsTable />
    </Dialog>
  );
}

function FieldsTable() {
  const catalog_metadata = useCatalogMetadata();
  const root_node_children = catalog_metadata?.hierarchy?.children ?? [];
  const columns: ColumnDef<CatalogHierarchyNode, any>[] = [
    {
      header: `Field`,
      accessorFn: (node: CatalogHierarchyNode) => node.data.title,
      cell: ({ row, getValue }) => {
        const icon_class = `h-4 w-4`;
        const can_expand = row.getCanExpand();
        const expand_handler = row.getToggleExpandedHandler();
        const expand_button = (
          <button className="cursor-pointer">
            {row.getIsExpanded() ? (
              <RadixIcons.ChevronDownIcon className={icon_class} />
            ) : (
              <RadixIcons.ChevronRightIcon className={icon_class} />
            )}
          </button>
        );
        const placeholder = <div className={icon_class} />;
        return (
          <div
            style={
              {
                "--depth": row.depth
              } as React.CSSProperties
            }
            onClick={can_expand ? expand_handler : undefined}
            className={clsx(
              "flex items-center pl-[calc(var(--depth)*1rem)]",
              can_expand && "cursor-pointer"
            )}
          >
            {can_expand ? expand_button : placeholder}
            <Katex>{getValue()}</Katex>
          </div>
        );
      }
    },
    {
      header: `Variable`,
      accessorFn: (node: CatalogHierarchyNode) => node.data.name
    },
    {
      header: `Units`,
      accessorFn: (node: CatalogHierarchyNode) => node.data.units,
      cell: ({ getValue }) => <Katex>{getValue()}</Katex>
    },
    {
      header: `Filter`,
      accessorFn: (node: CatalogHierarchyNode) => node,
      cell: ({ getValue }) => <AddRemoveFilterButton node={getValue()} />
    }
  ];

  const [expanded, set_expanded] = React.useState<ExpandedState>({});

  const table = useReactTable({
    data: root_node_children,
    columns,
    state: {
      expanded
    },
    getCoreRowModel: getCoreRowModel(),
    getExpandedRowModel: getExpandedRowModel(),
    onExpandedChange: set_expanded,
    getSubRows: (row) => row.children
  });

  const expand_all_button = (() => {
    const is_all_expanded = table.getIsAllRowsExpanded();
    const expand_all_handler = table.getToggleAllRowsExpandedHandler();
    return (
      <button
        className="-translate-y-1 cursor-pointer underline decoration-black decoration-dashed dark:decoration-white"
        onClick={expand_all_handler}
      >
        {is_all_expanded ? `Collapse All` : `Expand All`}
      </button>
    );
  })();

  return (
    <>
      {expand_all_button}
      <div className="max-h-[80dvh] w-[min(80dvw,800px)] overflow-x-scroll overflow-y-scroll">
        <table className="w-full">
          <thead className="sticky top-0 h-10 -translate-y-px bg-white dark:bg-black">
            <tr>
              {table.getLeafHeaders().map((header) => (
                <th key={header.id} className="relative text-left">
                  {flexRender(
                    header.column.columnDef.header,
                    header.getContext()
                  )}
                  <div className="absolute bottom-0 w-full border-b border-b-black dark:border-b-white"></div>
                </th>
              ))}
            </tr>
          </thead>
          <tbody>
            {table.getRowModel().rows.map((row) => {
              return (
                <tr
                  key={row.id}
                  className="odd:bg-gray-100 dark:odd:bg-white/20"
                >
                  {row.getVisibleCells().map((cell) => {
                    return (
                      <td key={cell.id} className="pe-2">
                        {flexRender(
                          cell.column.columnDef.cell,
                          cell.getContext()
                        )}
                      </td>
                    );
                  })}
                </tr>
              );
            })}
          </tbody>
        </table>
      </div>
    </>
  );
}

function AddRemoveFilterButton({ node }: { node: CatalogHierarchyNode }) {
  const metadata = node.data;
  const is_leaf = is_leaf_node(node);
  const can_remove = metadata.required !== true;
  const is_active_filter = useFilters()[metadata.name] !== undefined;
  const remove_filter = useRemoveFilter();
  const add_filter = useAddFilter();

  if (!is_leaf) return null;
  if (!can_remove) return null;
  const on_click = is_active_filter
    ? () => remove_filter(node)
    : () => add_filter(node);
  const text = is_active_filter ? `Remove` : `Add`;
  return (
    <button className="underline" onClick={on_click}>
      {text}
    </button>
  );
}

function FieldCards() {
  const catalog_metadata = useCatalogMetadata();
  const all_field_nodes = catalog_metadata?.depth_first ?? [];
  return (
    <>
      {all_field_nodes.map((node) => (
        <FieldCard fieldNode={node} key={node.data.__hash} />
      ))}
    </>
  );
}
