import type { CatalogHierarchyNode } from "../types";
import React from "react";
import clsx from "clsx";
import * as RadixIcons from "@radix-ui/react-icons";
import {
  type ExpandedState,
  type ColumnDef,
  useReactTable,
  getCoreRowModel,
  getExpandedRowModel,
  flexRender
} from "@tanstack/react-table";
import { is_leaf_node } from "../shared";
import {
  useAddColumn,
  useColumns,
  useRemoveColumn
} from "../contexts/ColumnsContext";
import { useCatalogMetadata } from "../contexts/CatalogMetadataContext";
import { useCatalogID } from "../contexts/CatalogContext";
import { Dialog } from "./Primitives";
import Katex from "./Katex";
import AddRemoveFilterButton from "./AddRemoveFilterButton";

export default function BrowseFieldsDialog({
  label = `Browse Fields`
}: {
  label?: string;
}) {
  const catalog_id = useCatalogID();
  return (
    <Dialog disabled={!catalog_id} label={label} className="p-8">
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
      header: `Units`,
      accessorFn: (node: CatalogHierarchyNode) => node.data.units,
      cell: ({ getValue }) => <Katex>{getValue()}</Katex>
    },
    {
      header: `Description`,
      accessorFn: (node: CatalogHierarchyNode) => node.data.descr,
      cell: ({ getValue }) => <div className="max-w-[30cqi]">{getValue()}</div>
    },
    {
      header: `Filter`,
      accessorFn: (node: CatalogHierarchyNode) => node,
      cell: ({ getValue }) => <AddRemoveFilterButton node={getValue()} />
    },
    {
      header: `Column`,
      accessorFn: (node: CatalogHierarchyNode) => node,
      cell: ({ getValue }) => <AddRemoveColumnButton node={getValue()} />
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
      <div className="max-h-[80dvh] w-[min(80dvw,800px)] overflow-x-scroll overflow-y-scroll @container">
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
                      <td key={cell.id} className="pb-2 pe-2 align-top">
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

function AddRemoveColumnButton({ node }: { node: CatalogHierarchyNode }) {
  const metadata = node.data;
  const is_leaf = is_leaf_node(node);
  const is_active_column = useColumns().has(metadata.name);
  const remove_column = useRemoveColumn();
  const add_column = useAddColumn();
  if (!is_leaf) return null;
  const on_click = is_active_column
    ? () => remove_column(node)
    : () => add_column(node);
  const text = is_active_column ? `Remove` : `Add`;
  return (
    <button className="underline" onClick={on_click}>
      {text}
    </button>
  );
}
