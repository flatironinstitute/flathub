import React from "react";
import {
  type ExpandedState,
  type ColumnDef,
  type Row,
  useReactTable,
  getCoreRowModel,
  getExpandedRowModel,
  flexRender,
  type RowSelectionState
} from "@tanstack/react-table";
import { ScrollArea } from "@/components/ui/scroll-area";
import { Checkbox } from "@/components/ui/checkbox";
import { useCatalogMetadata } from "@/components/contexts/CatalogMetadataContext";
import { Katex } from "./ui/katex";
import clsx from "clsx";
import type { CatalogHierarchyNode } from "@/types";
import { ChevronDown, ChevronRight } from "lucide-react";
import { log } from "@/utils";

export function FieldsBrowser() {
  return (
    <div className="space-y-4">
      <div>button</div>
      <ScrollArea className="h-[500px] w-full rounded-md border p-4">
        <FieldsTable />
      </ScrollArea>
    </div>
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
        const style = {
          "--depth": row.depth
        } as React.CSSProperties;
        return (
          <div
            style={style}
            className={clsx(
              `flex items-center whitespace-nowrap pl-[calc(var(--depth)*1rem)]`
            )}
          >
            <RowExpandButton row={row} />
            &ensp;
            <ColumnCheckbox row={row} />
            &ensp;
            <Katex className="pr-4">{getValue()}</Katex>
          </div>
        );
      }
    },
    {
      header: `Description`,
      accessorFn: (node: CatalogHierarchyNode) => node.data.descr,
      cell: ({ getValue }) => (
        <div className="max-w-[20cqi] overflow-hidden text-ellipsis whitespace-nowrap">
          {getValue()}
        </div>
      )
    },
    {
      header: `Units`,
      accessorFn: (node: CatalogHierarchyNode) => node.data.units,
      cell: ({ getValue }) => (
        <Katex className="whitespace-nowrap">{getValue()}</Katex>
      )
    }
  ];

  const [expanded, set_expanded] = React.useState<ExpandedState>({});
  const [selected, set_selected] = React.useState<RowSelectionState>({});

  const table = useReactTable({
    data: root_node_children,
    columns,
    state: {
      expanded,
      rowSelection: selected
    },
    getRowId: (row) => catalog_metadata?.get_hash_from_node(row),
    getSubRows: (row) => row.children,
    getCoreRowModel: getCoreRowModel(),
    getExpandedRowModel: getExpandedRowModel(),
    onExpandedChange: set_expanded,
    onRowSelectionChange: set_selected
  });

  return (
    <table className="@container/table w-full">
      <tbody>
        {table.getRowModel().rows.map((row) => {
          return (
            <tr key={row.id} className="odd:bg-gray-100 dark:odd:bg-white/20">
              {row.getVisibleCells().map((cell) => {
                return (
                  <td key={cell.id}>
                    {flexRender(cell.column.columnDef.cell, cell.getContext())}
                  </td>
                );
              })}
            </tr>
          );
        })}
      </tbody>
    </table>
  );
}

function RowExpandButton<T>({ row }: { row: Row<T> }) {
  const icon_class = `h-4 w-4`;
  const can_expand = row.getCanExpand();
  const expand_handler = row.getToggleExpandedHandler();
  const expand_button = (
    <button
      className="cursor-pointer"
      onClick={can_expand ? expand_handler : undefined}
    >
      {row.getIsExpanded() ? (
        <ChevronDown className={icon_class} />
      ) : (
        <ChevronRight className={icon_class} />
      )}
    </button>
  );
  const placeholder = <div className={icon_class} />;
  return can_expand ? expand_button : placeholder;
}

function ColumnCheckbox({ row }: { row: Row<CatalogHierarchyNode> }) {
  const is_leaf = row.subRows.length === 0;
  const leaves_selected = get_leaves_selected(row);
  const checked = is_leaf ? row.getIsSelected() : leaves_selected === `all`;
  const indeterminate = leaves_selected === `some`;
  const toggle_handler = is_leaf
    ? row.getToggleSelectedHandler()
    : toggle_children(row, leaves_selected === `all` ? false : true);
  return (
    <Checkbox
      checked={indeterminate ? `indeterminate` : checked}
      onCheckedChange={toggle_handler}
    />
  );
}

function get_leaves_selected(
  row: Row<CatalogHierarchyNode>
): `all` | `some` | `none` {
  const leaves = row.getLeafRows().filter((d) => d.subRows.length === 0);
  if (leaves.length === 0) return `none`;
  const num_selected = leaves.filter((leaf) => leaf.getIsSelected()).length;
  if (num_selected === leaves.length) return `all`;
  if (num_selected > 0) return `some`;
  return `none`;
}

function toggle_children(row: Row<CatalogHierarchyNode>, value: boolean) {
  const leaves = row.getLeafRows().filter((d) => d.subRows.length === 0);
  return () => {
    for (const leaf of leaves) {
      leaf.toggleSelected(value);
    }
    // const is_selected = row.getIsSelected();
    // const children = row.getVisibleChildren();
    // children.forEach((child) => {
    //   child.getToggleSelectedHandler()(!is_selected);
    // });
  };
}
