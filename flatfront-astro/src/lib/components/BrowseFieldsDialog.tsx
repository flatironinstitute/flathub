import type { CatalogHierarchyNode } from "../types";
import React from "react";
import clsx from "clsx";
import * as RadixIcons from "@radix-ui/react-icons";
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
import { is_leaf_node, log } from "../shared";
import {
  useAddAllLeafColumns,
  useAddColumn,
  useColumns,
  useRemoveAllLeafColumns,
  useRemoveColumn
} from "../contexts/ColumnsContext";
import { useCatalogMetadata } from "../contexts/CatalogMetadataContext";
import { useCatalogID } from "../contexts/CatalogContext";
import { Dialog } from "./Primitives";
import Katex from "./Katex";
import AddRemoveFilterButton from "./AddRemoveFilterButton";

export default function BrowseFieldsDialog({
  label = `Browse Fields`,
  className
}: {
  label?: string;
  className?: string;
}) {
  const catalog_id = useCatalogID();
  return (
    <Dialog
      disabled={!catalog_id}
      label={label}
      buttonClassName={clsx("p-8", className)}
    >
      <FieldsTable />
    </Dialog>
  );
}

export function FieldsTable() {
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
            className={clsx("flex items-center pl-[calc(var(--depth)*1rem)]")}
          >
            <RowExpandButton row={row} />
            &ensp;
            <ColumnCheckbox row={row} />
            &ensp;
            <Katex>{getValue()}</Katex>
          </div>
        );
      }
    }
    // {
    //   header: `Units`,
    //   accessorFn: (node: CatalogHierarchyNode) => node.data.units,
    //   cell: ({ getValue }) => <Katex>{getValue()}</Katex>
    // },
    // {
    //   header: `Description`,
    //   accessorFn: (node: CatalogHierarchyNode) => node.data.descr,
    //   cell: ({ getValue }) => <div className="max-w-[30cqi]">{getValue()}</div>
    // },
    // {
    //   header: `Filter`,
    //   accessorFn: (node: CatalogHierarchyNode) => node,
    //   cell: ({ getValue }) => <AddRemoveFilterButton node={getValue()} />
    // },
    // {
    //   header: `Column`,
    //   accessorFn: (node: CatalogHierarchyNode) => node,
    //   cell: ({ getValue }) => <AddRemoveColumnButton node={getValue()} />
    // }
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
    getRowId: (row, index, parent) => catalog_metadata?.get_hash_from_node(row),
    getSubRows: (row) => row.children,
    getCoreRowModel: getCoreRowModel(),
    getExpandedRowModel: getExpandedRowModel(),
    onExpandedChange: set_expanded,
    onRowSelectionChange: set_selected
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
    <div>
      {expand_all_button}
      <div className="max-h-[500px] w-full overflow-x-scroll overflow-y-scroll @container">
        <table className="w-full">
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
    </div>
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
        <RadixIcons.ChevronDownIcon className={icon_class} />
      ) : (
        <RadixIcons.ChevronRightIcon className={icon_class} />
      )}
    </button>
  );
  const placeholder = <div className={icon_class} />;
  return can_expand ? expand_button : placeholder;
}

function ColumnCheckbox({ row }: { row: Row<CatalogHierarchyNode> }) {
  const node = row.original;
  // const metadata = node.data;
  const is_leaf = row.subRows.length === 0;
  // const is_leaf = is_leaf_node(node);
  const active_columns = useColumns();
  const remove_column = useRemoveColumn();
  const add_column = useAddColumn();
  const add_all_leaf_columns = useAddAllLeafColumns();
  const remove_all_leaf_columns = useRemoveAllLeafColumns();
  const is_active_column = active_columns.has(row.original.data.name);
  const leaves = node.leaves();
  const num_active_leaves = leaves.filter((leaf) =>
    active_columns.has(leaf.data.name)
  ).length;
  const all_leaves_active = num_active_leaves === leaves.length;
  const checked = (() => {
    if (is_leaf) return is_active_column;
    if (all_leaves_active) return true;
    return false;
  })();

  const indeterminate = (() => {
    if (is_leaf) return false;
    if (num_active_leaves === 0) return false;
    if (all_leaves_active) return false;
    return true;
  })();

  const ref = React.useRef<HTMLInputElement>(null);

  React.useEffect(() => {
    ref.current.indeterminate = indeterminate;
  }, [ref, indeterminate]);
  // const on_change = () => {
  //   // if (!is_leaf) return;
  //   // if (is_active_column) remove_column(node);
  //   // else add_column(node);
  //   return row.getToggleSelectedHandler()();
  // };
  const on_change = (() => {
    if (!is_leaf)
      return () => {
        if (all_leaves_active) remove_all_leaf_columns(node);
        else add_all_leaf_columns(node);
      };
    if (is_active_column) return () => remove_column(node);
    return () => add_column(node);
  })();
  // log(`barney`, node);
  return (
    <input
      type="checkbox"
      className="cursor-pointer"
      checked={checked}
      onChange={on_change}
      ref={ref}
    />
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
