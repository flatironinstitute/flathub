import type { CatalogHierarchyNode } from "@/types";
import React from "react";
import clsx from "clsx";
import { ChevronDown, ChevronRight } from "lucide-react";
import {
  type ExpandedState,
  type ColumnDef,
  type Row,
  useReactTable,
  getCoreRowModel,
  getExpandedRowModel,
  flexRender,
  type RowSelectionState,
  type Table as TableType,
  getFilteredRowModel
} from "@tanstack/react-table";
import { ScrollArea } from "@/components/ui/scroll-area";
import { Checkbox } from "@/components/ui/checkbox";
import { Katex } from "@/components/ui/katex";
import { Button } from "@/components/ui/button";
import { Input } from "@/components/ui/input";
import {
  Table,
  TableBody,
  TableCell,
  TableHeader,
  TableRow
} from "@/components/ui/table";
import { Badge } from "@/components/ui/badge";
import { useCatalogMetadata } from "@/components/contexts/CatalogMetadataContext";
import {
  useColumns,
  useSetColumns
} from "@/components/contexts/ColumnsContext";
import { log } from "@/utils";

export function FieldsBrowser() {
  const catalog_metadata = useCatalogMetadata();
  const root_node_children = catalog_metadata?.hierarchy?.children ?? [];

  const table_columns: ColumnDef<CatalogHierarchyNode, any>[] = [
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
            <Katex>{getValue()}</Katex>
          </div>
        );
      }
    },
    {
      header: `Filter Toggle`,
      id: `filter-toggle`,
      cell: ({ row }) => <FilterCheckbox row={row} />
    },
    {
      header: `Description`,
      accessorFn: (node: CatalogHierarchyNode) => node.data.descr,
      cell: ({ getValue }) => (
        <div className="flex">
          <span className="max-w-[37cqi] truncate">{getValue()}</span>
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
  const [search_string, set_search_string] = React.useState("");

  const table = useReactTable({
    data: root_node_children,
    columns: table_columns,
    state: {
      expanded,
      globalFilter: search_string
    },
    getRowId: (row) => catalog_metadata?.get_hash_from_node(row),
    getSubRows: (row) => row.children,
    getCoreRowModel: getCoreRowModel(),
    getFilteredRowModel: getFilteredRowModel(),
    getExpandedRowModel: getExpandedRowModel(),
    onGlobalFilterChange: set_search_string,
    onExpandedChange: set_expanded,
    globalFilterFn: `includesString`,
    filterFromLeafRows: true,
    autoResetExpanded: true
  });

  const is_all_expanded = table.getIsAllRowsExpanded();
  const expand_all_handler = table.getToggleAllRowsExpandedHandler();

  React.useEffect(() => {
    if (search_string.length === 0) return;
    if (!is_all_expanded) table.toggleAllRowsExpanded(true);
  }, [search_string]);

  return (
    <div className="space-y-4">
      <div className="flex gap-x-4">
        <DebouncedInput
          value={search_string ?? ""}
          onChange={(value) => table.setGlobalFilter(String(value))}
          className="max-w-[40ch]"
          placeholder="Search..."
        />
        <Button variant="outline" onClick={expand_all_handler}>
          {is_all_expanded ? `Collapse All` : `Expand All`}
        </Button>
      </div>
      <ScrollArea
        className="w-full rounded-md border p-4"
        viewportClassName="max-h-[400px] @container/scrollarea"
      >
        <TablePrimitive table={table} />
      </ScrollArea>
    </div>
  );
}

function DebouncedInput({
  value: initialValue,
  onChange,
  debounce = 500,
  ...props
}: {
  value: string | number;
  onChange: (value: string | number) => void;
  debounce?: number;
} & Omit<React.InputHTMLAttributes<HTMLInputElement>, "onChange">) {
  const [value, setValue] = React.useState(initialValue);

  React.useEffect(() => {
    setValue(initialValue);
  }, [initialValue]);

  React.useEffect(() => {
    const timeout = setTimeout(() => {
      onChange(value);
    }, debounce);

    return () => clearTimeout(timeout);
  }, [value]);

  return (
    <Input
      {...props}
      value={value}
      onChange={(e) => setValue(e.target.value)}
    />
  );
}

function TablePrimitive({ table }: { table: TableType<CatalogHierarchyNode> }) {
  return (
    <Table>
      <TableBody>
        {table.getRowModel().rows.map((row) => (
          <TableRow key={row.id}>
            {row.getVisibleCells().map((cell) => (
              <TableCell key={cell.id} className="px-2 py-1">
                {flexRender(cell.column.columnDef.cell, cell.getContext())}
              </TableCell>
            ))}
          </TableRow>
        ))}
      </TableBody>
    </Table>
  );
}

// function TablePrimitive({ table }: { table: Table<CatalogHierarchyNode> }) {
//   return (
//     <div className="grid w-[100cqi] grid-cols-[min-content_1fr_min-content] gap-x-4">
//       {table.getRowModel().rows.map((row) => {
//         return (
//           <React.Fragment key={row.id}>
//             {row.getVisibleCells().map((cell) => {
//               return (
//                 <div key={cell.id}>
//                   {flexRender(cell.column.columnDef.cell, cell.getContext())}
//                 </div>
//               );
//             })}
//           </React.Fragment>
//         );
//       })}
//     </div>
//   );
// }

// function TablePrimitive({ table }: { table: Table<CatalogHierarchyNode> }) {
//   return (
//     <table className="w-full">
//       <tbody>
//         {table.getRowModel().rows.map((row) => {
//           return (
//             <tr key={row.id} className="odd:bg-gray-100 dark:odd:bg-white/20">
//               {row.getVisibleCells().map((cell) => {
//                 return (
//                   <td key={cell.id} className="px-2">
//                     {flexRender(cell.column.columnDef.cell, cell.getContext())}
//                   </td>
//                 );
//               })}
//             </tr>
//           );
//         })}
//       </tbody>
//     </table>
//   );
// }

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
  const columns = useColumns();
  const set_columns = useSetColumns();
  const leaves = row.getLeafRows().filter((d) => d.subRows.length === 0);
  const leaves_selected = (() => {
    if (leaves.length === 0) return `none`;
    const num_selected = leaves.filter((leaf) => columns.has(leaf.id)).length;
    if (num_selected === leaves.length) return `all`;
    if (num_selected > 0) return `some`;
    return `none`;
  })();
  const checked = is_leaf ? columns.has(row.id) : leaves_selected === `all`;
  const indeterminate = leaves_selected === `some`;
  const toggle_handler = () => {
    if (is_leaf) {
      set_columns({
        [row.id]: checked ? false : true
      });
    } else {
      const value = leaves_selected === `all` ? false : true;
      const leaf_ids = leaves.map((d) => d.id);
      set_columns(Object.fromEntries(leaf_ids.map((d) => [d, value])));
    }
  };
  return (
    <Checkbox
      checked={indeterminate ? `indeterminate` : checked}
      onCheckedChange={toggle_handler}
    />
  );
}

function FilterCheckbox({ row }: { row: Row<CatalogHierarchyNode> }) {
  return (
    <Badge variant="outline" className="cursor-pointer">
      Filter
    </Badge>
  );
}
