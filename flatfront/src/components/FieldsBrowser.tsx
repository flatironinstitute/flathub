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
  type Table as TableType,
  getFilteredRowModel,
  createColumnHelper
} from "@tanstack/react-table";
import { useCatalogMetadata } from "@/components/contexts/CatalogMetadataContext";
import {
  useAddColumn,
  useColumnIDs,
  useSetColumns
} from "@/components/contexts/ColumnsContext";
import {
  useAddFilter,
  useFilterIDs,
  useRemoveFilter
} from "@/components/contexts/FiltersContext";
import { ScrollArea } from "@/components/ui/scroll-area";
import { Checkbox } from "@/components/ui/checkbox";
import { Katex } from "@/components/ui/katex";
import { Button } from "@/components/ui/button";
import { Input } from "@/components/ui/input";
import { Table, TableBody, TableCell, TableRow } from "@/components/ui/table";
import { Badge } from "@/components/ui/badge";
import { FieldInfoDialog } from "./FieldInfoDialog";

const column_helper = createColumnHelper<CatalogHierarchyNode>();

export function FieldsBrowser() {
  const catalog_metadata = useCatalogMetadata();
  const root_node_children = catalog_metadata?.hierarchy?.children ?? [];

  const table_columns: ColumnDef<CatalogHierarchyNode, any>[] = [
    column_helper.accessor((node: CatalogHierarchyNode) => node.data.title, {
      header: `Field`,
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
            <FieldInfoDialog row={row}>
              <Katex className="cursor-pointer">{getValue()}</Katex>
            </FieldInfoDialog>
          </div>
        );
      }
    }),
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
          <span className="truncate">{getValue()}</span>
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
    getRowId: (row) => catalog_metadata?.get_id_from_node(row),
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
      <div className="flex flex-col gap-4 @md/cell:flex-row">
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
    <Table
      className="grid w-full gap-x-2"
      style={{
        gridTemplateColumns: `repeat(2,min-content) minmax(0,1fr) min-content`
      }}
    >
      <TableBody className="contents">
        {table.getRowModel().rows.map((row) => (
          <TableRow key={row.id} className="contents">
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

// function TablePrimitive({ table }: { table: TableType<CatalogHierarchyNode> }) {
//   return (
//     <Table>
//       <TableBody>
//         {table.getRowModel().rows.map((row) => (
//           <TableRow key={row.id}>
//             {row.getVisibleCells().map((cell) => (
//               <TableCell key={cell.id} className="px-2 py-1">
//                 {flexRender(cell.column.columnDef.cell, cell.getContext())}
//               </TableCell>
//             ))}
//           </TableRow>
//         ))}
//       </TableBody>
//     </Table>
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
  const column_ids = useColumnIDs();
  const set_columns = useSetColumns();
  const leaves = row.getLeafRows().filter((d) => d.subRows.length === 0);
  const leaves_selected = (() => {
    if (leaves.length === 0) return `none`;
    const num_selected = leaves.filter((leaf) =>
      column_ids.has(leaf.id)
    ).length;
    if (num_selected === leaves.length) return `all`;
    if (num_selected > 0) return `some`;
    return `none`;
  })();
  const checked = is_leaf ? column_ids.has(row.id) : leaves_selected === `all`;
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
  const is_leaf = row.subRows.length === 0;

  const node = row.original;
  const field_id = useCatalogMetadata()?.get_id_from_node(node);
  const metadata = node.data;
  const is_required = metadata.required === true;
  const can_remove = !is_required;
  const add_column = useAddColumn();
  const add_filter = useAddFilter();
  const remove_filter = useRemoveFilter();

  const filter_ids = useFilterIDs();
  const is_active = filter_ids.has(field_id);

  const on_click = () => {
    if (is_active && can_remove) {
      remove_filter(node);
    } else {
      add_column(field_id);
      add_filter(node);
    }
  };

  const label = is_required
    ? `Required Filter`
    : is_active
      ? `Remove Filter`
      : `Add Filter`;

  return is_leaf ? (
    <Badge
      variant="outline"
      aria-disabled={is_required}
      className="cursor-pointer whitespace-nowrap rounded-sm aria-disabled:cursor-not-allowed aria-disabled:opacity-50"
      onClick={on_click}
    >
      {label}
    </Badge>
  ) : null;
}
