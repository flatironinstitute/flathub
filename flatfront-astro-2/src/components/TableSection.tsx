import React from "react";
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
import type {
  CatalogHierarchyNode,
  CatalogMetadataWrapper,
  DataPostRequestBody,
  DataResponse,
  DataRow
} from "@/types";
import { useQuery } from "@tanstack/react-query";
import {
  FLATHUB_API_BASE_URL,
  fetch_api_post,
  get_field_type,
  is_leaf_node,
  is_root_node
} from "@/utils";
import { ScrollArea } from "./ui/scroll-area";
import { Download, Info } from "lucide-react";
import { Alert, AlertDescription, AlertTitle } from "./ui/alert";
import type {
  AccessorColumnDef,
  ColumnDef,
  GroupColumnDef
} from "@tanstack/react-table";
import { Katex } from "./ui/katex";
import { useCatalogMetadata } from "./contexts/CatalogMetadataContext";

export function TableSection() {
  return (
    <SortProvider>
      <div className="space-y-4">
        <Table />
        {/* <DownloadSection /> */}
      </div>
    </SortProvider>
  );
}

function Table() {
  const catalog_id = useCatalogID();

  const column_names = useColumnNames();

  const filters = useFilterValuesWithFieldNames();
  const random_config = useRandomConfig();

  const [rows_per_page, set_rows_per_page] = React.useState<number>(25);
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

  const enable_request = !!catalog_id && column_names.size > 0;

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

  const content =
    query.data && query.data.length > 0 ? <TablePrimitive /> : <StatusBox />;

  return (
    <ScrollArea className="w-full rounded-md border p-4">{content}</ScrollArea>
  );
}

function TablePrimitive() {
  const column_ids = useColumnIDs();
  const catalog_metadata = useCatalogMetadata();
  return <div>table</div>;
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
    const metadata = node.data;
    const field_id = catalog_metadata.get_id_from_node(node);
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

function StatusBox() {
  const title = `No Data`;
  const description = `There is no data to display.`;
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
