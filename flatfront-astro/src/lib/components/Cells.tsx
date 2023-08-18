import type {
  CatalogCellID,
  DataRequestBody,
  DataResponse,
  FilterCellID,
  FilterListAction,
  Filters,
  TableCellID,
  PlotCellID,
} from "../types";
import type { QueryObserver, QueryObserverResult } from "@tanstack/query-core";

import React from "react";

import clsx from "clsx";

import * as Dialog from "@radix-ui/react-dialog";
import { TrashIcon } from "@radix-ui/react-icons";
import * as d3 from "d3";
import { Cross1Icon } from "@radix-ui/react-icons";

import {
  BigButton,
  CellWrapper,
  create_query_observer,
  dispatch_action,
  fetch_api_get,
  fetch_api_post,
  hooks,
  is_catalog_cell_id,
  is_filter_cell_id,
  log,
  Providers,
  get_field_id,
  get_catalog_hierarchy,
  get_column_ids,
  get_filter_ids,
  get_initial_cell_filters,
  get_final_filters,
  wrap_catalog_response,
  get_catalog_id,
  get_nodes_depth_first,
  is_table_cell_id,
  is_plot_cell_id,
} from "../shared";
import * as stores from "../stores";
import Table from "./Table";
import { FieldCard, FilterCard, ColumnCard } from "./FieldCard";

export default function Cells() {
  const cells = hooks.useStore(stores.cells).map((d) => d.data);

  return (
    <>
      {cells.map((cell) => {
        if (cell.type === `root`) return null;
        const component = (() => {
          switch (cell.type) {
            case `catalog`:
              return <CatalogCell />;
            case `filter`:
              return <FilterCell />;
            case `table`:
              return <TableCell />;
            case `plot`:
              return <PlotCell />;
            default:
              cell satisfies never;
          }
        })();
        return (
          <Providers.CellProvider key={cell.cell_id} value={cell}>
            {component}
          </Providers.CellProvider>
        );
      })}
    </>
  );
}

// function CatalogDataProviders({
//   children,
// }: {
//   children: React.ReactNode;
// }): React.JSX.Element {
//   const cell_id = hooks.useCellID();

//   const catalog_id = get_catalog_id(cells_hierarchy, cell_id);

//   const catalog_metadata_query_observer = hooks
//     .useStore(stores.catalog_metadata_query_observer_by_catalog_id)
//     .get(catalog_id);

//   const catalog_metadata_query = useQueryObserver(
//     catalog_metadata_query_observer
//   );

//   return <>{children}</>;
// }

function CatalogCell() {
  const cell_id = hooks.useCell().cell_id;

  const catalog_id = hooks.useStore(stores.catalog_id_by_cell_id).get(cell_id);

  const catalog_metadata_query_observer = hooks
    .useStore(stores.catalog_metadata_query_observer_by_catalog_id)
    .get(catalog_id);

  const catalog_metadata_query = useQueryObserver(
    catalog_metadata_query_observer
  );

  const catalog_metadata = catalog_metadata_query?.data ?? null;

  const catalog_title = catalog_metadata?.title;

  return (
    <CellWrapper>
      <CellTitle subtitle={cell_id}>{catalog_title} Catalog</CellTitle>
      <CellSection label="actions">
        <BigButton
          onClick={() => {
            dispatch_action({
              type: `add_filter_cell`,
              cell_id: `filter_cell_${Date.now()}`,
              parent_cell_id: cell_id as CatalogCellID,
            });
          }}
        >
          Add Filters
        </BigButton>
      </CellSection>
    </CellWrapper>
  );
}

function FilterCell() {
  const cell = hooks.useCell();
  const cell_id = is_filter_cell_id(cell.cell_id);
  const parent_cell_id: CatalogCellID = is_catalog_cell_id(cell.parent_cell_id);
  const catalog_id = hooks.useStore(stores.catalog_id_by_cell_id).get(cell_id);

  const catalog_metadata_wrapper = hooks
    .useStore(stores.catalog_metadata_wrapper_by_catalog_id)
    .get(catalog_id);

  const catalog_title = catalog_metadata_wrapper?.metadata?.title;

  return (
    <CellWrapper>
      <CellTitle subtitle={cell_id}>{catalog_title} Filters</CellTitle>
      <CellSection label="source" small>
        {parent_cell_id}
      </CellSection>
      <CellSection label="filters">
        {catalog_metadata_wrapper ? (
          <CellFiltersSection />
        ) : (
          <PendingBox>Loading Catalog Metadata...</PendingBox>
        )}
      </CellSection>
      <CellSection label="actions">
        <div className="flex flex-col gap-y-4">
          <BigButton
            onClick={() => {
              dispatch_action({
                type: `add_table_cell`,
                cell_id: `table_cell_${Date.now()}`,
                parent_cell_id: cell_id,
              });
            }}
          >
            Add Table
          </BigButton>
          <BigButton
            onClick={() => {
              dispatch_action({
                type: `add_plot_cell`,
                cell_id: `plot_cell_${Date.now()}`,
                parent_cell_id: cell_id,
              });
            }}
          >
            Add Plot
          </BigButton>
          <BigButton
            onClick={() => {
              dispatch_action({
                type: `remove_filter_cell`,
                cell_id: cell_id,
              });
            }}
          >
            Remove
          </BigButton>
        </div>
      </CellSection>
    </CellWrapper>
  );
}

function CellFiltersSection() {
  const cell_id = hooks.useCell().cell_id;
  const filters = hooks.useStore(stores.filters_by_cell_id).get(cell_id);
  const catalog_id = hooks.useStore(stores.catalog_id_by_cell_id).get(cell_id);
  const catalog_metadata = hooks
    .useStore(stores.catalog_metadata_wrapper_by_catalog_id)
    .get(catalog_id);
  const catalog_field_hierarchy = catalog_metadata.hierarchy;

  const filter_ids = Object.keys(filters);

  const filter_nodes = filter_ids.map((filter_id) =>
    catalog_field_hierarchy.find((d) => get_field_id(d.data) === filter_id)
  );

  const node_ids_set = new Set(
    filter_nodes
      .map((node) => node.ancestors())
      .flat()
      .map((d) => get_field_id(d.data))
  );

  const all_nodes = catalog_metadata?.nodes_array ?? [];

  const filtered_nodes = all_nodes.filter((node) => {
    return node_ids_set.has(get_field_id(node.data));
  });

  const filtered_ids = filtered_nodes.map((node) => get_field_id(node.data));

  return (
    <>
      <FieldsDialog />
      <div className="h-4" />
      <div className="grid grid-cols-1 gap-x-3">
        <div className="flex flex-col gap-y-2">
          {filtered_ids.map((filter_id) => (
            <Providers.FieldIDProvider key={filter_id} value={filter_id}>
              <FilterCard />
            </Providers.FieldIDProvider>
          ))}
        </div>
      </div>
    </>
  );
}

function useQueryObserver<T>(observer: QueryObserver<T> | null) {
  const [value, set_value] = React.useState(observer?.getCurrentResult());
  React.useEffect(() => {
    if (!observer) return;
    set_value(observer.getCurrentResult());
    const unsubscribe = observer.subscribe(set_value);
    return unsubscribe;
  }, [observer]);
  return value;
}

function TableCell() {
  const cell = hooks.useCell();
  const cell_id: TableCellID = is_table_cell_id(cell.cell_id);
  const parent_cell_id: FilterCellID = is_filter_cell_id(cell.parent_cell_id);
  const catalog_id = hooks.useStore(stores.catalog_id_by_cell_id).get(cell_id);

  const catalog_metadata = hooks
    .useStore(stores.catalog_metadata_wrapper_by_catalog_id)
    .get(catalog_id);

  const catalog_hierarchy = catalog_metadata?.hierarchy;

  const catalog_title = catalog_metadata?.metadata?.title;

  const column_ids_set = hooks
    .useStore(stores.column_ids_by_cell_id)
    .get(cell_id);

  const filters = hooks.useStore(stores.filters_by_cell_id).get(cell_id);

  const request_body: DataRequestBody = {
    object: true,
    count: 100,
    fields: Array.from(column_ids_set),
    ...filters,
  };

  const query_config = {
    path: `/${catalog_id}/data`,
    body: request_body,
  };

  const [data_query_observer, set_data_query_observer] =
    React.useState<QueryObserver<DataResponse> | null>(null);

  const fetch_data = () => {
    const observer = create_query_observer<DataResponse>({
      staleTime: Infinity,
      queryKey: [`data`, query_config],
      queryFn: async (): Promise<DataResponse> => {
        return fetch_api_post<DataResponse>(
          query_config.path,
          query_config.body
        ).then((response) => {
          log(`query response`, response);
          return response;
        });
      },
    });
    set_data_query_observer(observer);
  };

  const data_query = useQueryObserver(data_query_observer);

  const fetching = data_query?.isFetching;
  const data = data_query?.data ?? null;

  const has_data = data?.length && data?.length > 0;

  const is_ready =
    data_query?.data && data_query?.data?.length > 0 && catalog_metadata;

  const status_message = (() => {
    if (!has_data && !fetching) {
      return `No Data Loaded`;
    }
    if (!has_data && fetching) {
      return `Loading Data...`;
    }
  })();

  return (
    <CellWrapper>
      <CellTitle subtitle={cell_id}>{catalog_title} Table</CellTitle>
      <CellSection label="source" small>
        {parent_cell_id}
      </CellSection>
      <CellSection label="columns">
        <CellColumnsSection />
      </CellSection>
      <CellSection label="query parameters">
        {/* <pre>{JSON.stringify(query_config, null, 2)}</pre> */}
      </CellSection>
      <CellSection label="table">
        {is_ready ? (
          <Table data={data} catalog_field_hierarchy={catalog_hierarchy} />
        ) : (
          <PendingBox>{status_message}</PendingBox>
        )}
      </CellSection>
      <CellSection label="actions">
        <BigButton onClick={() => fetch_data()}>
          {fetching ? `Fetching Data...` : `Fetch Data`}
        </BigButton>
        <div>status: {data_query?.status}</div>
        <div>fetch status: {data_query?.fetchStatus}</div>
        <BigButton
          onClick={() => {
            dispatch_action({
              type: `remove_table_cell`,
              cell_id: cell_id,
            });
          }}
        >
          Remove
        </BigButton>
      </CellSection>
    </CellWrapper>
  );
}

function CellColumnsSection() {
  const cell_id = hooks.useCell().cell_id;
  const filters = hooks.useStore(stores.filters_by_cell_id).get(cell_id);
  const catalog_id = hooks.useStore(stores.catalog_id_by_cell_id).get(cell_id);
  const catalog_metadata = hooks
    .useStore(stores.catalog_metadata_wrapper_by_catalog_id)
    .get(catalog_id);

  const catalog_field_hierarchy = catalog_metadata?.hierarchy;

  const column_ids_set = hooks
    .useStore(stores.column_ids_by_cell_id)
    .get(cell_id);

  const column_ids = Array.from(column_ids_set);

  log(`catalog_metadata`, catalog_field_hierarchy);

  const column_nodes = column_ids.map((column_id) =>
    catalog_field_hierarchy.find((d) => get_field_id(d.data) === column_id)
  );

  const ancestor_ids_set = new Set(
    column_nodes
      .map((node) => node.ancestors())
      .flat()
      .map((d) => get_field_id(d.data))
  );

  log(`ancestor_ids_set`, ancestor_ids_set);

  const all_nodes = catalog_metadata?.nodes_array ?? [];

  const filtered_ids = all_nodes
    .map((node) => get_field_id(node.data))
    .filter((field_id) => ancestor_ids_set.has(field_id));

  return (
    <>
      <div className="h-4" />
      <div className="grid grid-cols-1 gap-x-3">
        <div className="flex flex-col gap-y-2">
          {filtered_ids.map((column_id) => (
            <Providers.FieldIDProvider key={column_id} value={column_id}>
              <ColumnCard />
            </Providers.FieldIDProvider>
          ))}
        </div>
      </div>
    </>
  );
}

function FieldsDialog() {
  const cell = hooks.useCell();
  const cell_id = cell.cell_id;
  const catalog_id = hooks.useStore(stores.catalog_id_by_cell_id).get(cell_id);
  const catalog_metadata_wrapper = hooks
    .useStore(stores.catalog_metadata_wrapper_by_catalog_id)
    .get(catalog_id);
  const fields_list = catalog_metadata_wrapper?.nodes_array ?? [];
  const field_cards = fields_list
    .filter((d) => "name" in d.data)
    .map((field) => {
      const field_id = get_field_id(field.data);
      return (
        <Providers.FieldIDProvider value={field_id} key={field_id}>
          <FieldCard></FieldCard>
        </Providers.FieldIDProvider>
      );
    });
  return (
    <Dialog.Root>
      <Dialog.Trigger className={BigButton.className}>
        Show All Fields
      </Dialog.Trigger>
      <Dialog.Portal>
        <Dialog.Overlay className="fixed inset-0 z-20 bg-black/50" />
        <Dialog.Content
          className={clsx(
            `fixed z-50 w-[95vw] max-w-3xl rounded-lg p-4`,
            `top-[50%] left-[50%] -translate-x-[50%] -translate-y-[50%]`,
            `bg-light-2 dark:bg-dark-2`,
            `focus:outline-none focus-visible:ring focus-visible:ring-purple-500 focus-visible:ring-opacity-75`
          )}
        >
          <Dialog.Title className="text-sm font-medium text-light-text dark:text-dark-text">
            All Fields
          </Dialog.Title>
          <div className="h-4" />
          <Dialog.Description />
          <div className="h-4" />
          <input
            className="w-full bg-light-0 dark:bg-dark-0 rounded-lg text-lg leading-5 py-2 px-3 focus:ring-2 focus:ring-light-4 dark:focus:ring-dark-4 focus:outline-none"
            type="text"
            placeholder="search"
          />
          <div className="h-4" />
          <div className="h-[600px] overflow-y-scroll overflow-x-visible grid grid-cols-1 gap-4">
            {field_cards}
          </div>
          <Dialog.Close
            className={clsx(
              "absolute top-3.5 right-3.5 inline-flex items-center justify-center rounded-full p-1",
              "focus:outline-none focus-visible:ring focus-visible:ring-purple-500 focus-visible:ring-opacity-75"
            )}
          >
            <Cross1Icon className="h-4 w-4 text-light-text dark:text-dark-text" />
          </Dialog.Close>
        </Dialog.Content>
      </Dialog.Portal>
    </Dialog.Root>
  );
}

// const number_regex = /^[+-]?(\d+(\.\d*)?|\.\d+)([eE][+-]?\d+)?$/;

// function CellColumnsSection({ column_ids }: { column_ids: Set<string> }) {
//   const cell_id = hooks.useCellID();

//   const names_order =
//     hooks.useCatalogMetadata()?.nodes_array.map((d) => d.data.name) ?? [];

//   const column_ids_raw = Array.from(column_ids);

//   const sorted = d3.sort(column_ids_raw, (d) => names_order.indexOf(d));
//   return (
//     <div className="grid grid-cols-3 gap-x-3 gap-y-2">
//       {sorted.map((column_id) => {
//         return (
//           <div
//             key={column_id}
//             className="flex items-center justify-between text-xs text-slate-700 dark:text-slate-200 px-2 py-1 rounded bg-slate-50 dark:bg-slate-600"
//           >
//             <div>id: {column_id}</div>
//             <TrashIcon
//               className="w-3 h-3 cursor-pointer"
//               onClick={() => {
//                 dispatch_action({
//                   type: `remove_column`,
//                   cell_id,
//                   column_id,
//                 });
//               }}
//             />
//           </div>
//         );
//       })}
//     </div>
//   );
// }

function PlotCell() {
  const cell = hooks.useCell();
  const cell_id: PlotCellID = is_plot_cell_id(cell.cell_id);
  const parent_cell_id: FilterCellID = is_filter_cell_id(cell.parent_cell_id);

  const catalog_id = hooks.useStore(stores.catalog_id_by_cell_id).get(cell_id);

  const catalog_metadata_query_observer = hooks
    .useStore(stores.catalog_metadata_query_observer_by_catalog_id)
    .get(catalog_id);

  const catalog_metadata_query = useQueryObserver(
    catalog_metadata_query_observer
  );

  const catalog_metadata = catalog_metadata_query?.data ?? null;

  const catalog_title = catalog_metadata?.title;
  return (
    <CellWrapper>
      <CellTitle subtitle={cell_id}>{catalog_title} Plot</CellTitle>
      <CellSection label="source" small>
        {parent_cell_id}
      </CellSection>
      <CellSection label="actions">
        {/* <BigButton onClick={() => fetch_data()}>
          {fetching ? `Fetching Data...` : `Fetch Data`}
        </BigButton>
        <div>status: {data_query?.status}</div>
        <div>fetch status: {data_query?.fetchStatus}</div> */}
        <BigButton
          onClick={() => {
            dispatch_action({
              type: `remove_plot_cell`,
              cell_id: cell_id,
            });
          }}
        >
          Remove
        </BigButton>
      </CellSection>
    </CellWrapper>
  );
}

function PendingBox({ children }: { children: React.ReactNode }) {
  return (
    <div className="h-40 rounded-lg p-4 outline-2 outline-dashed outline-slate-700 dark:outline-slate-50 grid place-items-center opacity-50">
      {children}
    </div>
  );
}

function CellTitle({
  children,
  subtitle,
}: {
  children: React.ReactNode;
  subtitle?: React.ReactNode;
}): React.JSX.Element {
  return (
    <div>
      <div className="text-2xl font-bold">{children}</div>
      {subtitle && <div className="text-slate-400">{subtitle}</div>}
    </div>
  );
}

function CellSection({
  label,
  children = null,
  small = false,
}: {
  label: string;
  children?: React.ReactNode;
  small?: boolean;
}): React.JSX.Element {
  return (
    <div className="flex flex-col">
      <SimpleLabel>{label}</SimpleLabel>
      <div className={small ? `h-1` : `h-4`}></div>
      {children}
    </div>
  );
}

function SimpleLabel({
  children,
}: {
  children: React.ReactNode;
}): React.JSX.Element {
  return (
    <div className="text-slate-400 dark:text-slate-400 uppercase text-sm">
      {children}
    </div>
  );
}
