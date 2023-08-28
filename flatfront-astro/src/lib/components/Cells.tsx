import type {
  DataRequestBody,
  DataResponse,
  TopResponseEntry,
  Actions,
} from "../types";
import type { QueryObserver } from "@tanstack/query-core";

import React from "react";

import clsx from "clsx";

import * as Dialog from "@radix-ui/react-dialog";
import * as d3 from "d3";
import { Cross1Icon } from "@radix-ui/react-icons";
import { Select, useQueryObserver } from "../shared";
import { QueryParameter } from "./FieldCard";

import {
  BigButton,
  CellWrapper,
  create_query_observer,
  dispatch_action,
  fetch_api_post,
  hooks,
  log,
  Providers,
  get_field_id,
  CellSection,
  PendingBox,
} from "../shared";
import * as stores from "../stores";
import Table from "./Table";
import { FieldCard, FilterCard } from "./FieldCard";
import PlotSections from "./PlotSections";

export default function Cells() {
  const cells = hooks.useStore(stores.cells);

  return (
    <>
      {cells.map((cell) => {
        if (cell.type === `root`) return null;
        const component = (() => {
          switch (cell.type) {
            case `cell`:
              return <GenericCell />;
            case `catalog`:
              return null;
            case `filter`:
              return null;
            case `table`:
              return null;
            case `plot`:
              return null;
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

function GenericCell() {
  const cell_id = hooks.useCell().cell_id;

  return (
    <CellWrapper className="desktop:grid-cols-6">
      <div className="text-slate-400 desktop:col-span-6">{cell_id}</div>
      <CellSection label="catalog" className="desktop:col-span-3">
        <CatalogSelect />
      </CellSection>
      <CellSection label="type" className="desktop:col-span-3">
        <CellTypeSelect />
      </CellSection>
      <CellSection
        label="filters"
        className="desktop:col-start-1 desktop:col-span-2"
      >
        <CellFiltersSection />
      </CellSection>
      <div className="desktop:col-start-3 desktop:col-span-4">
        <CellSubSections />
      </div>
    </CellWrapper>
  );
}

function CatalogSelect() {
  const cell_id = hooks.useCell().cell_id;
  const catalog_id = hooks
    .useStore(stores.actions_by_cell_id)
    .get(cell_id)
    .filter((d): d is Actions[`SetCatalog`] => d.type === `set_catalog`)
    .at(-1)?.catalog_id;
  const get_title = (d: TopResponseEntry) => d?.title;
  const catalog_list_unsorted = hooks.useStore(stores.top_response).data ?? [];
  const ready = catalog_list_unsorted.length > 0;
  const catalog_list = d3.sort(catalog_list_unsorted, get_title);
  const selected = catalog_list.find((d) => d.name === catalog_id);
  return (
    <div data-type="CatalogSelect">
      <Select
        disabled={!ready}
        placeholder={ready ? "Select a catalog..." : "Loading catalogs..."}
        options={catalog_list}
        getKey={(d) => d?.name}
        getDisplayName={get_title}
        value={selected}
        onValueChange={(d) => {
          dispatch_action({
            type: `set_catalog`,
            cell_id,
            catalog_id: d?.name,
          } as Actions[`SetCatalog`]);
          // set_selected(d);
        }}
        buttonClassName="bg-light-3 dark:bg-dark-3"
        optionsClassName="bg-light-3 dark:bg-dark-3"
        optionClassName="ui-active:bg-light-4 dark:ui-active:bg-dark-4"
      />
    </div>
  );
}

function CellTypeSelect() {
  const cell_id = hooks.useCell().cell_id;
  const cell_type = hooks
    .useStore(stores.actions_by_cell_id)
    .get(cell_id)
    .filter((d): d is Actions[`SetCellType`] => d.type === `set_cell_type`)
    .at(-1)?.cell_type;
  return (
    <div data-type="CatalogSelect">
      <Select
        placeholder={`Select a cell type...`}
        options={[`table`, `plot`]}
        value={cell_type}
        getDisplayName={(d) => {
          switch (d) {
            case `table`:
              return `Table`;
            case `plot`:
              return `Plot`;
          }
        }}
        onValueChange={(d) => {
          dispatch_action({
            type: `set_cell_type`,
            cell_id,
            cell_type: d,
          } as Actions[`SetCellType`]);
        }}
        buttonClassName="bg-light-3 dark:bg-dark-3"
        optionsClassName="bg-light-3 dark:bg-dark-3"
        optionClassName="ui-active:bg-light-4 dark:ui-active:bg-dark-4"
      />
    </div>
  );
}

function CellSubSections() {
  const cell_id = hooks.useCell().cell_id;
  const cell_type = hooks.useStore(stores.cell_type_by_cell_id).get(cell_id);

  const placeholder = <PendingBox>Select a cell type...</PendingBox>;

  const component = (() => {
    switch (cell_type) {
      case `table`:
        return <TableSections />;
      case `plot`:
        return <PlotSections />;
      default:
        return placeholder;
    }
  })();

  return <div className="desktop:col-start-2">{component}</div>;
}

function TableSections() {
  const cell_id = hooks.useCell().cell_id;
  const catalog_id = hooks.useStore(stores.catalog_id_by_cell_id).get(cell_id);

  const column_ids_set = hooks
    .useStore(stores.column_ids_by_cell_id)
    .get(cell_id);

  const filters = hooks.useStore(stores.filters_by_cell_id).get(cell_id);

  const query_parameters = hooks
    .useStore(stores.query_parameters_by_cell_id)
    .get(cell_id);

  const request_body: DataRequestBody = {
    object: true,
    count: 100,
    fields: Array.from(column_ids_set),
    ...filters,
    ...query_parameters,
  };

  const query_config = {
    path: `/${catalog_id}/data`,
    body: request_body,
  };

  const [data_query_observer, set_data_query_observer] =
    React.useState<QueryObserver<DataResponse> | null>(null);

  const fetch_data = React.useCallback(() => {
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
  }, [query_config]);

  const data_query = useQueryObserver(data_query_observer);

  const catalog_hierarchy = hooks
    .useStore(stores.catalog_metadata_wrapper_by_catalog_id)
    .get(catalog_id)?.hierarchy;

  const fetching = data_query?.isFetching;
  const data = data_query?.data ?? null;
  const has_data = data?.length && data?.length > 0;
  const is_ready = has_data && catalog_hierarchy;

  const table_component = (() => {
    if (!is_ready) {
      const status_message = fetching ? `Loading Data...` : `No Data Loaded`;
      return <PendingBox>{status_message}</PendingBox>;
    }
    return <Table data={data} catalog_field_hierarchy={catalog_hierarchy} />;
  })();

  return (
    <div className="space-y-4">
      <CellSection label="columns">
        <ColumnsDialog />
      </CellSection>
      <CellSection label="query parameters">
        <QueryParameter label="Rows" field_id="count" min={10} max={200} />
      </CellSection>
      <CellSection label="fetch">
        <BigButton onClick={() => fetch_data()}>
          {fetching ? `Fetching Data...` : `Fetch Data`}
        </BigButton>
      </CellSection>
      <CellSection label="table">{table_component}</CellSection>
    </div>
  );
}

function ColumnsDialog() {
  const cell_id = hooks.useCell().cell_id;
  const catalog_id = hooks.useStore(stores.catalog_id_by_cell_id).get(cell_id);
  const catalog_metadata = hooks
    .useStore(stores.catalog_metadata_wrapper_by_catalog_id)
    .get(catalog_id);
  const all_field_nodes = catalog_metadata?.nodes_array ?? [];
  const column_cards = all_field_nodes
    .filter((d) => !d.data.__is_root)
    .map((field) => {
      const field_id = get_field_id(field.data);
      return (
        <Providers.FieldIDProvider value={field_id} key={field_id}>
          <FieldCard mode="column"></FieldCard>
        </Providers.FieldIDProvider>
      );
    });
  return (
    <FieldsDialog label="Select Columns">
      <div className="space-y-4">{column_cards}</div>
    </FieldsDialog>
  );
}

function CellFiltersSection() {
  const cell_id = hooks.useCell().cell_id;
  const catalog_id = hooks
    .useStore(stores.actions_by_cell_id)
    .get(cell_id)
    .filter((d): d is Actions[`SetCatalog`] => d.type === `set_catalog`)
    .at(-1)?.catalog_id;
  // const filters = hooks.useStore(stores.filters_by_cell_id).get(cell_id);
  // const catalog_id = hooks.useStore(stores.catalog_id_by_cell_id).get(cell_id);
  const catalog_metadata = hooks
    .useStore(stores.catalog_metadata_wrapper_by_catalog_id)
    .get(catalog_id);

  // const filter_ids_set = new Set(Object.keys(filters));

  const all_field_nodes = catalog_metadata?.nodes_array ?? [];

  // const filter_and_ancestor_ids = all_field_nodes
  //   .filter((d) => !d.data.__is_root)
  //   .filter((node) => {
  //     // Include if this node is in the filter list
  //     if (filter_ids_set.has(get_field_id(node.data))) return true;
  //     // Include if this node is an ancestor of a node in the filter list
  //     if (
  //       node
  //         .leaves()
  //         .some((leaf) => filter_ids_set.has(get_field_id(leaf.data)))
  //     )
  //       return true;
  //     return false;
  //   })
  //   .map((d) => get_field_id(d.data));

  const all_field_cards = all_field_nodes
    .filter((d) => !d.data.__is_root)
    .map((field) => {
      const field_id = get_field_id(field.data);
      return (
        <Providers.FieldIDProvider value={field_id} key={field_id}>
          <FieldCard mode="filter"></FieldCard>
        </Providers.FieldIDProvider>
      );
    });

  return (
    <div className="flex flex-col gap-y-4">
      <FieldsDialog label="Select Filters">
        <div className="space-y-4">{all_field_cards}</div>
      </FieldsDialog>
      <FieldsDialog label="Update Filters" className="desktop:hidden">
        <FiltersList />
      </FieldsDialog>
      <div className="hidden desktop:block">
        <FiltersList />
      </div>
    </div>
  );
}

function FiltersList() {
  const cell_id = hooks.useCell().cell_id;
  const filters = hooks.useStore(stores.filters_by_cell_id).get(cell_id);
  const catalog_id = hooks.useStore(stores.catalog_id_by_cell_id).get(cell_id);
  const catalog_metadata = hooks
    .useStore(stores.catalog_metadata_wrapper_by_catalog_id)
    .get(catalog_id);
  const all_field_nodes = catalog_metadata?.nodes_array ?? [];
  const filter_ids_set = new Set(Object.keys(filters));
  const filter_and_ancestor_ids = all_field_nodes
    .filter((d) => !d.data.__is_root)
    .filter((node) => {
      // Include if this node is in the filter list
      if (filter_ids_set.has(get_field_id(node.data))) return true;
      // Include if this node is an ancestor of a node in the filter list
      if (
        node
          .leaves()
          .some((leaf) => filter_ids_set.has(get_field_id(leaf.data)))
      )
        return true;
      return false;
    })
    .map((d) => get_field_id(d.data));

  return (
    <div className="space-y-2 text-xs">
      {filter_and_ancestor_ids.map((filter_id) => (
        <Providers.FieldIDProvider key={filter_id} value={filter_id}>
          <FilterCard />
        </Providers.FieldIDProvider>
      ))}
    </div>
  );
}

function CellColumnsSection() {
  const cell_id = hooks.useCell().cell_id;
  const catalog_id = hooks.useStore(stores.catalog_id_by_cell_id).get(cell_id);
  const catalog_metadata = hooks
    .useStore(stores.catalog_metadata_wrapper_by_catalog_id)
    .get(catalog_id);

  const all_field_nodes = catalog_metadata?.nodes_array ?? [];

  const column_ids_set = hooks
    .useStore(stores.column_ids_by_cell_id)
    .get(cell_id);

  const column_and_ancestor_ids = all_field_nodes
    .filter((d) => !d.data.__is_root)
    .filter((node) => {
      // Include if this node is in the column list
      if (column_ids_set.has(get_field_id(node.data))) return true;
      // Include if this node is an ancestor of a node in the column list
      if (
        node
          .leaves()
          .some((leaf) => column_ids_set.has(get_field_id(leaf.data)))
      )
        return true;
      return false;
    })
    .map((d) => get_field_id(d.data));

  const all_field_cards = all_field_nodes
    .filter((d) => !d.data.__is_root)
    .map((field) => {
      const field_id = get_field_id(field.data);
      return (
        <Providers.FieldIDProvider value={field_id} key={field_id}>
          <FieldCard mode="column"></FieldCard>
        </Providers.FieldIDProvider>
      );
    });

  return (
    <>
      <FieldsDialog label="Select Columns">{all_field_cards}</FieldsDialog>
      {/* <div className="h-4" />
      <div className="flex flex-col gap-y-2 text-xs">
        {column_and_ancestor_ids.map((column_id) => (
          <Providers.FieldIDProvider key={column_id} value={column_id}>
            <ColumnCard />
          </Providers.FieldIDProvider>
        ))}
      </div> */}
    </>
  );
}

function FieldsDialog({
  label,
  children,
  className,
}: {
  label: string;
  children: React.ReactNode;
  className?: string;
}): React.JSX.Element {
  return (
    <Dialog.Root>
      <Dialog.Trigger className={clsx(BigButton.className, className)}>
        {label}
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
          {/* <div className="h-4" /> */}
          {/* <Dialog.Description /> */}
          <div className="h-4" />
          <input
            className="w-full bg-light-0 dark:bg-dark-0 rounded-lg text-lg leading-5 py-2 px-3 focus:ring-2 focus:ring-light-4 dark:focus:ring-dark-4 focus:outline-none"
            type="text"
            placeholder="search"
          />
          <div className="h-4" />
          <div className="h-[600px] overflow-y-scroll overflow-x-visible">
            {children}
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
