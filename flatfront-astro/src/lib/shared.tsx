import type { QueryObserverOptions } from "@tanstack/query-core";
import type { Readable } from "svelte/store";
import type { Action, Cell, CellID } from "./types";

import React from "react";
import clsx from "clsx";
import { get } from "svelte/store";
import { QueryClient, QueryObserver } from "@tanstack/query-core";

import * as stores from "./stores";

const FLATHUB_API_BASE_URL = `https://flathub.flatironinstitute.org`;

const query_client = new QueryClient();

const [useCell, CellProvider] = useContextHelper<Cell.Any>(`Cell`);
// const [usePlotID, PlotIDProvider] = useContextHelper<string>(`PlotID`);
// const [useFieldID, FieldIDProvider] = useContextHelper<string>(`FieldID`);
// const [useData, DataProvider] = useContextHelper<Datum[]>(`Data`);

export const Providers = {
  CellProvider
  // PlotIDProvider,
  // FieldIDProvider,
  // DataProvider
};

export const hooks = {
  useStore,
  useCell
};

function useStore<T>(store: Readable<T>) {
  const [state, setState] = React.useState<T>(get(store));
  React.useEffect(
    () =>
      store.subscribe((value) => {
        setState(value);
      }),
    [store]
  );
  return state;
}

export function dispatch_action(action: Action.Any) {
  stores.actions.update(($actions) => [...$actions, action]);
}

function useContextHelper<T>(debug: string) {
  const context = React.createContext<T | null>(null);
  const useContext = (): T => {
    const value: T | null = React.useContext(context);
    if (value === null) {
      throw new Error(`useContextHelper: ${debug}: value is null`);
    }
    return value;
  };
  return [useContext, context.Provider] as const;
}

export function create_query_observer<T>(
  options: QueryObserverOptions<T>
): QueryObserver<T> {
  log(`🐛 Creating Query Observer:`, options.queryKey, options);
  const defaulted_options = query_client.defaultQueryOptions(options);
  return new QueryObserver(query_client, defaulted_options);
}

export async function fetch_api_get<T>(path: string): Promise<T> {
  const url = new URL(`/api${path}`, FLATHUB_API_BASE_URL);
  log(`💥 Fetching:`, url.toString());
  const response = await fetch(url.toString(), {
    method: `GET`,
    headers: new Headers({
      "Content-Type": `application/json`
    })
  });
  log(`💥 Got Response:`, url.toString());
  if (!response.ok) {
    throw new Error(`API Fetch Error: ${response.status}`);
  }
  const json: T = await response.json();
  return json;
}

export function log(...args: any[]) {
  console.log(`🌔`, ...args);
}

// Function to ensure that a string is a CellID.Catalog
export function is_catalog_cell_id(cell_id: CellID.Any): CellID.Catalog {
  if (!cell_id.match(/^catalog_cell_/)) {
    throw new Error(`${cell_id} is not a catalog cell id`);
  }
  return cell_id as CellID.Catalog;
}

export function is_table_cell_id(cell_id: CellID.Any): CellID.Table {
  if (!cell_id.match(/^table_cell_/)) {
    throw new Error(`${cell_id} is not a table cell id`);
  }
  return cell_id as CellID.Table;
}

export function CellWrapper({
  children,
  className
}: {
  children: React.ReactNode;
  className?: string;
}): React.JSX.Element {
  return (
    <div
      data-type="CellWrapper"
      className={clsx(
        `w-full rounded p-6`,
        `ring-1 ring-black/30 dark:ring-white/30`,
        className
      )}
    >
      {children}
    </div>
  );
}

export function BigButton({
  children,
  onClick,
  className,
  ...props
}: React.ButtonHTMLAttributes<HTMLButtonElement> & {
  children: React.ReactNode;
}): React.JSX.Element {
  return (
    <button
      data-type="BigButton"
      className={clsx(BigButton.className, className)}
      onClick={onClick}
      {...props}
    >
      {children}
    </button>
  );
}

BigButton.className = clsx(
  `block rounded-lg py-3 font-bold text-xl`,
  `ring-1 ring-black dark:ring-white`,
  `focus:outline-none focus-visible:ring-4`,
  `disabled:opacity-50 disabled:cursor-not-allowed`
);

export function CellSection({
  label,
  children = null,
  className
}: {
  label?: string;
  children?: React.ReactNode;
  className?: string;
}): React.JSX.Element {
  return (
    <div data-type="CellSection" className={className}>
      {label && <SimpleLabel>{label}</SimpleLabel>}
      {children}
    </div>
  );
}

export function SimpleLabel({
  children,
  className
}: {
  children: React.ReactNode;
  className?: string;
}): React.JSX.Element {
  return <div className={clsx(`text-sm uppercase`, className)}>{children}</div>;
}

export function Placeholder({
  children,
  className,
  ...rest
}: { children: React.ReactNode } & React.HTMLAttributes<HTMLDivElement>) {
  return (
    <div
      className={clsx(
        `grid h-40 place-items-center rounded-lg p-4 opacity-50`,
        `leading-none`,
        `outline-dashed outline-2 outline-slate-700 dark:outline-slate-50`,
        className
      )}
    >
      {children}
    </div>
  );
}
