import type { AppState } from "../types";
import React from "react";
import { useDebounce } from "@uidotdev/usehooks";
import * as lzstring from "lz-string";
import lodash_merge from "lodash.merge";
import { useImmer, type Updater } from "use-immer";
import { log } from "../shared";

const AppStateContext = React.createContext<AppState>({});
const SetAppStateContext = React.createContext<Updater<AppState> | undefined>(
  undefined
);

export function AppStateProvider({ children }) {
  const [app_state, set_app_state] = useImmer<AppState>(() => {
    const app_state_from_url = get_data_from_url<any>(`app_state`);
    const initial_app_state: AppState = app_state_from_url ?? {};
    return initial_app_state;
  });
  return (
    <AppStateContext.Provider value={app_state}>
      <SetAppStateContext.Provider value={set_app_state}>
        {children}
      </SetAppStateContext.Provider>
    </AppStateContext.Provider>
  );
}

export function useAppState() {
  const app_state = React.useContext(AppStateContext);
  if (app_state === undefined) {
    throw new Error(`useAppState must be used within a Provider`);
  }
  return app_state;
}

export function useSetAppState() {
  const set_app_state = React.useContext(SetAppStateContext);
  if (set_app_state === undefined) {
    throw new Error(`useSetAppState must be used within a Provider`);
  }
  return set_app_state;
}

export function useMergeState() {
  const set_app_state = useSetAppState();
  return (next: AppState) => set_app_state((prev) => lodash_merge(prev, next));
}

export function useSaveStateInURL() {
  const app_state = useAppState();
  const debounced = useDebounce(app_state, 1000);
  React.useEffect(() => {
    if (Object.keys(debounced).length === 0) return;
    store_data_in_url(debounced, `app_state`);
  }, [debounced]);
}

function store_data_in_url<T>(data: T, key: string) {
  log(`Storing ${key} data in URL:`, data);
  const compressed = compress_data(data);
  const url = new URL(window.location.href);
  url.searchParams.set(key, compressed);
  window.history.replaceState({}, ``, url.toString());
  const url_length = url.toString().length;
  log(`URL length: ${url_length}`);
  if (url_length > 5000) {
    console.error(`URL is too long!`);
  }
}

function get_data_from_url<T>(key: string): T | undefined {
  const url = new URL(window.location.href);
  const compressed = url.searchParams.get(key);
  if (compressed && compressed.length > 0) {
    const data = decompress_data<T>(compressed);
    return data;
  }
  return undefined;
}

function compress_data<T>(data: T): string {
  const compressed = lzstring.compressToEncodedURIComponent(
    JSON.stringify(data)
  );
  return compressed;
}

function decompress_data<T>(compressed: string): T {
  const restored = JSON.parse(
    lzstring.decompressFromEncodedURIComponent(compressed)
  );
  return restored;
}
