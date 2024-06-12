import type { AppState } from "@/types";
import React from "react";
import { useDebounce } from "@uidotdev/usehooks";
import * as lzstring from "lz-string";
import lodash_merge from "lodash.merge";
import { useImmer, type Updater } from "use-immer";
import { log } from "@/utils";
import { get_new_catalog_cell } from "../GlobalControls";

const AppStateContext = React.createContext<AppState>({});
const SetAppStateContext = React.createContext<Updater<AppState> | undefined>(
  undefined
);

export function AppStateProvider({ children }) {
  const [app_state, set_app_state] = useImmer<AppState>(() => {
    let initial_app_state: AppState = {};
    const url = new URL(window.location.href);
    const compressed_app_state = url.searchParams.get(`app_state`);
    if (compressed_app_state && compressed_app_state.length > 0) {
      const data: AppState = decompress_data(compressed_app_state);
      log(`Retrieved app_state data from URL:`, data);
      initial_app_state = data;
    }
    const initial_catalog_id = url.searchParams.get(`init_catalog`);
    if (initial_catalog_id && !compressed_app_state) {
      log(`Retrieved initial catalog from URL:`, initial_catalog_id);
      initial_app_state.cells ??= {};
      const initial_catalog_cell = get_new_catalog_cell(initial_catalog_id);
      initial_app_state.cells[initial_catalog_cell.cell_id] =
        initial_catalog_cell;
    }
    return initial_app_state;
  });
  // @ts-ignore FIXME: HACKY HACK
  window.set_app_state = set_app_state;
  return (
    <AppStateContext.Provider value={app_state}>
      <SetAppStateContext.Provider value={set_app_state}>
        {children}
      </SetAppStateContext.Provider>
    </AppStateContext.Provider>
  );
}

export function useAppState(): AppState {
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
  return (next: Partial<AppState>) =>
    set_app_state((prev: AppState) => lodash_merge(prev, next));
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
  log(`Compressed app state:`, compressed);
  const url = new URL(window.location.href);
  url.searchParams.set(key, compressed);
  window.history.replaceState({}, ``, url.toString());
  const url_length = url.toString().length;
  log(`URL length: ${url_length}`);
  if (url_length > 5000) {
    console.error(`URL is too long!`);
  }
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
