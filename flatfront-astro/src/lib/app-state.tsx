import type { AppState } from "./types";
import React from "react";
import { useDebounce } from "@uidotdev/usehooks";
import * as lzstring from "lz-string";
import lodash_set from "lodash.set";
import { log } from "./shared";

const AppStateContext = React.createContext<AppState>({});
const DispatchContext = React.createContext<
  React.Dispatch<React.SetStateAction<AppState>> | undefined
>(undefined);

export function Provider({ children }) {
  const [app_state, set_app_state] = React.useState<AppState>({});
  return (
    <AppStateContext.Provider value={app_state}>
      <DispatchContext.Provider value={set_app_state}>
        {children}
      </DispatchContext.Provider>
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

function useSetAppState() {
  const set_app_state = React.useContext(DispatchContext);
  if (set_app_state === undefined) {
    throw new Error(`useSetAppState must be used within a Provider`);
  }
  return set_app_state;
}

export function useDispatch() {
  const set_app_state = useSetAppState();
  const dispatch = React.useCallback(
    (keys: Array<string | number>, value: any) => {
      set_app_state((obj) => {
        const new_obj = { ...obj };
        lodash_set(new_obj, keys, value);
        return new_obj;
      });
    },
    [set_app_state]
  );
  return dispatch;
}

export function useSaveAndRestoreState() {
  const app_state = useAppState();
  const set_app_state = useSetAppState();
  React.useEffect(() => {
    const app_state = get_data_from_url<any>(`app_state`);
    if (app_state) {
      log(`Setting app state from URL:`, app_state);
      set_app_state(app_state);
    }
  }, []);
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
