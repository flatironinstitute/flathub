import type { AppState } from "./types";
import React from "react";
import { createStateContext } from "react-use";
import * as lzstring from "lz-string";
import lodash_set from "lodash.set";
import { useDebounce } from "react-use";
import { log } from "./shared";

const [useContext, Provider] = createStateContext<AppState>({});

export { Provider };

export function useSaveAndRestoreState() {
  const [app_state, set_app_state] = useContext();
  React.useEffect(() => {
    const app_state = get_data_from_url<any>(`app_state`);
    if (app_state) {
      log(`Setting app state from URL:`, app_state);
      set_app_state(app_state);
    }
  }, []);
  useDebounce(
    () => {
      if (Object.keys(app_state).length === 0) return;
      store_data_in_url(app_state, `app_state`);
    },
    1000,
    [app_state]
  );
}

export function useState() {
  const [app_state] = useContext();
  return app_state;
}

export function useDispatch() {
  const [, set_app_state] = useContext();
  const dispatch = React.useCallback(
    (keys: Array<string | number>, value) => {
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
