import type { Readable } from "svelte/store";
import type {
  Action,
  CatalogMetadataWrapper,
  CellID,
  DarkModeValue,
  FilterValueRaw,
  Filters
} from "./types";

import React from "react";
import { get } from "svelte/store";
import * as immer from "immer";

import { assert_catalog_cell_id, log } from "./shared";
import * as stores from "./stores";
import { hooks as context_hooks } from "./contexts";

const { useFieldNode, useCell } = context_hooks;

export { useCell, useFieldNode };

export function useDelayVisible(
  ref: React.RefObject<HTMLElement>,
  delay: number
): boolean {
  const [visible, setVisible] = React.useState(false);
  React.useEffect(() => {
    let timeout: number;
    const observer = new IntersectionObserver(
      (entries) => {
        if (entries[0].isIntersecting) {
          timeout = window.setTimeout(() => {
            setVisible(true);
          }, delay);
        }
        if (!entries[0].isIntersecting) {
          clearTimeout(timeout);
        }
      },
      { threshold: 0.5 }
    );
    if (ref.current) {
      observer.observe(ref.current);
    }
    return () => {
      if (ref.current) {
        observer.unobserve(ref.current);
      }
    };
  }, [ref, delay]);
  return visible;
}

export function useStore<T>(store: Readable<T>) {
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

export function useCatalogCellID(): CellID.Catalog {
  const cell_data = useCell();
  const catalog_cell_id =
    "catalog_cell_id" in cell_data
      ? cell_data.catalog_cell_id
      : cell_data.cell_id;
  assert_catalog_cell_id(catalog_cell_id);
  return catalog_cell_id;
}

export function useCellActions(): Action.Any[] {
  const catalog_cell_id = useCatalogCellID();
  const cell_actions = useStore(stores.actions_by_cell_id).get(catalog_cell_id);
  return cell_actions;
}

export function useCatalogID(): string {
  const catalog_cell_id = useCatalogCellID();
  const catalog_id = useStore(stores.catalog_id_by_cell_id).get(
    catalog_cell_id
  );
  return catalog_id;
}

export function useCatalogMetadata(): CatalogMetadataWrapper {
  const catalog_id = useCatalogID();
  const catalog_metadata = useStore(stores.catalog_metadata_by_catalog_id)?.[
    catalog_id
  ];
  return catalog_metadata;
}

export function useFilters(): Filters {
  const catalog_cell_id = useCatalogCellID();
  const filters = useStore(stores.filters_by_cell_id).get(catalog_cell_id);
  return filters;
}

export function useFilterValueSetter() {
  const catalog_cell_id = useCatalogCellID();
  const field_node = useFieldNode();
  const field_id = field_node.data.name;
  const set_filter_value = (filter_value: FilterValueRaw) => {
    stores.filter_state.update((fitler_state_object) => {
      return immer.produce(fitler_state_object, (draft) => {
        draft[catalog_cell_id] = draft[catalog_cell_id] || {};
        draft[catalog_cell_id][field_id] = filter_value;
      });
    });
  };
  return set_filter_value;
}

export function useDarkModeValue(): DarkModeValue {
  const all_actions = useStore(stores.actions);
  const dark_mode_actions = all_actions.filter(
    (action): action is Action.SetDarkMode => {
      return action.type === `set_dark_mode`;
    }
  );
  const latest = dark_mode_actions.at(-1);
  return latest?.value ?? `system`;
}

function useSystemDarkMode(): boolean {
  const [is_dark_mode, set_is_dark_mode] = React.useState<boolean>(() => {
    const system_dark_mode = window.matchMedia(
      "(prefers-color-scheme: dark)"
    ).matches;
    return system_dark_mode;
  });
  React.useEffect(() => {
    const system_dark_mode = window.matchMedia("(prefers-color-scheme: dark)");
    const listener = (event: MediaQueryListEvent) => {
      log(`System dark mode changed:`, event.matches);
      set_is_dark_mode(event.matches);
    };
    system_dark_mode.addEventListener("change", listener);
    return () => {
      system_dark_mode.removeEventListener("change", listener);
    };
  }, []);
  return is_dark_mode;
}

export function useIsDarkMode(): boolean {
  const dark_mode_value = useDarkModeValue();
  const system_dark_mode = useSystemDarkMode();
  if (dark_mode_value === `system`) return system_dark_mode;
  const is_dark_mode = dark_mode_value === `dark`;
  return is_dark_mode;
}

export function useDebounce<T>(value: T, delay: number): T {
  const [debounced, set_debounced] = React.useState<T>(value);
  React.useEffect(() => {
    const handler = setTimeout(() => {
      set_debounced(value);
    }, delay);
    return () => {
      clearTimeout(handler);
    };
  }, [value, delay]);
  return debounced;
}

export function useToggleDarkMode(): void {
  const is_dark_mode = useIsDarkMode();
  React.useEffect(() => {
    const has_dark_mode_class =
      document.documentElement.classList.contains("dark");
    if (is_dark_mode && !has_dark_mode_class) {
      document.documentElement.classList.add("dark");
    } else if (!is_dark_mode && has_dark_mode_class) {
      document.documentElement.classList.remove("dark");
    }
  }, [is_dark_mode]);
}
