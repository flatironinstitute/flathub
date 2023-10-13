import type { Readable } from "svelte/store";
import type {
  Action,
  CatalogMetadataWrapper,
  DarkModeValue,
  FieldMetadata,
  FilterValueRaw,
  Filters
} from "./types";

import React from "react";
import { get } from "svelte/store";
import lodash_merge from "lodash.merge";

import * as controller from "./app-state";

import {
  assert_numeric_field_stats,
  get_field_type,
  has_numeric_field_stats,
  log
} from "./shared";
import { useCatalogCellID, useCatalogID } from "./components/CatalogCell";
import { useCatalogMetadata } from "./components/CatalogMetadata";

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

export function useDebouncedValue<T>(value: T, delay: number): T {
  const [debounced, set_debounced] = React.useState(value);
  const timeout_ref = React.useRef<NodeJS.Timeout | null>(null);
  const string_ref = React.useRef<string | null>(null);

  React.useEffect(() => {
    if (string_ref.current === JSON.stringify(value)) {
      log(`Debounced value unchanged:`, value);
      return;
    }
    string_ref.current = JSON.stringify(value);
    if (timeout_ref.current) {
      clearTimeout(timeout_ref.current);
      timeout_ref.current = null;
    }

    timeout_ref.current = setTimeout(() => {
      log(`Debounced value changed:`, value);
      set_debounced(value);
    }, delay);

    return () => {
      if (timeout_ref.current) {
        clearTimeout(timeout_ref.current);
        timeout_ref.current = null;
      }
    };
  }, [value, delay]);

  return debounced;
}

export function useFilters(): Filters {
  const catalog_cell_id = useCatalogCellID();
  const catalog_id = useCatalogID();
  const catalog_metadata = useCatalogMetadata();
  const catalog_hierarchy = catalog_metadata?.hierarchy;
  // TODO: Add and remove filters
  const filter_actions = [];
  const filter_names_set = catalog_hierarchy
    ? get_filter_names(catalog_hierarchy, filter_actions)
    : new Set<string>();
  const initial_filters: Filters = get_initial_cell_filters(
    filter_names_set,
    catalog_hierarchy
  );
  const filter_state: Filters =
    controller.useState()?.filter_value?.[catalog_cell_id]?.[catalog_id];
  const filters = lodash_merge(initial_filters, filter_state);
  return filters;
}

export function get_filter_names(
  hierarchy: d3.HierarchyNode<FieldMetadata>,
  actions: Action.Any[]
) {
  const nodes = hierarchy.descendants();
  const initial_filter_names = nodes
    .filter((node) => node.height === 0 && `required` in node.data)
    .map((node) => node.data.name);
  const filter_names_set: Set<string> = new Set(initial_filter_names);
  const filter_list_actions = actions.filter(
    (action): action is Action.AddFilter | Action.RemoveFilter => {
      return action.type === `add_filter` || action.type === `remove_filter`;
    }
  );
  for (const action of filter_list_actions) {
    switch (action.type) {
      case `remove_filter`:
        filter_names_set.delete(action.field_id);
        break;
      case `add_filter`:
        filter_names_set.add(action.field_id);
        break;
      default:
        action satisfies never;
    }
  }
  return filter_names_set;
}

function get_initial_cell_filters(
  filter_names: Set<string>,
  catalog_field_hierarchy?: d3.HierarchyNode<FieldMetadata>
): Filters {
  if (!catalog_field_hierarchy) return {};
  const initial_filter_object: Filters = Object.fromEntries(
    Array.from(filter_names).map((filter_name) => {
      const metadata = catalog_field_hierarchy.find(
        (node) => node.data.name === filter_name
      )?.data;

      if (!metadata) {
        throw new Error(`Could not find metadata for filter ${filter_name}`);
      }

      const field_type = get_field_type(metadata);

      const initial_value: FilterValueRaw = (() => {
        switch (field_type) {
          case `ROOT`:
            throw new Error(`Root field should not be a filter`);
          case `INTEGER`:
          case `FLOAT`:
            if (!has_numeric_field_stats(metadata)) {
              throw new Error(
                `Field ${metadata.name} does not have numeric field stats`
              );
            }
            assert_numeric_field_stats(metadata);
            return {
              gte: metadata.stats.min,
              lte: metadata.stats.max
            };
          case `LABELLED_ENUMERABLE_BOOLEAN`:
            return false;
          case `LABELLED_ENUMERABLE_INTEGER`:
          case `ENUMERABLE_INTEGER`:
            return 0;
          case `ARRAY`:
            return null;
          case `STRING`:
            return ``;
          default:
            field_type satisfies never;
        }
      })();

      return [filter_name, initial_value];
    })
  );
  return initial_filter_object;
}

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
