import React from "react";
import {
  useCatalogCellID,
  useCatalogID
} from "@/components/contexts/CatalogIDContext";
import { useAppState, useMergeState } from "./AppStateContext";

const RandomContext = React.createContext<
  | {
      sample?: number;
      seed?: number;
    }
  | undefined
>(undefined);

export function RandomProvider({ children }) {
  const catalog_cell_id = useCatalogCellID();
  const app_state = useAppState();
  const random_config = app_state?.random_sample?.[catalog_cell_id];

  return (
    <RandomContext.Provider value={random_config}>
      {children}
    </RandomContext.Provider>
  );
}

export function useRandomConfig() {
  const value = React.useContext(RandomContext);
  return value;
}

export function useSetRandomConfig() {
  const catalog_cell_id = useCatalogCellID();
  const catalog_id = useCatalogID();
  const merge_state = useMergeState();
  return (key: `sample` | `seed`, value: number) => {
    if (!catalog_id) return;
    merge_state({
      random_sample: {
        [catalog_cell_id]: {
          [key]: value
        }
      }
    });
  };
}
