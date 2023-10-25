import React from "react";
import * as controller from "./AppStateContext";
import { useCatalogCellID, useCatalogID } from "./CatalogContext";

const RandomContext = React.createContext<
  | {
      sample?: number;
      seed?: number;
    }
  | undefined
>(undefined);

export function RandomProvider({ children }) {
  const catalog_cell_id = useCatalogCellID();
  const catalog_id = useCatalogID();
  const app_state = controller.useAppState();
  const random_config =
    app_state?.set_random_sample?.[catalog_cell_id]?.[catalog_id];

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
  const dispatch = controller.useDispatch();
  return (key: `sample` | `seed`, value: number) => {
    dispatch([`set_random_sample`, catalog_cell_id, catalog_id, key], value);
    // dispatch({
    //   set_random_sample: {
    //     [catalog_cell_id]: {
    //       [catalog_id]: {
    //         [key]: value
    //       }
    //     }
    //   }
    // });
  };
}
