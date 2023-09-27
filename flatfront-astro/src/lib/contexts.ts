import type { CatalogHierarchyNode, CellID } from "./types";

import React from "react";

function create_context_helper<T>(debug: string) {
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

const [useCatalogCellID, CatalogCellIDProvider] =
  create_context_helper<CellID.Catalog>(`CatalogCellID`);
const [useFieldNode, FieldNodeProvider] =
  create_context_helper<CatalogHierarchyNode>(`FieldNode`);

export const Providers = {
  CatalogCellIDProvider,
  FieldNodeProvider
};

export const hooks = {
  useCatalogCellID,
  useFieldNode
};
