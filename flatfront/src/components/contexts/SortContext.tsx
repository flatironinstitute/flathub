import React from "react";

type TableSort = { field: string; order: `asc` | `desc` };

const SortContext = React.createContext<TableSort[]>([]);
const SetSortContext =
  React.createContext<React.Dispatch<React.SetStateAction<TableSort[]>>>(null);

export function SortProvider({ children }: { children: React.ReactNode }) {
  const [sort, set_sort] = React.useState<TableSort[]>([]);
  return (
    <SortContext.Provider value={sort}>
      <SetSortContext.Provider value={set_sort}>
        {children}
      </SetSortContext.Provider>
    </SortContext.Provider>
  );
}

export function useSort(): TableSort[] {
  const sort = React.useContext(SortContext);
  return sort;
}

export function useSetSort(): React.Dispatch<
  React.SetStateAction<TableSort[]>
> {
  const set_sort = React.useContext(SetSortContext);
  return set_sort;
}
