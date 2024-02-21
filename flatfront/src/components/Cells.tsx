import type { CatalogCell as CatalogCellType, CatalogCellID } from "../types";
import * as d3 from "d3";

import { useAppState } from "./contexts/AppStateContext";
import { CatalogCell } from "./CatalogCell";

export function Cells() {
  const app_state = useAppState();
  const cells = app_state?.cells ?? {};
  const cells_order = app_state?.cells_order ?? [];
  const entries = Object.entries(cells) as [CatalogCellID, CatalogCellType][];
  const cells_array = d3
    .sort(entries, ([id]) => cells_order.indexOf(id) ?? id)
    .map((d) => d[1]);
  return (
    <>
      {cells_array.map((cell) => {
        const component = (() => {
          return <CatalogCell key={cell.id} id={cell.id} />;
        })();
        return component;
      })}
    </>
  );
}
