import type { Cell, CellID } from "../types";
import * as d3 from "d3";

import { useAppState } from "./contexts/AppStateContext";
import { CatalogCell } from "./CatalogCell";

export function Cells() {
  const cells = useAppState()?.cells ?? {};
  const entries: [string, Cell.Any][] = Object.entries(cells);
  const cells_array = d3.sort(entries, ([id]) => id).map((d) => d[1]);
  return (
    <>
      {cells_array.map((cell) => {
        const component = (() => {
          switch (cell.type) {
            case `catalog`:
              return <CatalogCell key={cell.id} id={cell.id} />;
            case `comparison`:
              return <ComparisonCell key={cell.id} id={cell.id} />;
            default:
              cell satisfies never;
          }
        })();
        return component;
      })}
    </>
  );
}

function ComparisonCell({ id }: { id: CellID.Comparison }) {
  id;
  return <div>comparison</div>;
}
