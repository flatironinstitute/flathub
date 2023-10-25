import type { CellID } from "../types";
import * as d3 from "d3";
import CatalogCell from "./CatalogCell";
import { useAppState } from "../contexts/AppStateContext";

export default function Cells() {
  const cells = useAppState()?.add_cell ?? {};

  const cells_array = d3
    .sort(Object.entries(cells), (d) => d[0])
    .map((d) => d[1]);

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
