import CatalogCell from "./CatalogCell";
import * as controller from "../contexts/AppStateContext";
import type { CellID } from "../types";

export default function Cells() {
  const cells = controller.useAppState()?.add_cell ?? [];

  return (
    <>
      {cells.map((cell) => {
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
