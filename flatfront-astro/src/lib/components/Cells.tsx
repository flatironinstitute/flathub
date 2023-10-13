import CatalogCell from "./CatalogCell";
import * as controller from "../app_state";
import type { CellID } from "../types";

export default function Cells() {
  const cells = controller.useState()?.add_cell ?? [];

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
  return <div>comparison</div>;
}
