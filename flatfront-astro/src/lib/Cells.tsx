import type { Cell } from "./types";

import { Providers } from "./contexts";
import * as hooks from "./hooks";
import * as stores from "./stores";
import CatalogCell from "./CatalogCell";

export default function Cells() {
  const cells = hooks.useStore(stores.cells);

  const is_parent_cell = (
    cell: Cell.Any
  ): cell is Cell.Catalog | Cell.Comparison => {
    return [`catalog`, `comparison`].includes(cell.type);
  };

  return (
    <>
      {cells.filter(is_parent_cell).map((cell) => {
        const component = (() => {
          switch (cell.type) {
            case `catalog`:
              return <CatalogCell />;
            case `comparison`:
              return <ComparisonCell />;
            default:
              cell satisfies never;
          }
        })();
        return (
          <Providers.CellProvider key={cell.cell_id} value={cell}>
            {component}
          </Providers.CellProvider>
        );
      })}
    </>
  );
}

function ComparisonCell() {
  return <div>comparison</div>;
}
