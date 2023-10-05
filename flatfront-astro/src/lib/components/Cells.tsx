import type { Cell } from "../types";

import React from "react";
import { Providers } from "../contexts";
import * as hooks from "../hooks";
import CatalogCell from "./CatalogCell";

export default function Cells() {
  const actions = hooks.useActions();

  const cells = React.useMemo(() => {
    let _cells: Cell.Any[] = [];
    for (const action of actions) {
      switch (action.type) {
        case `add_catalog_cell`:
          _cells.push({
            type: `catalog`,
            id: action.catalog_cell_id
          });
          break;
        case `add_comparison_cell`:
          _cells.push({
            type: `comparison`,
            id: action.comparison_cell_id
          });
          break;
        case `remove_cell`:
          _cells = _cells.filter((cell) => cell.id !== action.cell_id);
          break;
      }
    }
    return _cells;
  }, [actions]);

  return (
    <>
      {cells.map((cell) => {
        const component = (() => {
          switch (cell.type) {
            case `catalog`:
              return (
                <Providers.CatalogCellIDProvider key={cell.id} value={cell.id}>
                  <CatalogCell />
                </Providers.CatalogCellIDProvider>
              );
            case `comparison`:
              return <ComparisonCell />;
            default:
              cell satisfies never;
          }
        })();
        return component;
      })}
    </>
  );
}

function ComparisonCell() {
  return <div>comparison</div>;
}
