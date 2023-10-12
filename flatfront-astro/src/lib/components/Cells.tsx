import { Providers } from "../contexts";
import CatalogCell from "./CatalogCell";
import * as controller from "./AppController";

export default function Cells() {
  const cells = controller.useState()?.add_cell ?? [];

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
