import { CardContent, Card } from "@/components/ui/card";
import { Button } from "@/components/ui/button";
import { DarkModeToggle } from "./DarkModeToggle";
import { useAppState, useMergeState } from "./contexts/AppStateContext";

export function GlobalControls() {
  return (
    <Card>
      <CardContent className="flex flex-col items-center justify-center gap-4 p-4">
        <AddCatalogButton />
        <DarkModeToggle />
      </CardContent>
    </Card>
  );
}

function AddCatalogButton() {
  const cells_object = useAppState()?.cells ?? {};
  const number_of_cells = Object.keys(cells_object).length ?? 0;
  const merge_state = useMergeState();

  return (
    <Button
      variant="outline"
      onClick={() =>
        merge_state({
          cells: {
            [number_of_cells]: {
              type: `catalog`,
              id: `catalog_cell_${number_of_cells}`
            }
          }
        })
      }
    >
      Add Catalog
    </Button>
  );
}
