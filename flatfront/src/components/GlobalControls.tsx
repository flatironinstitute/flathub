import * as d3 from "d3";
import { useQuery } from "@tanstack/react-query";
import type { CellID, TopResponse, TopResponseEntry } from "@/types";
import { fetch_api_get } from "@/utils";
import { CardContent, Card } from "@/components/ui/card";
import { Button } from "@/components/ui/button";
import { DarkModeToggle } from "./DarkModeToggle";
import { useAppState, useMergeState } from "./contexts/AppStateContext";
import {
  Popover,
  PopoverClose,
  PopoverContent,
  PopoverTrigger
} from "./ui/popover";

export function GlobalControls() {
  return (
    <Card>
      <CardContent className="grid gap-4 p-4 sm:grid-cols-2">
        <CatalogSelect />
        <DarkModeToggle />
      </CardContent>
    </Card>
  );
}

export function CatalogSelect() {
  const catalog_list_query = useQuery({
    queryKey: ["top"],
    queryFn: async (): Promise<TopResponse> => fetch_api_get<TopResponse>(`/`)
  });

  const catalog_list_unsorted = catalog_list_query.data ?? [];
  const catalog_list = d3.sort(
    catalog_list_unsorted,
    (d: TopResponseEntry) => d?.title
  );

  const cells_object = useAppState()?.cells ?? {};
  const number_of_cells = Object.keys(cells_object).length ?? 0;
  const next_catalog_cell_id: CellID.Catalog = `catalog_cell_${number_of_cells}`;

  const merge_state = useMergeState();

  return (
    <Popover>
      <PopoverTrigger asChild>
        <Button>Add Catalog</Button>
      </PopoverTrigger>
      <PopoverContent className="w-80" align="start" avoidCollisions={false}>
        <div className="grid gap-4">
          {catalog_list.map(({ name, title }) => (
            <PopoverClose key={name} asChild>
              <Button
                variant="link"
                className="h-5 justify-start p-0"
                onClick={() =>
                  merge_state({
                    cells: {
                      [next_catalog_cell_id]: {
                        type: `catalog`,
                        id: next_catalog_cell_id,
                        catalog_id: name
                      }
                    }
                  })
                }
              >
                {title}
              </Button>
            </PopoverClose>
          ))}
        </div>
      </PopoverContent>
    </Popover>
  );
}
