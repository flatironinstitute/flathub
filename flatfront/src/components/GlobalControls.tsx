import * as d3 from "d3";
import { useQuery } from "@tanstack/react-query";
import { RotateCw } from "lucide-react";
import type { CatalogCellID, TopResponse, TopResponseEntry } from "@/types";
import { fetch_api_get } from "@/utils";
import { CardContent, Card } from "@/components/ui/card";
import { Button } from "@/components/ui/button";
import { DarkModeToggle } from "./DarkModeToggle";
import { useMergeState } from "./contexts/AppStateContext";
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

  const next_catalog_cell_id: CatalogCellID = `catalog_cell_${Date.now()}`;

  const merge_state = useMergeState();

  const loading = catalog_list_query.isLoading;

  const loading_text = (
    <>
      <RotateCw className="mr-2 h-4 w-4 animate-spin" />
      Loading
    </>
  );

  return (
    <Popover>
      <PopoverTrigger asChild>
        <Button disabled={loading}>
          {loading ? loading_text : `Add Catalog`}
        </Button>
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
                        cell_id: next_catalog_cell_id,
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
