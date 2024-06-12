import * as d3 from "d3";
import { useQuery } from "@tanstack/react-query";
import { RotateCw } from "lucide-react";
import type {
  CatalogCell,
  CatalogCellID,
  CatalogID,
  TopResponse,
  TopResponseEntry
} from "@/types";
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
      <CardContent className="grid items-center gap-4 p-4 sm:grid-cols-3">
        <div className="flex items-center gap-4">
          <img src="./icon.png" alt="FlatHUB" className="h-12 w-12" />
          <h1 className="scroll-m-20 text-3xl font-semibold tracking-tight">
            FlatHUB
          </h1>
        </div>
        <CatalogSelect />
        <DarkModeToggle />
      </CardContent>
    </Card>
  );
}

export function get_new_catalog_cell(catalog_id: CatalogID): CatalogCell {
  const next_catalog_cell_id: CatalogCellID = `catalog_cell_${Date.now()}`;
  return {
    cell_id: next_catalog_cell_id,
    catalog_id
  };
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
          {catalog_list.map(({ name: catalog_id, title }) => (
            <PopoverClose key={catalog_id} asChild>
              <Button
                variant="link"
                className="h-5 justify-start p-0"
                onClick={() => {
                  const new_catalog_cell = get_new_catalog_cell(catalog_id);
                  return merge_state({
                    cells: {
                      [new_catalog_cell.cell_id]: new_catalog_cell
                    }
                  });
                }}
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
