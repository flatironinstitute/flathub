import React from "react";
import * as d3 from "d3";
import { useQuery } from "@tanstack/react-query";
import type { CatalogID, CellID, TopResponse, TopResponseEntry } from "@/types";
import { fetch_api_get } from "@/utils";
import {
  Select,
  SelectContent,
  SelectGroup,
  SelectItem,
  SelectTrigger,
  SelectValue
} from "@/components/ui/select";

import { CardContent, Card } from "@/components/ui/card";
import { Button } from "@/components/ui/button";
import { DarkModeToggle } from "./DarkModeToggle";
import { useAppState, useMergeState } from "./contexts/AppStateContext";
import { AboutThisCatalog } from "./AboutThisCatalog";

export function GlobalControls() {
  return (
    <Card>
      <CardContent className="flex flex-col items-center justify-center gap-4 p-4">
        <div>
          <CatalogSelect />
        </div>
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
  const ready = catalog_list.length > 0;

  const [catalog_id, set_catalog_id] = React.useState<CatalogID | undefined>(
    undefined
  );

  const items = catalog_list.map(({ name, title }) => {
    return (
      <SelectItem key={name} value={name}>
        {title}
      </SelectItem>
    );
  });

  const dropdown = (
    <Select
      disabled={!ready}
      value={catalog_id}
      onValueChange={(catalog_id) => set_catalog_id(catalog_id)}
    >
      <SelectTrigger className="max-w-[40ch] disabled:cursor-wait">
        <SelectValue
          placeholder={ready ? `Select a catalog...` : `Loading catalogs...`}
        />
      </SelectTrigger>
      <SelectContent position="popper">
        <SelectGroup>{items}</SelectGroup>
      </SelectContent>
    </Select>
  );

  const about_this_catalog = <AboutThisCatalog catalog_id={catalog_id} />;

  const cells_object = useAppState()?.cells ?? {};
  const number_of_cells = Object.keys(cells_object).length ?? 0;
  const next_catalog_cell_id: CellID.Catalog = `catalog_cell_${number_of_cells}`;

  const merge_state = useMergeState();

  const add_catalog_button = (
    <Button
      disabled={!catalog_id}
      variant="outline"
      onClick={() =>
        merge_state({
          cells: {
            [next_catalog_cell_id]: {
              type: `catalog`,
              id: next_catalog_cell_id,
              catalog_id
            }
          }
        })
      }
    >
      Add Catalog
    </Button>
  );

  return (
    <div className="grid gap-4 md:grid-cols-3">
      {dropdown}
      {about_this_catalog}
      {add_catalog_button}
    </div>
  );
}
