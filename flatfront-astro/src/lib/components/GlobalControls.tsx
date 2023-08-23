import type { TopResponseEntry } from "../types";

import React from "react";

import { Switch } from "@headlessui/react";
import * as d3 from "d3";

import * as stores from "../stores";
import {
  dispatch_action,
  LabeledSelect,
  log,
  useStore,
  CellWrapper,
  BigButton,
} from "../shared";

export default function GlobalControls(): React.JSX.Element {
  return (
    <CellWrapper>
      <div className="grid gap-y-4 items-center">
        <BigButton
          onClick={() => {
            dispatch_action({
              type: `add_cell`,
              cell_id: `cell_${Date.now()}`,
            });
          }}
        >
          Add Panel
        </BigButton>
        <DarkModeToggle />
      </div>
    </CellWrapper>
  );
}

function CatalogSelect() {
  const [selected, set_selected] = React.useState<TopResponseEntry | undefined>(
    undefined
  );
  const get_title = (d: TopResponseEntry) => d?.title;
  const catalog_list_unsorted = useStore(stores.top_response).data ?? [];
  const ready = catalog_list_unsorted.length > 0;
  const catalog_list = d3.sort(catalog_list_unsorted, get_title);
  return (
    <div data-type="CatalogSelect">
      <LabeledSelect
        disabled={!ready}
        label="Add Catalog"
        placeholder={ready ? "Select a catalog..." : "Loading catalogs..."}
        options={catalog_list}
        getKey={(d) => d?.name}
        getDisplayName={get_title}
        value={selected}
        onValueChange={(d) => {
          set_selected(d);
        }}
        button
        buttonText="Add"
        onClick={() => {
          if (!selected) return;
          dispatch_action({
            type: `add_catalog_cell`,
            cell_id: `catalog_cell_${selected.name}`,
            catalog_id: selected.name,
          });
        }}
        buttonClassName="bg-light-3 dark:bg-dark-3"
        optionsClassName="bg-light-3 dark:bg-dark-3"
        optionClassName="ui-active:bg-light-4 dark:ui-active:bg-dark-4"
      />
    </div>
  );
}

function DarkModeToggle() {
  const [dark_mode, set_dark_mode] = React.useState(true);

  React.useEffect(() => {
    if (dark_mode) {
      document.documentElement.classList.add("dark");
    } else {
      document.documentElement.classList.remove("dark");
    }
  }, [dark_mode]);

  return (
    <div data-type="DarkModeToggle" className="flex items-center gap-x-4">
      <div>Dark Mode</div>
      <div>
        <Switch
          checked={dark_mode}
          onChange={set_dark_mode}
          className={`bg-light-3 dark:bg-dark-3 relative inline-flex h-8 w-14 items-center rounded-full`}
        >
          <span
            className={`${
              dark_mode ? "translate-x-7" : "translate-x-1"
            } inline-block h-6 w-6 transform rounded-full bg-light-4 dark:bg-dark-4 transition`}
          />
        </Switch>
      </div>
    </div>
  );
}
