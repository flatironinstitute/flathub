import type { DarkModeValue } from "../types";

import React from "react";
import { useDarkModeValue } from "../dark-mode";
import { CellWrapper, BigButton, RadioGroup } from "./Primitives";
import { useAppState, useMergeState } from "../contexts/AppStateContext";

export default function GlobalControls(): React.JSX.Element {
  const cells_object = useAppState()?.add_cell ?? {};
  const number_of_cells = Object.keys(cells_object).length ?? 0;
  const merge_state = useMergeState();
  return (
    <CellWrapper className="mx-auto grid max-w-[400px] items-center gap-y-4">
      <BigButton
        className="max-w-[400px]"
        onClick={() => {
          merge_state({
            add_cell: {
              [number_of_cells]: {
                type: `catalog`,
                id: `catalog_cell_${number_of_cells}`
              }
            }
          });
        }}
      >
        Add Catalog
      </BigButton>
      <DarkModeSelect />
    </CellWrapper>
  );
}

function DarkModeSelect() {
  const current_value = useDarkModeValue();

  const merge_state = useMergeState();

  const on_change = (value: DarkModeValue) => {
    merge_state({ dark_mode: value });
  };

  return (
    <div data-type="DarkModeSelect" className="flex h-20 items-center gap-x-4">
      <div>Dark Mode</div>
      <RadioGroup
        value={current_value}
        onValueChange={on_change}
        items={[
          { value: `system` as DarkModeValue, text: `System` },
          { value: `light` as DarkModeValue, text: `Light` },
          { value: `dark` as DarkModeValue, text: `Dark` }
        ]}
      />
    </div>
  );
}
