import type { DarkModeValue } from "../types";

import React from "react";
import * as controller from "../app-state";
import { useDarkModeValue } from "../dark-mode";
import { CellWrapper, BigButton, RadioGroup } from "./Primitives";

export default function GlobalControls(): React.JSX.Element {
  const number_of_cells = controller.useAppState()?.add_cell?.length ?? 0;
  const dispatch = controller.useDispatch();
  return (
    <CellWrapper className="grid items-center gap-y-4">
      <BigButton
        onClick={() => {
          dispatch([`add_cell`, number_of_cells.toString()], {
            type: `catalog`,
            id: `catalog_cell_${number_of_cells}`
          });
        }}
      >
        Add Catalog
      </BigButton>
      <BigButton disabled onClick={() => {}}>
        Add Comparison
      </BigButton>
      <DarkModeSelect />
    </CellWrapper>
  );
}

function DarkModeSelect() {
  const current_value = useDarkModeValue();

  const dispatch = controller.useDispatch();

  const on_change = (value: DarkModeValue) => {
    dispatch([`dark_mode`], value);
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
