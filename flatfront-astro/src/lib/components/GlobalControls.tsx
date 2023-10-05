import type { DarkModeValue } from "../types";

import React from "react";
import * as hooks from "../hooks";
import { dispatch_action, log } from "../shared";
import { CellWrapper, BigButton, RadioGroup } from "./Primitives";

export default function GlobalControls(): React.JSX.Element {
  return (
    <CellWrapper className="grid items-center gap-y-4">
      <BigButton
        onClick={() => {
          dispatch_action({
            type: `add_catalog_cell`,
            catalog_cell_id: `catalog_cell_${Date.now()}`
          });
        }}
      >
        Add Catalog
      </BigButton>
      <BigButton
        disabled
        onClick={() => {
          dispatch_action({
            type: `add_comparison_cell`,
            comparison_cell_id: `comparison_cell_${Date.now()}`
          });
        }}
      >
        Add Comparison
      </BigButton>
      {/* <DarkModeToggle /> */}
      <DarkModeSelect />
    </CellWrapper>
  );
}

function DarkModeSelect() {
  const current_value = hooks.useDarkModeValue();

  const on_change = (value: DarkModeValue) => {
    log(`DarkModeSelect.on_change`, { value });
    dispatch_action({
      type: `set_dark_mode`,
      value
    });
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
