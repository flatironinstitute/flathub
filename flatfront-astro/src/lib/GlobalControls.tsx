import React from "react";
import clsx from "clsx";

import * as RadixSwitch from "@radix-ui/react-switch";

import { dispatch_action, hooks, log } from "./shared";
import { CellWrapper, BigButton } from "./Primitives";

export default function GlobalControls(): React.JSX.Element {
  return (
    <CellWrapper className="grid items-center gap-y-4">
      <BigButton
        onClick={() => {
          dispatch_action({
            type: `add_catalog_cell`,
            cell_id: `catalog_cell_${Date.now()}`
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
            cell_id: `comparison_cell_${Date.now()}`
          });
        }}
      >
        Add Comparison
      </BigButton>
      <DarkModeToggle />
    </CellWrapper>
  );
}

function DarkModeToggle() {
  const dark_mode = hooks.useIsDarkMode();

  const set_dark_mode = (value: boolean) => {
    if (value) {
      document.documentElement.classList.add("dark");
    } else {
      document.documentElement.classList.remove("dark");
    }
  };

  return (
    <div data-type="DarkModeToggle" className="flex h-20 items-center gap-x-4">
      <div>Dark Mode</div>
      <RadixSwitch.Root
        className={clsx(
          `relative h-[30px] w-[50px] cursor-pointer rounded-full`,
          `transition-colors duration-200 ease-in-out`,
          `data-[state=checked]:bg-simons-gray-1/100 data-[state=unchecked]:bg-simons-gray-1/50`,
          `focus:outline-none focus-visible:ring-4 focus-visible:ring-white`
        )}
        checked={dark_mode}
        onCheckedChange={set_dark_mode}
      >
        <RadixSwitch.Thumb
          className={clsx(
            `pointer-events-none absolute block h-[20px] w-[20px] rounded-full bg-white shadow-lg ring-0`,
            `-translate-y-1/2 transform`,
            `transition-all duration-200 ease-in-out`,
            `data-[state=checked]:left-[calc(100%-25px)] data-[state=unchecked]:left-[5px]`
          )}
        />
      </RadixSwitch.Root>
    </div>
  );
}
