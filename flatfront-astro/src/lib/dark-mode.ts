import type { DarkModeValue } from "./types";

import React from "react";
import * as controller from "./app-state";
import { log } from "./shared";

export function useDarkModeValue(): DarkModeValue {
  const dark_mode_value = controller.useAppState()?.dark_mode;
  return dark_mode_value ?? `system`;
}

function useSystemDarkMode(): boolean {
  const [is_dark_mode, set_is_dark_mode] = React.useState<boolean>(() => {
    const system_dark_mode = window.matchMedia(
      "(prefers-color-scheme: dark)"
    ).matches;
    return system_dark_mode;
  });
  React.useEffect(() => {
    const system_dark_mode = window.matchMedia("(prefers-color-scheme: dark)");
    const listener = (event: MediaQueryListEvent) => {
      log(`System dark mode changed:`, event.matches);
      set_is_dark_mode(event.matches);
    };
    system_dark_mode.addEventListener("change", listener);
    return () => {
      system_dark_mode.removeEventListener("change", listener);
    };
  }, []);
  return is_dark_mode;
}

export function useIsDarkMode(): boolean {
  const dark_mode_value = useDarkModeValue();
  const system_dark_mode = useSystemDarkMode();
  if (dark_mode_value === `system`) return system_dark_mode;
  const is_dark_mode = dark_mode_value === `dark`;
  return is_dark_mode;
}

export function useToggleDarkMode(): void {
  const is_dark_mode = useIsDarkMode();
  React.useEffect(() => {
    const has_dark_mode_class =
      document.documentElement.classList.contains("dark");
    if (is_dark_mode && !has_dark_mode_class) {
      document.documentElement.classList.add("dark");
    } else if (!is_dark_mode && has_dark_mode_class) {
      document.documentElement.classList.remove("dark");
    }
  }, [is_dark_mode]);
}
