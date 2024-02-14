import * as React from "react";
import { Moon, Sun } from "lucide-react";

import { Button } from "@/components/ui/button";
import {
  DropdownMenu,
  DropdownMenuContent,
  DropdownMenuItem,
  DropdownMenuTrigger
} from "@/components/ui/dropdown-menu";

export function useIsDarkMode(): boolean {
  const [isDarkMode, setIsDarkMode] = React.useState<boolean>(false);
  React.useEffect(() => {
    const do_it = () => {
      setIsDarkMode(document.documentElement.classList.contains("dark"));
    };
    do_it();
    // Add a mutation observer to the <html> element to detect changes to the
    // "dark" class. This is necessary because the "dark" class is added
    // asynchronously by the DarkModeToggle component.
    const observer = new MutationObserver(do_it);
    observer.observe(document.documentElement, {
      attributes: true,
      attributeFilter: ["class"]
    });
  }, []);
  return isDarkMode;
}

export function DarkModeToggle() {
  const [theme, setThemeState] = React.useState<
    "theme-light" | "dark" | "system"
  >("theme-light");

  React.useEffect(() => {
    const isDarkMode = document.documentElement.classList.contains("dark");
    setThemeState(isDarkMode ? "dark" : "theme-light");
  }, []);

  React.useEffect(() => {
    const is_dark =
      theme === "dark" ||
      (theme === "system" &&
        window.matchMedia("(prefers-color-scheme: dark)").matches);
    document.documentElement.classList[is_dark ? "add" : "remove"]("dark");
  }, [theme]);

  const sun = <Sun className="h-[1.2rem] w-[1.2rem]" />;

  const moon = <Moon className="h-[1.2rem] w-[1.2rem]" />;

  const icon = theme === "dark" ? moon : sun;

  return (
    <DropdownMenu>
      <DropdownMenuTrigger asChild>
        <Button variant="outline" className="flex gap-x-1">
          {icon}
          <span>Toggle Theme</span>
        </Button>
      </DropdownMenuTrigger>
      <DropdownMenuContent align="end">
        <DropdownMenuItem onClick={() => setThemeState("theme-light")}>
          Light
        </DropdownMenuItem>
        <DropdownMenuItem onClick={() => setThemeState("dark")}>
          Dark
        </DropdownMenuItem>
        <DropdownMenuItem onClick={() => setThemeState("system")}>
          System
        </DropdownMenuItem>
      </DropdownMenuContent>
    </DropdownMenu>
  );
}
