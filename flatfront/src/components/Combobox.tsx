import React from "react";
import { ChevronDown } from "lucide-react";
import { Button } from "@/components/ui/button";
import {
  Popover,
  PopoverContent,
  PopoverTrigger
} from "@/components/ui/popover";
import {
  Command,
  CommandEmpty,
  CommandGroup,
  CommandInput,
  CommandItem,
  CommandList
} from "@/components/ui/command";

export function Combobox({
  placeholder,
  items,
  autoClose: auto_close = false,
  searchText: search_text = "Search...",
  emptyText: empty_text = "No items found."
}: {
  placeholder: string;
  items: {
    key: string;
    value: string;
    label: React.ReactNode;
    onSelect: () => void;
  }[];
  autoClose?: boolean;
  searchText?: string;
  emptyText?: string;
}) {
  const [open, set_open] = React.useState<boolean>(false);

  return (
    <Popover open={open} onOpenChange={set_open}>
      <PopoverTrigger asChild>
        <Button
          variant="outline"
          role="combobox"
          aria-expanded={open}
          className="w-[min(100%,40ch)] justify-between"
        >
          {placeholder}
          <ChevronDown className="ml-2 h-4 w-4 shrink-0 opacity-50" />
        </Button>
      </PopoverTrigger>
      <PopoverContent
        side="bottom"
        align="start"
        avoidCollisions={false}
        className="p-0"
      >
        <Command
          filter={(value, search) => {
            if (value.includes(search.toLowerCase())) return 1;
            return 0;
          }}
        >
          <CommandInput placeholder={search_text} />
          <CommandList>
            <CommandEmpty>{empty_text}</CommandEmpty>
            <CommandGroup>
              {items.map(({ key, value, label, onSelect }) => (
                <CommandItem
                  key={key}
                  value={value}
                  data-value={value}
                  onSelect={() => {
                    onSelect();
                    if (auto_close) set_open(false);
                  }}
                  className="flex gap-x-2"
                >
                  {label}
                </CommandItem>
              ))}
            </CommandGroup>
          </CommandList>
        </Command>
      </PopoverContent>
    </Popover>
  );
}
