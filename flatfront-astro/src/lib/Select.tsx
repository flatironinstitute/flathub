import React from "react";
import clsx from "clsx";
import * as RadixIcons from "@radix-ui/react-icons";
import * as RadixSelect from "@radix-ui/react-select";

export default function Select<T>({
  placeholder,
  options,
  getKey = (d) => d?.toString(),
  getDisplayName = (d) => d?.toString(),
  disabled = false,
  value = undefined,
  onValueChange = undefined
}: {
  placeholder?: string;
  options: T[];
  getKey?: (option: T) => string;
  getDisplayName?: (option: T) => React.ReactNode;
  disabled?: boolean;
  value?: T;
  onValueChange?: (value: T) => void;
}) {
  return (
    <RadixSelect.Root
      data-type="Select"
      disabled={disabled}
      value={value ? getKey(value) : undefined}
      onValueChange={(key) => {
        const option = options.find((option) => getKey(option) === key);
        if (option === undefined) {
          throw new Error(`Could not find option for key: ${key}`);
        }
        onValueChange?.(option);
      }}
    >
      <RadixSelect.Trigger
        className={clsx(
          `relative flex w-full items-center justify-between`,
          `cursor-pointer py-3 pl-3 pr-4 text-left`,
          `disabled:cursor-wait disabled:opacity-50`,
          `rounded-md ring-1 ring-black dark:ring-white`,
          `focus:outline-none focus-visible:ring-4`
        )}
      >
        <RadixSelect.Value placeholder={placeholder}></RadixSelect.Value>
        <RadixSelect.Icon>
          <RadixIcons.ChevronDownIcon className="h-6 w-6" />
        </RadixSelect.Icon>
      </RadixSelect.Trigger>
      <RadixSelect.Portal>
        <RadixSelect.Content position="popper" sideOffset={10}>
          <RadixSelect.Viewport
            className={clsx(
              `w-[var(--radix-select-trigger-width)] rounded-lg`,
              `bg-white shadow-2xl dark:bg-black`,
              `ring-1 ring-black dark:ring-white`
            )}
          >
            {options.map((option) => {
              const key = getKey(option);
              return (
                <RadixSelect.Item
                  key={key}
                  value={key}
                  className={clsx(
                    `cursor-pointer select-none focus:outline-none`,
                    `py-2 pl-3`,
                    `focus:bg-black/10 dark:focus:bg-white/40`
                  )}
                >
                  <RadixSelect.ItemText>
                    {getDisplayName(option)}
                  </RadixSelect.ItemText>
                </RadixSelect.Item>
              );
            })}
          </RadixSelect.Viewport>
        </RadixSelect.Content>
      </RadixSelect.Portal>
    </RadixSelect.Root>
  );
}
