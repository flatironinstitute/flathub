import type { Readable } from "svelte/store";
import type {
  Action,
  CellID,
  CatalogMetadataWrapper,
  Filters,
  Datum,
} from "./types";

import React from "react";
import clsx from "clsx";
import { get } from "svelte/store";
import { ChevronDownIcon } from "@radix-ui/react-icons";
import { produce } from "immer";
import { Listbox, Transition } from "@headlessui/react";
import * as stores from "./stores";

export function log(...args: any[]) {
  console.log(`🌔`, ...args);
}

export function dispatch_action(action: Action) {
  stores.actions.update(($actions) => [...$actions, action]);
}

export function set_filter_value(
  cell_id: CellID,
  filter_name: string,
  filter_value: {
    gte: number;
    lte: number;
  }
) {
  stores.filter_state.update((fitler_state_object) => {
    return produce(fitler_state_object, (draft) => {
      draft[cell_id] = draft[cell_id] || {};
      draft[cell_id][filter_name] = filter_value;
    });
  });
}

export function useContextHelper<T>(debug?: string) {
  const context = React.createContext<T | null>(null);

  const useContext = (): T => {
    const value: T | null = React.useContext(context);
    if (value === null) {
      throw new Error(`useContextHelper: value is null`);
    }
    return value;
  };

  return [useContext, context.Provider] as const;
}

export function useStore<T>(store: Readable<T>) {
  const [state, setState] = React.useState<T>(get(store));

  React.useEffect(
    () =>
      store.subscribe((value) => {
        setState(value);
      }),
    [store]
  );

  return state;
}

export const [useCellID, CellIDProvider] = useContextHelper<CellID>();
export const [useParentCellID, ParentCellIDProvider] =
  useContextHelper<CellID>();
export const [usePlotID, PlotIDProvider] = useContextHelper<string>();
export const [useCatalogName, CatalogNameProvider] = useContextHelper<string>();
// prettier-ignore
export const [useCatalogMetadata, CatalogMetadataProvider] = useContextHelper<CatalogMetadataWrapper | undefined>();
// prettier-ignore
export const [useCellFilters, CellFiltersProvider] = useContextHelper<Filters>();
export const [useFieldName, FieldNameProvider] = useContextHelper<string>();
export const [useData, DataProvider] = useContextHelper<Datum[]>();

export function LabeledSelect<T>({
  button,
  buttonText,
  onClick,
  disabled,
  ...select_props
}: Parameters<typeof Select<T>>[0] & {
  button?: boolean;
  buttonText?: string;
  onClick?: () => void;
}): React.JSX.Element {
  return (
    <div className="grid gap-x-4 gap-y-2 items-center grid-cols-1 md:grid-cols-[max-content_1fr_max-content]">
      <Select {...select_props} disabled={disabled} />
      {button && (
        <button
          className="bg-light-2 dark:bg-dark-2 px-4 py-2 rounded-lg disabled:opacity-50"
          onClick={onClick}
          disabled={disabled}
        >
          {buttonText}
        </button>
      )}
    </div>
  );
}

// function Select({
//   label,
//   placeholder,
//   options,
//   value = undefined,
//   onValueChange = undefined,
// }: {
//   label: string;
//   placeholder: SelectValueProps["placeholder"];
//   options: string[];
//   value?: SelectProps["value"];
//   onValueChange?: SelectProps["onValueChange"];
// }): React.JSX.Element {
//   return (
//     <SelectPrimitive.Root value={value} onValueChange={onValueChange}>
//       <SelectPrimitive.Trigger
//         aria-label={label}
//         className="bg-light-2 dark:bg-dark-2 rounded-lg py-2"
//       >
//         <SelectPrimitive.Value placeholder={placeholder} aria-label={label}>
//           {value}
//         </SelectPrimitive.Value>
//       </SelectPrimitive.Trigger>
//       <SelectPrimitive.Portal>
//         <SelectPrimitive.Content position="popper">
//           <SelectPrimitive.Viewport className="bg-light-3 dark:bg-dark-3 p-2 rounded-lg shadow-lg">
//             {options.map((option) => {
//               return (
//                 <SelectPrimitive.Item
//                   value={option}
//                   key={option}
//                   className={clsx(
//                     `relative flex items-center`,
//                     `px-4 py-2 rounded-md`,
//                     `text-sm text-light-text dark:text-dark-text font-medium`,
//                     `focus:bg-light-0 dark:focus:bg-dark-0 focus:outline-none select-none`
//                   )}
//                 >
//                   <SelectPrimitive.ItemText>{option}</SelectPrimitive.ItemText>
//                 </SelectPrimitive.Item>
//               );
//             })}
//           </SelectPrimitive.Viewport>
//         </SelectPrimitive.Content>
//       </SelectPrimitive.Portal>
//     </SelectPrimitive.Root>
//   );
// }

function Select<T>({
  label,
  placeholder,
  options,
  getKey,
  getDisplayName,
  disabled = false,
  value = undefined,
  onValueChange = undefined,
}: {
  label: string;
  placeholder: string;
  options: T[];
  getKey: (option: T) => string;
  getDisplayName: (option: T) => string;
  disabled?: boolean;
  value?: T;
  onValueChange?: (value: T) => void;
}) {
  return (
    <Listbox
      value={value ?? ({} as T)}
      onChange={onValueChange}
      disabled={disabled}
    >
      <Listbox.Label>{label}</Listbox.Label>
      <div className="relative">
        <Listbox.Button
          className={clsx(
            `relative w-full cursor-pointer rounded-md shadow-md`,
            `bg-light-2 dark:bg-dark-2 py-2 pl-3 pr-10 text-left disabled:opacity-50 disabled:cursor-wait`
          )}
        >
          <span className="block">{getDisplayName(value) ?? placeholder}</span>
          <span className="pointer-events-none absolute inset-y-0 right-0 flex items-center pr-2">
            <ChevronDownIcon
              className="h-5 w-5 text-light-text dark:text-dark-text"
              aria-hidden="true"
            />
          </span>
        </Listbox.Button>
        <Listbox.Options
          className={clsx(
            `absolute z-50 w-full mt-1 py-1 shadow-lg overflow-auto rounded-md`,
            `bg-light-2 dark:bg-dark-2`
          )}
        >
          {options.map((option) => (
            <Listbox.Option
              key={getKey(option)}
              value={option}
              className={clsx(
                `relative cursor-pointer select-none py-2 pl-3 pr-4`,
                `ui-active:bg-light-3 dark:ui-active:bg-dark-3`
              )}
            >
              {getDisplayName(option)}
            </Listbox.Option>
          ))}
        </Listbox.Options>
      </div>
    </Listbox>
  );
}

// className={({ active }) =>
// `relative cursor-pointer select-none py-2 pl-3 pr-4 ${
//   active ? `bg-slate-700` : ``
// }`
// }

// const SelectItem = React.forwardRef(
//   ({ children, className, ...props }, forwardedRef) => {
//     return (
//       <Select.Item {...props} ref={forwardedRef}>
//         <Select.ItemText>{children}</Select.ItemText>
//         <Select.ItemIndicator className="SelectItemIndicator">
//           <CheckIcon />
//         </Select.ItemIndicator>
//       </Select.Item>
//     );
//   }
// );
