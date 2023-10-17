import type { UseQueryResult } from "@tanstack/react-query";
import React from "react";
import clsx from "clsx";
import * as d3 from "d3";
import * as RadixIcons from "@radix-ui/react-icons";
import * as RadixSlider from "@radix-ui/react-slider";
import * as RadixDialog from "@radix-ui/react-dialog";
import * as RadixSelect from "@radix-ui/react-select";
import * as RadixRadioGroup from "@radix-ui/react-radio-group";
import * as RadixSwitch from "@radix-ui/react-switch";
import * as RadixCheckbox from "@radix-ui/react-checkbox";
import * as RadixSeparator from "@radix-ui/react-separator";
import { useDebounce } from "@uidotdev/usehooks";

export function Separator({ orientation }: RadixSeparator.SeparatorProps) {
  return (
    <RadixSeparator.Root
      className="my-4 h-1 bg-black/40 dark:bg-white/10"
      orientation={orientation}
    />
  );
}

export function CellWrapper({
  children,
  className
}: {
  children: React.ReactNode;
  className?: string;
}): React.JSX.Element {
  return (
    <div
      data-type="CellWrapper"
      className={clsx(
        `w-full rounded p-6`,
        `ring-1 ring-black/30 dark:ring-white/30`,
        className
      )}
    >
      {children}
    </div>
  );
}

export function BigButton({
  children,
  onClick,
  className,
  ...props
}: React.ButtonHTMLAttributes<HTMLButtonElement> & {
  children: React.ReactNode;
}): React.JSX.Element {
  return (
    <button
      data-type="BigButton"
      className={clsx(BigButton.className, className)}
      onClick={onClick}
      {...props}
    >
      {children}
    </button>
  );
}
BigButton.className = clsx(
  `block rounded-lg py-3 font-bold`,
  `ring-1 ring-black dark:ring-white`,
  `focus:outline-none focus-visible:ring-4`,
  `disabled:opacity-50 disabled:cursor-not-allowed`
);

export function CellSection({
  label,
  children = null,
  className
}: {
  label?: string;
  children?: React.ReactNode;
  className?: string;
}): React.JSX.Element {
  return (
    <div data-type="CellSection" className={clsx(`@container`, className)}>
      {label && <SimpleLabel>{label}</SimpleLabel>}
      {children}
    </div>
  );
}

export function SimpleLabel({
  children,
  className
}: {
  children: React.ReactNode;
  className?: string;
}): React.JSX.Element {
  return <div className={clsx(`uppercase`, className)}>{children}</div>;
}

export function Placeholder({
  children,
  className
}: React.HTMLAttributes<HTMLDivElement>) {
  return (
    <div
      className={clsx(
        `grid h-40 place-items-center rounded-lg p-4 opacity-50`,
        `leading-none`,
        `outline-dashed outline-2 outline-slate-700 dark:outline-slate-50`,
        className
      )}
    >
      {children}
    </div>
  );
}

export function Dialog({
  label,
  children,
  buttonClassName,
  disabled = false
}: {
  label: string;
  children: React.ReactNode;
  buttonClassName?: string;
  disabled?: boolean;
}): React.JSX.Element {
  return (
    <RadixDialog.Root>
      <RadixDialog.Trigger
        className={clsx(BigButton.className, buttonClassName)}
        disabled={disabled}
      >
        {label}
      </RadixDialog.Trigger>
      <RadixDialog.Portal>
        <RadixDialog.Overlay className="fixed inset-0 z-20 bg-black/50" />
        <RadixDialog.Content
          className={clsx(
            `fixed z-50 rounded-lg p-4`,
            `max-h-[95dvh] max-w-[95dvw]`,
            `left-[50%] top-[50%] -translate-x-[50%] -translate-y-[50%]`,
            `bg-white dark:bg-black`,
            `ring-1 ring-black dark:ring-white`
          )}
        >
          {children}
          <RadixDialog.Close
            className={clsx(
              "absolute right-3.5 top-3.5 inline-flex items-center justify-center rounded-full p-1",
              "focus:outline-none focus-visible:ring focus-visible:ring-white focus-visible:ring-opacity-75"
            )}
          >
            <RadixIcons.Cross1Icon className="h-4 w-4" />
          </RadixDialog.Close>
        </RadixDialog.Content>
      </RadixDialog.Portal>
    </RadixDialog.Root>
  );
}

export function RangeSliderWithText(props: {
  min: number;
  max: number;
  low: number;
  high: number;
  debounce?: number;
  onValueChange?: (value: [number, number]) => void;
  onLowChange?: (value: number) => void;
  onHighChange?: (value: number) => void;
}) {
  const { min, max, onValueChange, onLowChange, onHighChange, debounce } =
    props;
  const [internal_low, set_internal_low] = React.useState<number>(props.low);
  const [internal_high, set_internal_high] = React.useState<number>(props.high);
  const debounced_low = useDebounce(internal_low, debounce ?? 0);
  const debounced_high = useDebounce(internal_high, debounce ?? 0);

  React.useEffect(() => {
    set_internal_low(props.low);
  }, [props.low]);
  React.useEffect(() => {
    set_internal_high(props.high);
  }, [props.high]);

  React.useEffect(() => {
    onValueChange?.([debounced_low, debounced_high]);
  }, [debounced_low, debounced_high]);
  React.useEffect(() => {
    onLowChange?.(debounced_low);
  }, [debounced_low]);
  React.useEffect(() => {
    onHighChange?.(debounced_high);
  }, [debounced_high]);

  return (
    <div
      data-type="RangeSliderWithText"
      className="mt-[9px] grid grid-cols-2 items-center gap-x-4 gap-y-2"
    >
      <TextInput
        value={internal_low.toString()}
        getValidityMessage={(string) => {
          const number = Number(string);
          if (!Number.isFinite(number)) return `Invalid number`;
          if (number < min) return `Must be greater than ${min.toString()}`;
          if (number > internal_high)
            return `Must be less than ${internal_high.toString()}`;
          return null;
        }}
        onStringInput={(string: string) => {
          const number = Number(string);
          if (!Number.isFinite(number)) return;
          if (string === internal_low.toString()) {
            // Not updating filter because it didn't change
            return;
          }
          set_internal_low(number);
        }}
      />
      <TextInput
        value={internal_high.toString()}
        getValidityMessage={(string) => {
          const number = Number(string);
          if (!Number.isFinite(number)) return `Invalid number`;
          if (number > max) return `Must be less than ${max.toString()}`;
          if (number < internal_low)
            return `Must be greater than ${internal_low.toString()}`;
          return null;
        }}
        onStringInput={(string: string) => {
          const number = Number(string);
          if (!Number.isFinite(number)) return;
          if (string === internal_high.toString()) {
            // Not updating filter because it didn't change
            return;
          }
          set_internal_high(number);
        }}
      />
      <div className="col-span-2">
        <RangeSlider
          min={min}
          max={max}
          value={[internal_low, internal_high]}
          onValueChange={([new_low, new_high]) => {
            if (new_low !== internal_low) {
              set_internal_low(new_low);
            }
            if (new_high !== internal_high) {
              set_internal_high(new_high);
            }
          }}
        />
      </div>
    </div>
  );
}

export function RangeSlider({
  min,
  max,
  value,
  onValueChange
}: RadixSlider.SliderProps) {
  const step = d3.tickStep(min, max, 100);

  const thumb_class = clsx(
    `block h-2 w-2 rounded-full`,
    `bg-black dark:bg-white`,
    `focus:outline-none focus-visible:ring-4 focus-visible:ring-offset-4 focus-visible:ring-black dark:focus-visible:ring-white dark:focus-visible:ring-offset-black`
  );

  return (
    <RadixSlider.Root
      data-type="RangeSlider"
      min={min}
      max={max}
      value={value}
      className="relative flex h-5 w-full cursor-pointer touch-none items-center"
      onValueChange={onValueChange}
      step={step}
    >
      <RadixSlider.Track className="relative h-1 w-full grow rounded-full bg-black/10 dark:bg-white/30">
        <RadixSlider.Range className="absolute h-full rounded-full bg-black/20 dark:bg-white/40" />
      </RadixSlider.Track>
      <RadixSlider.Thumb className={thumb_class} />
      {value.length > 1 && <RadixSlider.Thumb className={thumb_class} />}
    </RadixSlider.Root>
  );
}

export function TextInput({
  value,
  onStringInput = () => null,
  getValidityMessage = () => null,
  ...rest
}: React.InputHTMLAttributes<HTMLInputElement> & {
  value?: string;
  onStringInput?: (value: string) => void;
  getValidityMessage?: (value: string) => string | null;
}) {
  const ref = React.useRef<HTMLInputElement>(null);
  const [internal, set_internal] = React.useState(value ?? ``);

  const on_input = (string: string) => {
    set_internal(string);
    const message = getValidityMessage(string);
    if (message) {
      ref.current.setCustomValidity(message);
    } else {
      ref.current.setCustomValidity("");
      onStringInput(string);
    }
    ref.current.reportValidity();
  };

  React.useEffect(() => {
    on_input(value ?? ``);
  }, [value]);

  return (
    <input
      data-type="TextInput"
      ref={ref}
      type="text"
      className={clsx(
        `block w-full rounded-md border-0 bg-transparent px-2 py-2`,
        `ring-1 ring-inset ring-black invalid:!ring-red-400 dark:ring-white`,
        `focus:outline-none focus-visible:ring-4`
      )}
      value={internal}
      onInput={(event: React.FormEvent<HTMLInputElement>) =>
        on_input(event.currentTarget.value)
      }
      {...rest}
    />
  );
}

export function Label({
  children,
  className
}: {
  children: React.ReactNode;
  className?: string;
}) {
  return (
    <label className={clsx(`block uppercase`, className)}>{children}</label>
  );
}

export function RadioGroup<T extends string>(
  props: RadixRadioGroup.RadioGroupProps & {
    items: Array<{ value: T; text: string }>;
  }
) {
  const { items, ...root_props } = props;

  return (
    <RadixRadioGroup.Root orientation="horizontal" {...root_props}>
      <div className="flex gap-x-4">
        {items.map((item) => (
          <div key={item.value} className="flex items-center gap-x-2">
            <RadixRadioGroup.Item
              id={item.value}
              value={item.value}
              className={clsx(
                `peer relative h-5 w-5 rounded-full`,
                `border border-transparent text-white`,
                `bg-simons-gray-2`,
                `data-[state=unchecked]:opacity-30`,
                `data-[state=checked]:opacity-100`,
                `focus:outline-none focus-visible:ring-2 focus-visible:ring-black focus-visible:ring-offset-2 dark:focus-visible:ring-white`
              )}
            >
              <RadixRadioGroup.Indicator className="leading-0 absolute inset-0 flex items-center justify-center">
                <div className="h-1.5 w-1.5 rounded-full bg-white"></div>
              </RadixRadioGroup.Indicator>
            </RadixRadioGroup.Item>
            <label
              htmlFor={item.value}
              className="block cursor-pointer font-medium"
            >
              {item.text}
            </label>
          </div>
        ))}
      </div>
    </RadixRadioGroup.Root>
  );
}

export function Switch() {
  const size = 15;
  const style = {
    "--size": `${size}px`,
    "--tx": `calc(var(--size) - 2px)`,
    "--oh": `calc(var(--size) + 6px)`,
    "--ow": `calc(var(--size) * 2)`
  } as any;
  return (
    <RadixSwitch.Root
      style={style}
      className={clsx(
        `relative h-[var(--oh)] w-[var(--ow)] cursor-pointer rounded-full`,
        `bg-black outline-none`,
        `focus:shadow-[0_0_0_2px] focus:shadow-black data-[state=checked]:bg-black`
      )}
    >
      <RadixSwitch.Thumb
        className={clsx(
          `block h-[var(--size)] w-[var(--size)] translate-x-[3px]`,
          `rounded-full bg-white`,
          `transition-transform duration-100 will-change-transform data-[state=checked]:translate-x-[var(--tx)]`
        )}
      />
    </RadixSwitch.Root>
  );
}

export function Checkbox(props: RadixCheckbox.CheckboxProps) {
  return (
    <RadixCheckbox.Root
      className={clsx(
        `flex h-5 w-5 appearance-none items-center justify-center rounded-md`,
        `bg-white outline outline-black dark:bg-black dark:outline-white`,
        `data-[disabled]:opacity-20 data-[disabled]:cursor-not-allowed`
      )}
      defaultChecked
      {...props}
    >
      <RadixCheckbox.Indicator>
        <RadixIcons.CheckIcon className="h-4 w-4" />
      </RadixCheckbox.Indicator>
    </RadixCheckbox.Root>
  );
}

export function PlotWrapper({
  children,
  query,
  isLoading = false
}: {
  children: React.ReactNode;
  query: UseQueryResult;
  isLoading?: boolean;
}) {
  let status_box = null;
  if (isLoading || query.isLoading || query.isFetching) {
    status_box = (
      <div
        className={clsx(
          "absolute left-1/2 top-1/2 z-10 rounded-lg bg-white p-4",
          "-translate-x-1/2 -translate-y-full transform",
          "ring-2 ring-black dark:bg-black dark:ring-white"
        )}
      >
        Loading...
      </div>
    );
  }
  return (
    <div className="relative">
      {status_box}
      {children}
    </div>
  );
}

export function Select<T>({
  placeholder,
  options = [],
  value = undefined,
  getKey = (d) => d?.toString(),
  getDisplayName = (d) => d?.toString(),
  getDisabled = (d) => false,
  disabled = false,
  onValueChange = undefined,
  contentPosition = "popper",
  size = `large`
}: {
  placeholder?: string;
  options: T[];
  value?: T;
  getKey?: (option: T) => string;
  getDisplayName?: (option: T) => React.ReactNode;
  getDisabled?: (option: T) => boolean;
  disabled?: boolean;
  onValueChange?: (value: T) => void;
  contentPosition?: RadixSelect.SelectContentProps["position"];
  size?: `small` | `large`;
}) {
  const size_classes =
    size === `large`
      ? { trigger: `py-2 pl-3 pr-4`, icon: `h-5 w-5` }
      : { trigger: `py-1 pl-2 pr-2`, icon: `h-4 w-4` };
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
          `cursor-pointer text-left`,
          `disabled:cursor-wait disabled:opacity-50`,
          `rounded-md ring-1 ring-black dark:ring-white`,
          `focus:outline-none focus-visible:ring-4`,
          size_classes.trigger
        )}
      >
        <RadixSelect.Value placeholder={placeholder}></RadixSelect.Value>
        <RadixSelect.Icon>
          <RadixIcons.ChevronDownIcon className={size_classes.icon} />
        </RadixSelect.Icon>
      </RadixSelect.Trigger>
      <RadixSelect.Portal>
        <RadixSelect.Content position={contentPosition} sideOffset={10}>
          <RadixSelect.Viewport
            className={clsx(
              `max-h-[var(--radix-select-content-available-height)] overflow-scroll`,
              `w-[var(--radix-select-trigger-width)] rounded-md`,
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
                    `focus:bg-black/10 dark:focus:bg-white/40`,
                    `data-[disabled]:pointer-events-none data-[disabled]:opacity-40`,
                    size_classes.trigger
                  )}
                  disabled={getDisabled(option)}
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

type SelectProps = Parameters<typeof Select>[0];

export function Combobox({ ...props }: SelectProps) {
  return <Select {...props} />;
}

// const people = [
//   { id: 1, name: "Wade Cooper" },
//   { id: 2, name: "Arlene Mccoy" },
//   { id: 3, name: "Devon Webb" },
//   { id: 4, name: "Tom Cook" },
//   { id: 5, name: "Tanya Fox" },
//   { id: 6, name: "Hellen Schmidt" }
// ];

// export function Combobox() {
//   const [selected, setSelected] = useState(people[0]);
//   const [query, setQuery] = useState("");

//   const filteredPeople =
//     query === ""
//       ? people
//       : people.filter((person) =>
//           person.name
//             .toLowerCase()
//             .replace(/\s+/g, "")
//             .includes(query.toLowerCase().replace(/\s+/g, ""))
//         );

//   return (
//     <HeadlessCombobox value={selected} onChange={setSelected}>
//       <div className="relative">
//         <div className="relative w-full cursor-default overflow-hidden rounded-lg bg-white text-left shadow-md focus:outline-none focus-visible:ring-2 focus-visible:ring-white focus-visible:ring-opacity-75 focus-visible:ring-offset-2 focus-visible:ring-offset-teal-300 sm:text-sm">
//           <HeadlessCombobox.Input
//             className="w-full border-none py-2 pl-3 pr-10 text-sm leading-5 text-gray-900 focus:ring-0"
//             displayValue={(person: any) => person.name}
//             onChange={(event) => setQuery(event.target.value)}
//           />
//           <HeadlessCombobox.Button className="absolute inset-y-0 right-0 flex items-center pr-2">
//             updon
//           </HeadlessCombobox.Button>
//         </div>

//         <HeadlessCombobox.Options className="absolute mt-1 max-h-60 w-full overflow-auto rounded-md bg-white py-1 text-base shadow-lg ring-1 ring-black ring-opacity-5 focus:outline-none sm:text-sm">
//           {filteredPeople.length === 0 && query !== "" ? (
//             <div className="relative cursor-default select-none px-4 py-2 text-gray-700">
//               Nothing found.
//             </div>
//           ) : (
//             filteredPeople.map((person) => (
//               <HeadlessCombobox.Option
//                 key={person.id}
//                 className={({ active }) =>
//                   `relative cursor-default select-none py-2 pl-10 pr-4 ${
//                     active ? "bg-teal-600 text-white" : "text-gray-900"
//                   }`
//                 }
//                 value={person}
//               >
//                 {({ selected, active }) => (
//                   <>
//                     <span
//                       className={`block truncate ${
//                         selected ? "font-medium" : "font-normal"
//                       }`}
//                     >
//                       {person.name}
//                     </span>
//                     {selected ? (
//                       <span
//                         className={`absolute inset-y-0 left-0 flex items-center pl-3 ${
//                           active ? "text-white" : "text-teal-600"
//                         }`}
//                       >
//                         check
//                       </span>
//                     ) : null}
//                   </>
//                 )}
//               </HeadlessCombobox.Option>
//             ))
//           )}
//         </HeadlessCombobox.Options>
//       </div>
//     </HeadlessCombobox>
//   );
// }
