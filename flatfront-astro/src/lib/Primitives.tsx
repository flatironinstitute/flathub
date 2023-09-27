import React from "react";
import clsx from "clsx";
import * as d3 from "d3";
import * as RadixSlider from "@radix-ui/react-slider";
import * as RadixDialog from "@radix-ui/react-dialog";
import * as RadixIcons from "@radix-ui/react-icons";
import * as RadixSelect from "@radix-ui/react-select";
import * as RadixRadioGroup from "@radix-ui/react-radio-group";

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
  `block rounded-lg py-3 font-bold text-xl`,
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
    <div data-type="CellSection" className={className}>
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
  return <div className={clsx(`text-sm uppercase`, className)}>{children}</div>;
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

export function RangeSlider({
  min,
  max,
  value,
  onValueChange
}: {
  min: number;
  max: number;
  value: number[];
  onValueChange: (value: number[]) => void;
}) {
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
    <label className={clsx(`block text-xs uppercase`, className)}>
      {children}
    </label>
  );
}

export function Select<T>({
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
              className="block cursor-pointer text-sm font-medium"
            >
              {item.text}
            </label>
          </div>
        ))}
      </div>
    </RadixRadioGroup.Root>
  );
}

// function DarkModeToggle() {
//   const dark_mode = hooks.useIsDarkMode();

//   // const set_dark_mode = (value: boolean) => {
//   //   if (value) {
//   //     document.documentElement.classList.add("dark");
//   //   } else {
//   //     document.documentElement.classList.remove("dark");
//   //   }
//   // };

//   return (
//     <div data-type="DarkModeToggle" className="flex h-20 items-center gap-x-4">
//       <div>Dark Mode</div>
//       <RadixSwitch.Root
//         className={clsx(
//           `relative h-[30px] w-[50px] cursor-pointer rounded-full`,
//           `transition-colors duration-200 ease-in-out`,
//           `data-[state=checked]:bg-simons-gray-1/100 data-[state=unchecked]:bg-simons-gray-1/50`,
//           `focus:outline-none focus-visible:ring-4 focus-visible:ring-white`
//         )}
//         checked={dark_mode}
//         onCheckedChange={(value: boolean) => {
//           // set_dark_mode(value);
//           dispatch_action({
//             type: `set_dark_mode`,
//             value,
//             cell_id: null
//           });
//         }}
//       >
//         <RadixSwitch.Thumb
//           className={clsx(
//             `pointer-events-none absolute block h-[20px] w-[20px] rounded-full bg-white shadow-lg ring-0`,
//             `-translate-y-1/2 transform`,
//             `transition-all duration-200 ease-in-out`,
//             `data-[state=checked]:left-[calc(100%-25px)] data-[state=unchecked]:left-[5px]`
//           )}
//         />
//       </RadixSwitch.Root>
//     </div>
//   );
// }
