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
import * as RadixCollapsible from "@radix-ui/react-collapsible";
import { useDebounce } from "@uidotdev/usehooks";
import { log } from "../shared";
import Katex from "./Katex";

export function Heading({
  children,
  className
}: {
  children: React.ReactNode;
  className?: string;
}) {
  return <h2 className={clsx(`text-xl`, className)}>{children}</h2>;
}

export function Separator({ orientation }: RadixSeparator.SeparatorProps) {
  return (
    <RadixSeparator.Root
      className="my-4 h-px bg-black/40 dark:bg-white/10"
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
  `block w-full rounded-lg py-3 font-bold text-center max-h-10 whitespace-nowrap`,
  `ring-1 ring-black dark:ring-white`,
  `focus:outline-none focus-visible:ring-4`,
  `disabled:opacity-50 disabled:cursor-not-allowed`
);

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
  disabled = false,
  className,
  buttonClassName
}: {
  label: string;
  children: React.ReactNode;
  disabled?: boolean;
  className?: string;
  buttonClassName?: string;
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
            `ring-1 ring-black dark:ring-white`,
            className
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
    if (debounced_low === props.low) return;
    onLowChange?.(debounced_low);
  }, [debounced_low]);
  React.useEffect(() => {
    if (debounced_high === props.high) return;
    onHighChange?.(debounced_high);
  }, [debounced_high]);

  return (
    <div
      data-type="RangeSliderWithText"
      className="mt-[9px] grid grid-cols-2 items-center gap-x-4 gap-y-2"
    >
      <NumberInput
        value={internal_low?.toString()}
        min={min}
        max={Math.min(internal_high, max)}
        onNumberInput={(value) => set_internal_low(value)}
      />
      <NumberInput
        value={internal_high?.toString()}
        min={Math.max(internal_low, min)}
        max={max}
        onNumberInput={(value) => set_internal_high(value)}
      />
      <div className="col-span-2">
        <Slider
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

export function SliderWithText(props: {
  min: number;
  max: number;
  value: number;
  debounce?: number;
  onValueChange?: (value: number) => void;
}) {
  const { min, max, onValueChange, debounce } = props;
  const [internal_value, set_internal_value] = React.useState<number>(
    props.value
  );
  const debounced_value = useDebounce(internal_value, debounce ?? 0);

  React.useEffect(() => {
    set_internal_value(props.value);
  }, [props.value]);

  React.useEffect(() => {
    onValueChange?.(debounced_value);
  }, [debounced_value]);

  return (
    <div data-type="SliderWithText" className="@container">
      <div className="grid items-center gap-x-4 gap-y-2 @xs:grid-cols-2">
        <NumberInput
          value={internal_value?.toString()}
          min={min}
          max={max}
          onNumberInput={(value) => set_internal_value(value)}
        />
        <Slider
          min={min}
          max={max}
          value={[internal_value]}
          onValueChange={([new_value]) => {
            if (new_value !== internal_value) {
              set_internal_value(new_value);
            }
          }}
        />
      </div>
    </div>
  );
}

function Slider({
  min,
  max,
  value,
  onValueChange,
  step = d3.tickStep(min, max, 100)
}: RadixSlider.SliderProps) {
  const thumb_class = clsx(
    `block h-2 w-2 rounded-full`,
    `bg-black dark:bg-white`,
    `focus:outline-none focus-visible:ring-4 focus-visible:ring-offset-4 focus-visible:ring-black dark:focus-visible:ring-white dark:focus-visible:ring-offset-black`
  );

  return (
    <RadixSlider.Root
      data-type="Slider"
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
      <RadixSlider.Thumb className={thumb_class} />
    </RadixSlider.Root>
  );
}

export function NumberInput({
  value,
  min,
  max,
  onNumberInput,
  ...rest
}: TextInputProps & {
  onNumberInput?: (value: number) => void;
}) {
  return (
    <TextInput
      type="number"
      step="any"
      value={value?.toString()}
      getValidityMessage={(string) => {
        const number = Number(string);
        if (!Number.isFinite(number)) return `Invalid number`;
        if (number < Number(min))
          return `Must be greater than ${min.toString()}`;
        if (number > Number(max)) return `Must be less than ${max.toString()}`;
        return null;
      }}
      onStringInput={(string: string) => {
        const number = Number(string);
        if (!Number.isFinite(number)) return;
        if (string?.toString() === value?.toString()) {
          // Not updating filter because it didn't change
          return;
        }
        onNumberInput?.(number);
      }}
      {...rest}
    />
  );
}

type TextInputProps = React.InputHTMLAttributes<HTMLInputElement> & {
  onStringInput?: (value: string) => void;
  getValidityMessage?: (value: string) => string | null;
};

export function TextInput({
  value,
  onStringInput = () => null,
  getValidityMessage = () => null,
  ...rest
}: TextInputProps) {
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
    on_input(value?.toString() ?? ``);
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
  const { className, ...rest } = props;
  return (
    <RadixCheckbox.Root
      className={clsx(
        `flex h-6 w-6 appearance-none items-center justify-center rounded-md`,
        `bg-white ring-1 ring-black dark:bg-black dark:outline-white dark:ring-white`,
        `data-[disabled]:cursor-not-allowed data-[disabled]:opacity-20`,
        className
      )}
      {...rest}
    >
      <RadixCheckbox.Indicator>
        <RadixIcons.CheckIcon className="h-4 w-4" />
      </RadixCheckbox.Indicator>
    </RadixCheckbox.Root>
  );
}

export function StatusBox({ children }) {
  return (
    <div
      className={clsx(
        "absolute left-1/2 top-1/2 z-10 rounded-lg bg-white p-4",
        "-translate-x-1/2 -translate-y-full transform",
        "ring-2 ring-black dark:bg-black dark:ring-white"
      )}
    >
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
  size = `large`,
  debug = false,
  triggerClassName = undefined,
  valueClassName = undefined
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
  debug?: boolean;
  triggerClassName?: string;
  valueClassName?: string;
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
          `relative flex max-h-10 w-full items-center justify-between`,
          `cursor-pointer text-left`,
          `disabled:cursor-wait disabled:opacity-50`,
          `rounded-md ring-1 ring-black dark:ring-white`,
          `focus:outline-none focus-visible:ring-4`,
          size_classes.trigger,
          triggerClassName
        )}
      >
        <RadixSelect.Value placeholder={placeholder} asChild>
          <span className={valueClassName}>
            {value ? getDisplayName(value) : placeholder}
          </span>
        </RadixSelect.Value>
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

export { RadixCollapsible as Collapsible };

export function CollapsibleSection({
  label,
  children,
  className
}: {
  label: string;
  children: React.ReactNode;
  className?: string;
}) {
  const [open, set_open] = React.useState(true);
  return (
    <RadixCollapsible.Root open={open} onOpenChange={set_open}>
      <div className="space-y-4 @container">
        <div className="flex justify-between">
          <SimpleLabel>{label}</SimpleLabel>
          <RadixCollapsible.Trigger className="cursor-pointer underline">
            {open ? `Collapse` : `Expand`}
          </RadixCollapsible.Trigger>
        </div>
        <RadixCollapsible.Content className={className}>
          {children}
        </RadixCollapsible.Content>
      </div>
    </RadixCollapsible.Root>
  );
}

export function FieldTitles({ titles }: { titles: string[] }) {
  return (
    <div className="flex gap-x-2">
      {titles.map((title, index) => (
        <Katex key={`${title}-${index}`}>{title}</Katex>
      ))}
    </div>
  );
}
