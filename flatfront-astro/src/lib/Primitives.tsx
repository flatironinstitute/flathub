import React from "react";
import clsx from "clsx";
import * as d3 from "d3";
import * as RadixSlider from "@radix-ui/react-slider";
import * as RadixDialog from "@radix-ui/react-dialog";
import * as Icons from "@radix-ui/react-icons";

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
  className,
  ...rest
}: { children: React.ReactNode } & React.HTMLAttributes<HTMLDivElement>) {
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
            <Icons.Cross1Icon className="h-4 w-4" />
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
  onInput,
  getValidityMessage = (value) => null,
  ...rest
}: {
  value?: string;
  getValidityMessage?: (value: string) => string | null;
  onInput?: (value: string) => void;
} & React.InputHTMLAttributes<HTMLInputElement>) {
  const ref = React.useRef<HTMLInputElement>(null);
  const [internal, set_internal] = React.useState(value ?? ``);

  const on_input = (string) => {
    set_internal(string);
    const message = getValidityMessage(string);
    if (message) {
      ref.current.setCustomValidity(message);
    } else {
      ref.current.setCustomValidity("");
      onInput && onInput(string);
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
      onInput={(event) => on_input(event.currentTarget.value)}
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
