import React from "react";
import clsx from "clsx";
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
            `fixed z-50 w-[95vw] max-w-3xl rounded-lg p-4`,
            `left-[50%] top-[50%] -translate-x-[50%] -translate-y-[50%]`,
            `bg-white dark:bg-black`,
            `ring-1 ring-black dark:ring-white`
          )}
        >
          {children}
          <RadixDialog.Close
            className={clsx(
              "absolute right-3.5 top-3.5 inline-flex items-center justify-center rounded-full p-1",
              "focus:outline-none focus-visible:ring focus-visible:ring-purple-500 focus-visible:ring-opacity-75"
            )}
          >
            <Icons.Cross1Icon className="h-4 w-4" />
          </RadixDialog.Close>
        </RadixDialog.Content>
      </RadixDialog.Portal>
    </RadixDialog.Root>
  );
}
