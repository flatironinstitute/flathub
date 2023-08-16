import type { FieldMetadata, FilterValueRaw } from "../types";

import React from "react";

import clsx from "clsx";

import * as Slider from "@radix-ui/react-slider";
import * as d3 from "d3";

import {
  dispatch_action,
  hooks,
  log,
  set_filter_value,
  format,
  Select,
} from "../shared";
import * as stores from "../stores";
import Katex from "./Katex";

export function FieldCard(): React.JSX.Element {
  const cell_id = hooks.useCellID();
  const field_id = hooks.useFieldID();
  const filters = hooks.useCellFilters();
  const nodes_by_id = hooks.useCatalogMetadata()?.nodes_by_id;
  const field_node = nodes_by_id?.get(field_id);

  if (!field_node) {
    throw new Error(`Could not find node for ${field_id}`);
  }

  const is_leaf = field_node.children === undefined;
  const is_required = field_node.data.required;

  const field_title = <Katex>{field_node.data.title}</Katex>;

  const units = (() => {
    if (!field_node.data.units) return null;
    return (
      <div className="text-sm text-slate-500 dark:text-slate-300">
        <Katex>{field_node.data.units}</Katex>
      </div>
    );
  })();

  const field_description = (() => {
    if (!field_node.data.descr) return null;
    return (
      <div className="text-sm text-slate-500 dark:text-slate-300 overflow-hidden">
        <Katex>{field_node.data.descr}</Katex>
      </div>
    );
  })();

  const filter_toggle = (() => {
    if (!is_leaf) return null;
    const is_active_filter = field_id in filters;
    const on_click = () => {
      log(`filter toggle`, is_active_filter);
      if (is_active_filter) {
        dispatch_action({
          type: `remove_filter`,
          cell_id,
          filter_id: field_id,
        });
      } else {
        dispatch_action({
          type: `add_filter`,
          cell_id,
          filter_id: field_id,
        });
      }
    };
    return (
      <LittleTextButton disabled={is_required} onClick={() => on_click()}>
        {is_active_filter ? `Remove Filter` : `Add Filter`}
      </LittleTextButton>
    );
  })();

  const top_part = (
    <div className="flex justify-between items-center">
      {field_title}
      {filter_toggle}
    </div>
  );

  return (
    <FieldCardWrapper>
      {top_part}
      {units}
      {field_description}
    </FieldCardWrapper>
  );
}

export function FilterCard() {
  const cell_id = hooks.useCellID();
  const field_id = hooks.useFieldID();
  const nodes_by_id = hooks.useCatalogMetadata()?.nodes_by_id;
  const field_node = nodes_by_id?.get(field_id);

  if (!field_node) {
    throw new Error(`Could not find node for ${field_id}`);
  }

  const is_leaf = field_node.children === undefined;
  const is_required = field_node.data.required;

  const field_title = <Katex>{field_node.data.title}</Katex>;

  const filter_control = (() => {
    const metadata = field_node?.data;
    if (!metadata) {
      throw new Error(`Could not find metadata for ${field_id}`);
    }
    if (metadata.sub && metadata.sub.length > 0) {
      return null;
    } else if (metadata.type === `float` || metadata.type === `short`) {
      return <NumericFilterControl />;
    } else if (
      (metadata.type === `boolean` || metadata.type === `byte`) &&
      metadata.enum &&
      metadata.stats?.terms
    ) {
      return <SelectFilterControl />;
    } else {
      log(metadata);
      throw new Error(`Unknown field type: ${metadata.type}`);
    }
  })();

  const remove_filter_button = (() => {
    if (is_required) return null;
    if (!is_leaf)
      return (
        <LittleTextButton
          onClick={() => {
            dispatch_action({
              type: `remove_child_filters`,
              cell_id,
              filter_id: field_id,
            });
          }}
        >
          remove all
        </LittleTextButton>
      );
    return (
      <LittleTextButton
        onClick={() => {
          dispatch_action({
            type: `remove_filter`,
            cell_id,
            filter_id: field_id,
          });
        }}
      >
        remove
      </LittleTextButton>
    );
  })();

  const top_part = (
    <div className="flex justify-between items-center">
      {field_title}
      {remove_filter_button}
    </div>
  );

  return (
    <FieldCardWrapper>
      {top_part}
      {filter_control}
    </FieldCardWrapper>
  );
}

function FieldCardWrapper({
  children,
}: {
  children: React.ReactNode;
}): React.JSX.Element {
  const field_id = hooks.useFieldID();
  const nodes_by_id = hooks.useCatalogMetadata()?.nodes_by_id;
  const field_node = nodes_by_id?.get(field_id);
  if (!field_node) {
    throw new Error(`Could not find node for ${field_id}`);
  }
  const node_depth = (field_node?.depth ?? 0) - 1;
  return (
    <div
      data-type="FieldCardWrapper"
      className={clsx(
        `flex flex-col gap-y-4 rounded-md text-md px-4 py-4`,
        `bg-light-3 dark:bg-dark-3 text-light-text dark:text-dark-text`
      )}
      style={{
        marginLeft: `${node_depth * 2}ch`,
      }}
    >
      {children}
    </div>
  );
}

function NumericFilterControl() {
  const cell_id = hooks.useCellID();
  const catalog_id = hooks.useCatalogID();
  const field_id = hooks.useFieldID();
  const metadata = hooks
    .useStore(stores.field_metadata)
    ?.get(catalog_id, field_id)?.data;
  const filters = hooks.useCellFilters();
  const filter_value_raw: FilterValueRaw = filters[field_id];

  const { min, max } = get_field_stats(metadata);

  const { low, high } = get_numeric_filter_value(filter_value_raw, field_id);

  const value = [low, high] as [number, number];

  // const slider = (
  //   <div className="grid grid-cols-[10ch_1fr_10ch] items-center justify-items-center">
  //     <div className="col-start-1">
  //       <Label>from</Label>
  //     </div>
  //     <div className="col-start-3">
  //       <Label>to</Label>
  //     </div>
  //     <div>{format.concise(low)}</div>
  //     <RangeSlider
  //       min={min}
  //       max={max}
  //       value={value}
  //       onValueChange={([low, high]) => {
  //         set_filter_value(cell_id, field_id, {
  //           gte: low,
  //           lte: high,
  //         });
  //       }}
  //     />
  //     <div>{format.concise(high)}</div>
  //   </div>
  // );

  const slider = (
    <RangeSlider
      min={min}
      max={max}
      value={value}
      onValueChange={([low, high]) => {
        set_filter_value(cell_id, field_id, {
          gte: low,
          lte: high,
        });
      }}
    />
  );

  return (
    <>
      <div
        data-type="NumericFilterControl"
        className="mt-[9px] grid grid-cols-1 gap-4 sm:grid-cols-3 items-center"
      >
        <TextInput
          label="from"
          value={low.toString()}
          getValidityMessage={(string) => {
            const number = valid_number(string);
            if (number === null) return `Invalid number`;
            if (number < min) return `Must be greater than ${min.toString()}`;
            return null;
          }}
          onInput={(string) => {
            const number = valid_number(string);
            if (number === null) return;
            set_filter_value(cell_id, field_id, {
              gte: number,
              lte: high,
            });
          }}
        />
        <div>
          <Label className="hidden sm:block">&nbsp;</Label>
          {slider}
        </div>
        <TextInput
          label="to"
          value={high.toString()}
          getValidityMessage={(string) => {
            const number = valid_number(string);
            if (number === null) return `Invalid number`;
            if (number > max) return `Must be less than ${max.toString()}`;
            return null;
          }}
          onInput={(string) => {
            const number = valid_number(string);
            if (number === null) return;
            set_filter_value(cell_id, field_id, {
              gte: low,
              lte: number,
            });
          }}
        />
      </div>
    </>
  );
}

function valid_number(maybe: string): number | null {
  const number = +maybe;
  if (!Number.isFinite(number)) {
    // throw new Error(`Expected a number but got ${maybe}`);
    return null;
  }
  return number;
}

function get_field_stats(metadata: FieldMetadata) {
  const min = metadata.stats?.min;
  const max = metadata.stats?.max;
  if (
    !Number.isFinite(min) ||
    !Number.isFinite(max) ||
    min === null ||
    max === null ||
    min === undefined ||
    max === undefined
  ) {
    log(`Error meta:`, metadata);
    throw new Error(
      `Could not find min/max stats for ${metadata.name} of type ${metadata.type}`
    );
  }
  return { min, max };
}

function get_numeric_filter_value(
  filter_value: FilterValueRaw,
  field_id: string
): { low: number; high: number } {
  if (typeof filter_value !== `object`) {
    throw new Error(`Expected filter state to be an object for ${field_id}`);
  }
  if (!(`gte` in filter_value) || !(`lte` in filter_value)) {
    throw new Error(`Expected filter state to have gte/lte for ${field_id}`);
  }
  const low = filter_value.gte;
  const high = filter_value.lte;
  if (typeof low !== `number` || typeof high !== `number`) {
    throw new Error(
      `Expected filter state to have gte/lte numbers for ${field_id}`
    );
  }
  return { low, high };
}

function SelectFilterControl() {
  const cell_id = hooks.useCellID();
  const catalog_id = hooks.useCatalogID();
  const field_id = hooks.useFieldID();
  const metadata = hooks
    .useStore(stores.field_metadata)
    ?.get(catalog_id, field_id)?.data;
  const filters = hooks.useCellFilters();
  const filter_value_raw: FilterValueRaw = filters[field_id];

  const enums = metadata.enum;

  const terms = metadata.stats.terms;

  const values = enums.map((text, index) => {
    const { value, count } = terms[index];
    return { text, value, count };
  });

  const value = values.find((d) => d.value === filter_value_raw);

  const on_change = (d) => {
    set_filter_value(cell_id, field_id, d.value);
  };

  return (
    <Select
      value={value}
      options={values}
      getKey={(d) => d.value.toString()}
      getDisplayName={(d) => `${d.text} (${format.commas(d.count)} rows)`}
      onValueChange={on_change}
      buttonClassName="ring-2 ring-inset ring-light-4 dark:ring-dark-4"
      optionsClassName="bg-light-2 dark:bg-dark-2 shadow-2xl"
      optionClassName="ui-active:bg-light-3 dark:ui-active:bg-dark-3"
    />
  );
}

// function LabelledInput({
//   label,
//   value,
//   getValidityMessage = (value) => null,
//   onInput,
//   ...rest
// }: {
//   label: string;
//   value?: string;
//   getValidityMessage?: (value: string) => string | null;
//   onInput?: (value: string) => void;
// } & React.InputHTMLAttributes<HTMLInputElement>) {
//   const ref = React.useRef<HTMLInputElement>(null);
//   const [internal, set_internal] = React.useState(value ?? ``);

//   const on_input = (string) => {
//     set_internal(string);
//     const message = getValidityMessage(string);
//     if (message) {
//       ref.current.setCustomValidity(message);
//     } else {
//       ref.current.setCustomValidity("");
//       onInput && onInput(string);
//     }
//     ref.current.reportValidity();
//   };

//   React.useEffect(() => {
//     on_input(value ?? ``);
//   }, [value]);

//   return (
//     <div>
//       <Label>{label}</Label>
//       <div className="h-1"></div>
//       <div>
//         <TextInput
//           ref={ref}
//           type="text"
//           className={clsx(
//             `block w-full bg-transparent rounded-md border-0 py-2 px-2`,
//             `text-light-text dark:text-dark-text`,
//             `ring-2 ring-inset ring-light-4 dark:ring-dark-4 focus:outline-none focus:ring-dark-0 dark:focus:ring-light-0 invalid:!ring-red-400`
//           )}
//           value={internal}
//           onInput={(event) => on_input(event.currentTarget.value)}
//           {...rest}
//         />
//       </div>
//     </div>
//   );
// }

function TextInput({
  value,
  label,
  getValidityMessage = (value) => null,
  onInput,
  ...rest
}: {
  value?: string;
  label?: string;
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
    <div data-type="TextInput" className="relative w-full">
      {label && <Label>{label}</Label>}
      <input
        ref={ref}
        type="text"
        className={clsx(
          `block w-full bg-transparent rounded-md border-0 py-2 px-2`,
          `text-light-text dark:text-dark-text`,
          `ring-2 ring-inset ring-light-4 dark:ring-dark-4 focus:outline-none focus:ring-dark-0 dark:focus:ring-light-0 invalid:!ring-red-400`
        )}
        value={internal}
        onInput={(event) => on_input(event.currentTarget.value)}
        {...rest}
      />
    </div>
  );
}

function Label({
  children,
  className,
}: {
  children: React.ReactNode;
  className?: string;
}) {
  return (
    <label
      className={clsx(
        `uppercase block text-xs text-slate-700 dark:text-slate-200`,
        className
      )}
    >
      {children}
    </label>
  );
}

function RangeSlider({
  min,
  max,
  value,
  onValueChange,
}: {
  min: number;
  max: number;
  value: [number, number];
  onValueChange: (value: [number, number]) => void;
}) {
  const step = d3.tickStep(min, max, 100);

  const thumb_class = `block h-4 w-4 rounded-full bg-slate-700 dark:bg-slate-50 focus:outline-none focus-visible:ring focus-visible:ring-purple-500 focus-visible:ring-opacity-75`;

  return (
    <Slider.Root
      min={min}
      max={max}
      value={value}
      className="cursor-pointer relative flex h-5 w-full touch-none items-center"
      onValueChange={onValueChange}
      step={step}
    >
      <Slider.Track className="relative h-1 w-full grow rounded-full bg-slate-400 dark:bg-slate-800">
        <Slider.Range className="absolute h-full rounded-full bg-slate-600 dark:bg-white" />
      </Slider.Track>
      <Slider.Thumb className={thumb_class} />
      <Slider.Thumb className={thumb_class} />
    </Slider.Root>
  );
}

function LittleTextButton({
  children,
  disabled,
  onClick,
}: {
  children: React.ReactNode;
  disabled?: boolean;
  onClick?: () => void;
}): React.JSX.Element {
  return (
    <button
      disabled={disabled}
      className={clsx(
        `block cursor-pointer`,
        `underline uppercase text-xs tracking-widest`,
        `disabled:opacity-50 disabled:cursor-not-allowed`
      )}
      onClick={onClick}
    >
      {children}
    </button>
  );
}
