import {
  dispatch_action,
  hooks,
  log,
  format,
  assert_numeric_field_stats,
  assert_numeric_filter_value,
  join_enums
} from "./shared";
import * as stores from "./stores";
import Katex from "./Katex";
import { RangeSlider, TextInput, Select } from "./Primitives";
import type { FilterValueRaw } from "./types";

export function RangeFilterControl() {
  const filters = hooks.useFilters();
  const field_node = hooks.useFieldNode();
  const metadata = field_node.data;
  assert_numeric_field_stats(metadata);
  const field_id = metadata.name;
  const filter_value_raw: FilterValueRaw = filters[field_id];
  assert_numeric_filter_value(filter_value_raw);

  const { min, max } = metadata.stats;
  const { gte: low, lte: high } = filter_value_raw;

  const value = [low, high];

  const set_filter_value = hooks.useFilterValueSetter();

  const slider = (
    <RangeSlider
      min={min}
      max={max}
      value={value}
      onValueChange={([low, high]) => {
        set_filter_value({
          gte: low,
          lte: high
        });
      }}
    />
  );

  return (
    <div
      data-type="RangeFilterControl"
      className="mt-[9px] grid grid-cols-2 items-center gap-x-4 gap-y-2"
    >
      <TextInput
        value={low.toString()}
        getValidityMessage={(string) => {
          const number = Number(string);
          if (!Number.isFinite(number)) return `Invalid number`;
          if (number < min) return `Must be greater than ${min.toString()}`;
          return null;
        }}
        onInput={(string) => {
          const number = Number(string);
          if (!Number.isFinite(number)) return;
          set_filter_value({
            gte: number,
            lte: high
          });
        }}
      />
      <TextInput
        value={high.toString()}
        getValidityMessage={(string) => {
          const number = Number(string);
          if (!Number.isFinite(number)) return `Invalid number`;
          if (number > max) return `Must be less than ${max.toString()}`;
          return null;
        }}
        onInput={(string) => {
          const number = Number(string);
          if (!Number.isFinite(number)) return;
          set_filter_value({
            gte: low,
            lte: number
          });
        }}
      />
      <div className="col-span-2">
        {/* <Label className="hidden sm:block">&nbsp;</Label> */}
        {slider}
      </div>
    </div>
  );
}

export function SelectFilterControl() {
  const field_node = hooks.useFieldNode();
  const metadata = field_node.data;
  const field_id = metadata.name;
  const filters = hooks.useFilters();
  const filter_value_raw: FilterValueRaw = filters[field_id];
  const values = join_enums(metadata);
  const value = values.find((d) => d.value === filter_value_raw);

  const set_filter_value = hooks.useFilterValueSetter();

  return (
    <Select
      value={value}
      options={values}
      getKey={(d) => d?.value?.toString()}
      getDisplayName={(d) => {
        if (!d.count) return d.text;
        return `${d.text} (${format.commas(d.count)} rows)`;
      }}
      onValueChange={({ value }) => {
        set_filter_value(value);
      }}
    />
  );
}
