import type { FilterValueRaw } from "../types";

import {
  format,
  assert_numeric_field_stats,
  assert_numeric_filter_value,
  join_enums,
  log
} from "../shared";
import { Select, RangeSliderWithText } from "./Primitives";
import { useFieldNode } from "../contexts/FieldNodeContext";
import { useFilters, useSetFilterValue } from "../contexts/FiltersContext";

export function RangeFilterControl() {
  const field_node = useFieldNode();
  const metadata = field_node.data;
  assert_numeric_field_stats(metadata);
  const field_id = metadata.name;
  const filters = useFilters();

  const filter_value_raw: FilterValueRaw = filters[field_id];
  assert_numeric_filter_value(filter_value_raw);

  const { min, max } = metadata.stats;
  const { gte: low, lte: high } = filter_value_raw;

  const set_filter_value = useSetFilterValue();

  return (
    <RangeSliderWithText
      min={min}
      max={max}
      low={low}
      high={high}
      onLowChange={(number) => {
        set_filter_value({
          gte: number
        });
      }}
      onHighChange={(number) => {
        set_filter_value({
          lte: number
        });
      }}
      debounce={500}
    />
  );
}

export function SelectFilterControl() {
  const field_node = useFieldNode();
  const metadata = field_node.data;
  const field_id = metadata.name;
  const filters = useFilters();
  const filter_value_raw: FilterValueRaw = filters[field_id];
  const values = join_enums(metadata);
  const value = values.find((d) => d.value === filter_value_raw);

  const set_filter_value = useSetFilterValue();

  const get_key = (d) => d?.text;

  return (
    <Select
      value={value}
      options={values}
      getKey={get_key}
      getDisplayName={(d) => {
        if (!d.count) return d.text;
        return `${d.text} (${format.commas(d.count)} rows)`;
      }}
      onValueChange={({ value }) => {
        // set_filter_value([], value);
        set_filter_value(value);
      }}
    />
  );
}
