import type { FilterValueRaw } from "../types";

import * as controller from "../app-state";
import {
  format,
  assert_numeric_field_stats,
  assert_numeric_filter_value,
  join_enums
} from "../shared";
import { RangeSlider, TextInput, Select } from "./Primitives";
import { useFieldNode } from "./FieldCard";
import { useCatalogCellID, useCatalogID } from "./CatalogContext";
import { useFilters } from "../filters";

export function RangeFilterControl() {
  const field_node = useFieldNode();
  const metadata = field_node.data;
  assert_numeric_field_stats(metadata);
  const field_id = metadata.name;
  const filters = useFilters();

  const dispatch = controller.useDispatch();

  const filter_value_raw: FilterValueRaw = filters[field_id];
  assert_numeric_filter_value(filter_value_raw);

  const { min, max } = metadata.stats;
  const { gte: low, lte: high } = filter_value_raw;

  const value = [low, high];

  const catalog_cell_id = useCatalogCellID();
  const catalog_id = useCatalogID();

  const action_key = [`filter_value`, catalog_cell_id, catalog_id, field_id];

  const slider = (
    <RangeSlider
      min={min}
      max={max}
      value={value}
      onValueChange={([new_low, new_high]) => {
        if (new_low !== low) {
          dispatch([...action_key, `gte`], new_low);
        }
        if (new_high !== high) {
          dispatch([...action_key, `lte`], new_high);
        }
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
          if (number > high) return `Must be less than ${high.toString()}`;
          return null;
        }}
        onStringInput={(string: string) => {
          const number = Number(string);
          if (!Number.isFinite(number)) return;
          if (string === low.toString()) {
            // log(`Not updating filter because it didn't change:`, string, low);
            return;
          }
          dispatch([...action_key, `gte`], number);
        }}
      />
      <TextInput
        value={high.toString()}
        getValidityMessage={(string) => {
          const number = Number(string);
          if (!Number.isFinite(number)) return `Invalid number`;
          if (number > max) return `Must be less than ${max.toString()}`;
          if (number < low) return `Must be greater than ${low.toString()}`;
          return null;
        }}
        onStringInput={(string: string) => {
          const number = Number(string);
          if (!Number.isFinite(number)) return;
          if (string === high.toString()) {
            // log(`Not updating filter because it didn't change:`, string, high);
            return;
          }
          dispatch([...action_key, `lte`], number);
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
  const field_node = useFieldNode();
  const metadata = field_node.data;
  const field_id = metadata.name;
  const filters = useFilters();
  const filter_value_raw: FilterValueRaw = filters[field_id];
  const values = join_enums(metadata);
  const value = values.find((d) => d.value === filter_value_raw);

  const catalog_cell_id = useCatalogCellID();
  const catalog_id = useCatalogID();

  const dispatch = controller.useDispatch();
  const action_key = [`filter_value`, catalog_cell_id, catalog_id, field_id];

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
        dispatch(action_key, value);
      }}
    />
  );
}
