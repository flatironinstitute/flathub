import type { FilterValueRaw } from "../types";

import * as controller from "../app-state";
import {
  format,
  assert_numeric_field_stats,
  assert_numeric_filter_value,
  join_enums,
  log
} from "../shared";
import { Select, RangeSliderWithText } from "./Primitives";
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

  const catalog_cell_id = useCatalogCellID();
  const catalog_id = useCatalogID();

  const action_key = [`filter_value`, catalog_cell_id, catalog_id, field_id];

  return (
    <RangeSliderWithText
      min={min}
      max={max}
      low={low}
      high={high}
      onLowChange={(number) => {
        dispatch([...action_key, `gte`], number);
      }}
      onHighChange={(number) => {
        dispatch([...action_key, `lte`], number);
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

  const catalog_cell_id = useCatalogCellID();
  const catalog_id = useCatalogID();

  const dispatch = controller.useDispatch();
  const action_key = [`filter_value`, catalog_cell_id, catalog_id, field_id];

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
        dispatch(action_key, value);
      }}
    />
  );
}
