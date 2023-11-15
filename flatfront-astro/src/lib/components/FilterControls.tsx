import type { CatalogHierarchyNode, FilterValueRaw } from "../types";

import {
  format,
  assert_numeric_field_stats,
  assert_numeric_filter_value,
  join_enums,
  log,
  is_leaf_node,
  get_field_type,
  get_field_titles
} from "../shared";
import {
  Select,
  RangeSliderWithText,
  Separator,
  FieldTitles
} from "./Primitives";
import { FieldNodeProvider, useFieldNode } from "../contexts/FieldNodeContext";
import {
  useFilterNames,
  useFilterValues,
  useSetFilterValue
} from "../contexts/FiltersContext";
import { useCatalogMetadata } from "../contexts/CatalogMetadataContext";
import React from "react";
import clsx from "clsx";
import AddRemoveFilterButton from "./AddRemoveFilterButton";
import Katex from "./Katex";

export default function FilterControls() {
  const catalog_metadata = useCatalogMetadata();
  const names = useFilterNames();
  // const all_field_nodes = catalog_metadata?.depth_first ?? [];
  const leaves = catalog_metadata?.hierarchy?.leaves() ?? [];
  const filter_and_ancestor_nodes = leaves.filter((node) => {
    // Exclude if is root node
    if (node.depth === 0) return false;
    // Include if this node is in the filters list
    if (names.has(node.data.name)) return true;
    // Include if this node is an ancestor of a node in the filter list
    if (node.leaves().some((leaf) => names.has(leaf.data.name))) return true;
    // Exclude otherwise
    return false;
  });
  return (
    <div className="space-y-3">
      {filter_and_ancestor_nodes.map((node, index) => (
        <React.Fragment key={catalog_metadata.hash_map.get(node)}>
          {index === 0 ? null : <Separator></Separator>}
          <FilterCard fieldNode={node} />
        </React.Fragment>
      ))}
    </div>
  );
}

function FilterCard({
  fieldNode: field_node
}: {
  fieldNode: CatalogHierarchyNode;
}) {
  return (
    <FieldNodeProvider value={field_node}>
      <div
        data-type="FilterCard"
        className={clsx(`overflow-visible rounded-md px-px py-2`)}
      >
        <FieldCardContents />
      </div>
    </FieldNodeProvider>
  );
}

function FieldCardContents() {
  const field_node = useFieldNode();
  const metadata = field_node.data;
  const field_type = get_field_type(metadata);
  const titles = get_field_titles(field_node);
  const field_titles = <FieldTitles titles={titles} />;

  const field_units = metadata.units ? (
    <div>
      [<Katex>{metadata.units}</Katex>]
    </div>
  ) : null;

  const title_and_units = (
    <div className="flex justify-between">
      <div className="flex space-x-2">
        {field_titles}
        {field_units}
      </div>
      <AddRemoveFilterButton node={field_node} />
    </div>
  );

  const filter_control = (() => {
    if (!is_leaf_node(field_node)) return null;
    switch (field_type) {
      case `INTEGER`:
      case `FLOAT`:
        return <RangeFilterControl />;
      case `ENUMERABLE_INTEGER`:
      case `LABELLED_ENUMERABLE_INTEGER`:
      case `LABELLED_ENUMERABLE_BOOLEAN`:
        return <SelectFilterControl />;
      default:
        return <div>not yet implemented: {field_type}</div>;
    }
  })();

  return (
    <div className="space-y-4">
      {title_and_units}
      {filter_control}
    </div>
  );
}

function RangeFilterControl() {
  const field_node = useFieldNode();
  const metadata = field_node.data;
  assert_numeric_field_stats(metadata);
  const field_id = metadata.name;
  const filters = useFilterValues();

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

/**
 * Type is either:
 * - ENUMERABLE_INTEGER
 * - LABELLED_ENUMERABLE_INTEGER
 * - LABELLED_ENUMERABLE_BOOLEAN
 */
function SelectFilterControl() {
  const field_node = useFieldNode();
  const metadata = field_node.data;
  const field_id = metadata.name;
  const filters = useFilterValues();
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
      onValueChange={({ value }) => set_filter_value(value)}
    />
  );
}
