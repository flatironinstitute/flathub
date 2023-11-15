import type { CatalogHierarchyNode } from "../types";

import React from "react";
import clsx from "clsx";
import { get_field_titles, get_field_type, is_leaf_node } from "../shared";
import { useFilterValues, useRemoveFilter } from "../contexts/FiltersContext";
import {
  useFieldNode,
  Provider as FieldNodeProvider
} from "../contexts/FieldNodeContext";
import Katex from "./Katex";
import { FieldTitles } from "./Primitives";
import { RangeFilterControl, SelectFilterControl } from "./FilterControls";

export default function FieldCard({
  fieldNode: field_node
}: {
  fieldNode: CatalogHierarchyNode;
}) {
  return (
    <FieldNodeProvider value={field_node}>
      <FieldCardWrapper>
        <FieldCardContents />
      </FieldCardWrapper>
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

  const remove_filter = useRemoveFilter();
  const is_active_filter = useFilterValues()[metadata.name] !== undefined;
  const can_remove = metadata.required !== true;
  const remove_button =
    is_leaf_node(field_node) && is_active_filter && can_remove ? (
      <button
        className="cursor-pointer underline"
        onClick={() => remove_filter(field_node)}
      >
        Remove
      </button>
    ) : null;

  const title_and_units = (
    <div className="flex justify-between">
      <div className="flex space-x-2">
        {field_titles}
        {field_units}
      </div>
      {remove_button}
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
        return <div>not yet implemented</div>;
    }
  })();

  return (
    <div className="space-y-4">
      {title_and_units}
      {filter_control}
    </div>
  );
}

function FieldCardWrapper({
  children,
  className
}: {
  children: React.ReactNode;
  className?: string;
}): React.JSX.Element {
  return (
    <div
      data-type="FieldCardWrapper"
      className={clsx(FieldCardWrapper.className, className)}
    >
      {children}
    </div>
  );
}
FieldCardWrapper.className = clsx(`rounded-md px-px py-2 overflow-visible`);
