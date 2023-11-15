import type { CatalogHierarchyNode } from "../types";
import { is_leaf_node } from "../shared";
import {
  useAddFilter,
  useFilterValues,
  useRemoveFilter
} from "../contexts/FiltersContext";

export default function AddRemoveFilterButton({
  node
}: {
  node: CatalogHierarchyNode;
}) {
  const metadata = node.data;
  const is_leaf = is_leaf_node(node);
  const can_remove = metadata.required !== true;
  const is_active_filter = useFilterValues()[metadata.name] !== undefined;
  const remove_filter = useRemoveFilter(node);
  const add_filter = useAddFilter(node);

  if (!is_leaf) return null;
  if (!can_remove) return null;
  const on_click = is_active_filter
    ? () => remove_filter()
    : () => add_filter();
  const text = is_active_filter ? `Remove` : `Add`;
  return (
    <button className="underline" onClick={on_click}>
      {text}
    </button>
  );
}
