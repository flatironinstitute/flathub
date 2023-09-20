import type {
  Cell,
  TopResponseEntry,
  Action,
  CatalogHierarchyNode
} from "./types";

import * as d3 from "d3";
import * as stores from "./stores";
import clsx from "clsx";
import {
  BigButton,
  CellSection,
  CellWrapper,
  dispatch_action,
  hooks,
  assert_catalog_cell_id,
  log,
  Placeholder,
  Providers
} from "./shared";

export default function FilterControls() {
  const catalog_cell_id = hooks.useCell().cell_id;
  assert_catalog_cell_id(catalog_cell_id);
  const catalog_id = hooks
    .useStore(stores.catalog_id_by_cell_id)
    .get(catalog_cell_id);
  const catalog_metadata = hooks.useStore(
    stores.catalog_metadata_by_catalog_id
  )?.[catalog_id];
  log(`sherman`, `catalog_metadata`, catalog_metadata);
  const filters = hooks
    .useStore(stores.filters_by_cell_id)
    .get(catalog_cell_id);
  const all_field_nodes = catalog_metadata?.depth_first ?? [];
  const filter_and_ancestor_nodes = all_field_nodes.filter((node) => {
    // Exclude if is root node
    if (node.depth === 0) return false;
    // Include if this node is in the filters list
    if (node.data.name in filters) return true;
    // Include if this node is an ancestor of a node in the filter list
    if (node.leaves().some((leaf) => leaf.data.name in filters)) return true;
    // Exclude otherwise
    return false;
  });
  return (
    <div className="space-y-3 text-xs">
      <pre>{JSON.stringify(filters, null, 2)}</pre>
      {filter_and_ancestor_nodes.map((node) => (
        <Providers.FieldNodeProvider value={node} key={node.data.__hash}>
          <FilterCard />
        </Providers.FieldNodeProvider>
      ))}
    </div>
  );
}

function FilterCard() {
  const field_node = hooks.useFieldNode();
  return (
    <FieldCardWrapper>filter card: {field_node.data.name}</FieldCardWrapper>
  );
}

function FieldCardWrapper({
  children
}: {
  children: React.ReactNode;
}): React.JSX.Element {
  // const cell = hooks.useCell();
  // const cell_id = cell.cell_id;
  // const catalog_id = hooks.useStore(stores.catalog_id_by_cell_id).get(cell_id);
  // const field_id = hooks.useFieldID();
  // const nodes_by_id = hooks
  //   .useStore(stores.catalog_metadata_wrapper_by_catalog_id)
  //   .get(catalog_id)?.nodes_by_id;
  const field_node = hooks.useFieldNode();
  // if (!field_node) {
  //   throw new Error(`Could not find node for ${field_id}`);
  // }
  const node_depth = (field_node?.depth ?? 0) - 1;
  return (
    <div
      data-type="FieldCardWrapper"
      className={clsx(FieldCardWrapper.className, `space-y-4`)}
      style={{
        marginLeft: `${node_depth * 2}ch`
      }}
    >
      {children}
    </div>
  );
}

FieldCardWrapper.className = clsx(
  `rounded-md text-md px-4 py-4`,
  `ring-1 ring-black/30 dark:ring-white/30`
);
