import clsx from "clsx";
import { hooks, get_field_type } from "./shared";
import Katex from "./Katex";

export default function FieldCard() {
  const field_node = hooks.useFieldNode();

  const field_type = get_field_type(field_node.data);

  const field_title = <Katex>{field_node.data.title}</Katex>;
  const field_units = field_node.data.units ? (
    <div>
      (<Katex>{field_node.data.units}</Katex>)
    </div>
  ) : null;

  const title_and_units = (
    <div className="flex space-x-2 text-lg">
      {field_title}
      {field_units}
    </div>
  );
  const field_description = field_node.data.descr ? (
    <div className="overflow-hidden text-xs opacity-80">
      <Katex>{field_node.data.descr}</Katex>
    </div>
  ) : null;

  const top_part = (
    <div>
      {title_and_units}
      {field_description}
    </div>
  );

  return (
    <FieldCardWrapper>
      {top_part}
      {field_type}
      <pre>
        {JSON.stringify({ ...field_node.data, sub: undefined }, null, 2)}
      </pre>
    </FieldCardWrapper>
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
