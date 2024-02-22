import type { CatalogHierarchyNode, FilterValueRaw } from "@/types";
import React from "react";
import { useDebounce } from "@uidotdev/usehooks";
import { RotateCcw, Trash2 } from "lucide-react";
import * as d3 from "d3";
import {
  assert_numeric_field_stats,
  format,
  get_field_titles,
  get_field_type,
  is_leaf_node,
  join_enums
} from "@/utils";
import { useCatalogMetadata } from "@/components/contexts/CatalogMetadataContext";
import {
  useAddFilter,
  useClearFilterValue,
  useFilterIDs,
  useFilterValues,
  useRemoveFilter,
  useResetFilter,
  useSetFilterValue
} from "@/components/contexts/FiltersContext";
import { Card } from "@/components/ui/card";
import { Katex } from "@/components/ui/katex";
import {
  Select,
  SelectContent,
  SelectGroup,
  SelectItem,
  SelectTrigger,
  SelectValue
} from "@/components/ui/select";
import { Button } from "@/components/ui/button";
import { Slider } from "@/components/ui/slider";
import { useColumnIDs } from "@/components/contexts/ColumnsContext";
import { FieldTitles } from "./FieldTitles";
import { Label } from "./ui/label";
import { NumberInput } from "./NumberInput";
import { Combobox } from "./Combobox";
import { Input } from "./ui/input";

const FieldNodeContext = React.createContext(null);

const useFieldNode = () => {
  const field_node = React.useContext(FieldNodeContext);
  if (field_node === null) {
    throw new Error(`useFieldNode: value is null`);
  }
  return field_node;
};

export function FilterSection() {
  const filter_ids = useFilterIDs();
  const catalog_metadata = useCatalogMetadata();
  const leaves = catalog_metadata?.hierarchy?.leaves() ?? [];
  const filter_nodes = leaves.filter((node) => {
    // Exclude if is root node
    if (node.depth === 0) return false;
    const filter_id = catalog_metadata.get_id_from_node(node);
    // Include if this node is in the filters list
    if (filter_ids.has(filter_id)) return true;
    // Exclude otherwise
    return false;
  });
  return (
    <div className="grid gap-4 @xl/cell:grid-cols-2 @5xl/cell:grid-cols-1">
      {filter_nodes.map((node) => {
        const filter_id = catalog_metadata.get_id_from_node(node);
        return (
          <FieldNodeContext.Provider value={node} key={filter_id}>
            <FilterCard />
          </FieldNodeContext.Provider>
        );
      })}
    </div>
  );
}

export function AddFilterDropdown() {
  const column_ids = useColumnIDs();
  const filter_ids = useFilterIDs();
  const catalog_metadata = useCatalogMetadata();
  const add_filter = useAddFilter();

  const items = [...column_ids]
    .map((field_id) => {
      if (!catalog_metadata) return null;
      const is_active = filter_ids.has(field_id);
      const node = catalog_metadata?.get_node_from_id(field_id);
      const titles = get_field_titles(node);
      return {
        field_id,
        node,
        label: titles.join(` `),
        display_label: <FieldTitles titles={titles} />,
        is_active
      };
    })
    .filter(Boolean)
    .filter((d) => !d.is_active)
    .map((d) => ({
      key: d.field_id,
      value: d.label,
      label: d.display_label,
      onSelect: () => add_filter(d.node)
    }));

  return (
    <Combobox
      placeholder="Add a filter..."
      emptyText="No filters found."
      items={items}
      autoClose
    />
  );
}

function FilterCard() {
  const field_node = useFieldNode();
  const metadata = field_node.data;
  const field_type = get_field_type(metadata);

  const titles = <FieldTitles titles={get_field_titles(field_node)} />;

  const units = metadata.units ? (
    <span>
      [<Katex>{metadata.units}</Katex>]
    </span>
  ) : null;

  const title_and_units = (
    <div className="flex items-center justify-between whitespace-nowrap">
      <Label className="flex gap-x-2 font-mono text-[clamp(0.7rem,5cqi,1rem)]">
        {titles}
        {units}
      </Label>
      <div className="flex gap-x-2">
        <ResetFiltersButton node={field_node} />
        <RemoveFilterButton node={field_node} />
      </div>
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
      case `STRING`:
        return <TextFilterControl />;
      default:
        return <div>not yet implemented: {field_type}</div>;
    }
  })();
  return (
    <Card className="h-[min-content] space-y-4 bg-secondary p-4 @container/filter">
      {title_and_units}
      {filter_control}
    </Card>
  );
}

function ResetFiltersButton({ node }) {
  const reset_filter = useResetFilter();
  return (
    <Button
      size="sm"
      variant="ghost"
      className="h-5 px-0"
      onClick={() => reset_filter(node)}
    >
      <RotateCcw className="h-4 w-4" />
    </Button>
  );
}

function RemoveFilterButton({ node }: { node: CatalogHierarchyNode }) {
  const metadata = node.data;
  const is_leaf = is_leaf_node(node);
  const can_remove = metadata.required !== true;
  const remove_filter = useRemoveFilter();

  if (!is_leaf) return null;
  if (!can_remove) return null;

  return (
    <Button
      size="sm"
      variant="ghost"
      className="h-5 px-0"
      onClick={() => remove_filter(node)}
    >
      <Trash2 className="h-4 w-4" />
    </Button>
  );
}

function RangeFilterControl() {
  const field_node = useFieldNode();
  const metadata = field_node.data;
  assert_numeric_field_stats(metadata);
  const field_type = get_field_type(metadata);
  const filters = useFilterValues();
  const field_id = useCatalogMetadata().get_id_from_node(field_node);
  const filter_value_raw: FilterValueRaw = filters[field_id] ?? undefined;

  const { min, max } = metadata.stats;

  // TODO: Replace with getCurrentFilterMin and getCurrentFilterMax
  const low =
    typeof filter_value_raw === `object` && `gte` in filter_value_raw
      ? filter_value_raw.gte
      : min;

  const high =
    typeof filter_value_raw === `object` && `lte` in filter_value_raw
      ? filter_value_raw.lte
      : max;

  const set_filter_value = useSetFilterValue();

  const [internal_low, set_internal_low] = React.useState<number>(low);
  const [internal_high, set_internal_high] = React.useState<number>(high);

  React.useEffect(() => set_internal_low(low), [low]);
  React.useEffect(() => set_internal_high(high), [high]);

  const debounce = 500;
  const debounced_low = useDebounce(internal_low, debounce ?? 0);
  const debounced_high = useDebounce(internal_high, debounce ?? 0);

  const is_integer = field_type === `INTEGER`;

  React.useEffect(() => {
    if (debounced_low === low) return;
    set_filter_value(field_node, {
      gte: is_integer ? Math.round(debounced_low) : debounced_low
    });
  }, [debounced_low]);

  React.useEffect(() => {
    if (debounced_high === high) return;
    set_filter_value(field_node, {
      lte: is_integer ? Math.round(debounced_high) : debounced_high
    });
  }, [debounced_high]);

  const step = d3.tickStep(min, max, 100);

  return (
    <div className="grid gap-4 text-xs">
      <NumberInput
        value={internal_low?.toString()}
        min={min}
        max={Math.min(internal_high, max)}
        onNumberInput={(value) => set_internal_low(value)}
      />
      <NumberInput
        value={internal_high?.toString()}
        min={Math.max(internal_low, min)}
        max={max}
        onNumberInput={(value) => set_internal_high(value)}
      />
      <Slider
        min={min}
        max={max}
        value={[internal_low, internal_high]}
        onValueChange={([new_low, new_high]) => {
          if (new_low !== internal_low) {
            set_internal_low(new_low);
          }
          if (new_high !== internal_high) {
            set_internal_high(new_high);
          }
        }}
        step={step}
        className="my-2"
      />
    </div>
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
  const field_id = useCatalogMetadata().get_id_from_node(field_node);
  const filters = useFilterValues();
  const filter_value_raw: FilterValueRaw = filters[field_id] ?? null;
  const is_required = metadata.required === true;
  const has_default = `default` in metadata;
  // Only show "All" option if the field is not required and has no default
  const all_option =
    is_required && has_default
      ? null
      : {
          text: `All`,
          value: null,
          count: null,
          value_as_string: String(null)
        };
  const options = [all_option, ...join_enums(metadata)].filter(Boolean);
  const value = options.find(
    (d) => d.value === filter_value_raw
  )?.value_as_string;

  const set_filter_value = useSetFilterValue();

  const items = options.map(({ text, count, value_as_string }) => {
    const label = count ? `${text} (${format.commas(count)} rows)` : text;
    return (
      <SelectItem
        key={text}
        value={value_as_string}
        className="whitespace-nowrap"
      >
        {label}
      </SelectItem>
    );
  });

  return (
    <Select
      value={value}
      onValueChange={(value_as_string) => {
        const value = options.find(
          (d) => d.value_as_string === value_as_string
        )?.value;
        set_filter_value(field_node, value);
      }}
    >
      <SelectTrigger className="whitespace-nowrap text-[clamp(0.8rem,4.6cqi,1rem)]">
        <SelectValue />
      </SelectTrigger>
      <SelectContent position="popper">
        <SelectGroup>{items}</SelectGroup>
      </SelectContent>
    </Select>
  );
}

function TextFilterControl() {
  const field_node = useFieldNode();
  const field_id = useCatalogMetadata().get_id_from_node(field_node);
  const filters = useFilterValues();
  const filter_value_raw: FilterValueRaw = filters?.[field_id] ?? null;

  const initial_value = (() => {
    if (typeof filter_value_raw !== `object` || filter_value_raw === null)
      return undefined;
    if (!(`wildcard` in filter_value_raw)) return undefined;
    return filter_value_raw.wildcard;
  })();

  const [value, set_value] = React.useState<string>(initial_value);
  const debounced = useDebounce(value, 500);

  const set_filter_value = useSetFilterValue();
  const clear_filter_value = useClearFilterValue();

  React.useEffect(() => {
    if (typeof debounced !== `string`) return;
    if (debounced === ``) {
      clear_filter_value(field_node);
      return;
    }
    set_filter_value(field_node, {
      wildcard: debounced
    });
  }, [debounced]);

  return (
    <Input
      type="text"
      value={value}
      onInput={(event) => set_value(event.currentTarget.value)}
    />
  );
}
