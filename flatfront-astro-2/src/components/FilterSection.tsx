import type { CatalogHierarchyNode, FilterValueRaw } from "@/types";
import React from "react";
import { useDebounce } from "@uidotdev/usehooks";
import { Trash2 } from "lucide-react";
import * as d3 from "d3";
import {
  assert_numeric_field_stats,
  format,
  get_field_titles,
  get_field_type,
  is_leaf_node,
  is_numeric_filter_value,
  join_enums
} from "@/utils";
import { useCatalogMetadata } from "@/components/contexts/CatalogMetadataContext";
import {
  useClearFilterValue,
  useFilterNames,
  useFilterValues,
  useRemoveFilter,
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
import { Input, type InputProps } from "@/components/ui/input";
import { Slider } from "@/components/ui/slider";

const FieldNodeContext = React.createContext(null);

const useFieldNode = () => {
  const field_node = React.useContext(FieldNodeContext);
  if (field_node === null) {
    throw new Error(`useFieldNode: value is null`);
  }
  return field_node;
};

export function AddFilterDropdown() {
  return <div>quick add filter</div>;
}

export function FilterSection() {
  const names = useFilterNames();
  const catalog_metadata = useCatalogMetadata();
  const leaves = catalog_metadata?.hierarchy?.leaves() ?? [];
  const filter_nodes = leaves.filter((node) => {
    // Exclude if is root node
    if (node.depth === 0) return false;
    const hash = catalog_metadata.get_hash_from_node(node);
    // Include if this node is in the filters list
    if (names.has(hash)) return true;
    // Exclude otherwise
    return false;
  });
  return (
    <div className="grid gap-4 @xl/cell:grid-cols-2 @4xl/cell:grid-cols-3">
      {filter_nodes.map((node, index) => {
        const hash = catalog_metadata.get_hash_from_node(node);
        return (
          <FieldNodeContext.Provider value={node} key={hash}>
            <FilterCard />
          </FieldNodeContext.Provider>
        );
      })}
    </div>
  );
}

function FilterCard() {
  const field_node = useFieldNode();
  const metadata = field_node.data;
  const field_type = get_field_type(metadata);

  const titles = (
    <>
      {get_field_titles(field_node).map((title, index) => (
        <Katex key={`${title}-${index}`}>{title}</Katex>
      ))}
    </>
  );

  const units = metadata.units ? (
    <span>
      [<Katex>{metadata.units}</Katex>]
    </span>
  ) : null;

  const title_and_units = (
    <div className="flex items-center justify-between whitespace-nowrap">
      <div className="flex gap-x-2 font-mono text-[clamp(0.7rem,5cqi,1rem)]">
        {titles}
        {units}
      </div>
      <RemoveFilterButton node={field_node} />
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
    <Card className="h-[min-content] space-y-4 p-4 @container/filter">
      {title_and_units}
      {filter_control}
    </Card>
  );
}

function RemoveFilterButton({ node }: { node: CatalogHierarchyNode }) {
  const metadata = node.data;
  const is_leaf = is_leaf_node(node);
  const can_remove = metadata.required !== true;
  const remove_filter = useRemoveFilter(node);

  if (!is_leaf) return null;
  if (!can_remove) return null;

  return (
    <Button
      size="sm"
      variant="ghost"
      className="h-5"
      onClick={() => remove_filter()}
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
  const field_id = metadata.name;
  const filters = useFilterValues();

  const filter_value_raw: FilterValueRaw = filters[field_id];
  const { min, max } = metadata.stats;

  const low = is_numeric_filter_value(filter_value_raw)
    ? filter_value_raw.gte
    : min;

  const high = is_numeric_filter_value(filter_value_raw)
    ? filter_value_raw.lte
    : max;

  const set_filter_value = useSetFilterValue(field_node);

  const [internal_low, set_internal_low] = React.useState<number>(low);
  const [internal_high, set_internal_high] = React.useState<number>(high);

  const debounce = 500;
  const debounced_low = useDebounce(internal_low, debounce ?? 0);
  const debounced_high = useDebounce(internal_high, debounce ?? 0);

  const is_integer = field_type === `INTEGER`;

  React.useEffect(() => {
    if (debounced_low === low) return;
    set_filter_value({
      gte: is_integer ? Math.round(debounced_low) : debounced_low
    });
  }, [debounced_low]);

  React.useEffect(() => {
    if (debounced_high === high) return;
    set_filter_value({
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
      />
    </div>
  );
}

function NumberInput({
  value,
  min,
  max,
  onNumberInput,
  ...rest
}: InputProps & { onNumberInput?: (value: number) => void }) {
  const ref = React.useRef<HTMLInputElement>(null);
  const [internal, set_internal] = React.useState<string>(String(value) ?? ``);

  const get_validity_message = (string: string) => {
    const number = Number(string);
    if (!Number.isFinite(number)) return `Invalid number`;
    if (number < Number(min)) return `Must be greater than ${min.toString()}`;
    if (number > Number(max)) return `Must be less than ${max.toString()}`;
    return null;
  };

  const on_valid_input = (string: string) => {
    const number = Number(string);
    if (!Number.isFinite(number)) return;
    if (string?.toString() === value?.toString()) {
      // Not updating filter because it didn't change
      return;
    }
    onNumberInput?.(number);
  };

  const on_input = (string: string) => {
    set_internal(string);
    const message = get_validity_message(string);
    if (message) {
      ref.current.setCustomValidity(message);
    } else {
      ref.current.setCustomValidity("");
      on_valid_input(string);
    }
    ref.current.reportValidity();
  };

  React.useEffect(() => {
    on_input(value?.toString() ?? ``);
  }, [value]);

  return (
    <Input
      ref={ref}
      type="number"
      step="any"
      value={internal}
      onInput={(event: React.FormEvent<HTMLInputElement>) =>
        on_input(event.currentTarget.value)
      }
      {...rest}
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
  const field_hash = useCatalogMetadata().get_hash_from_node(field_node);
  const filters = useFilterValues();
  const filter_value_raw: FilterValueRaw = filters[field_hash] ?? null;
  const options = [
    { text: `All`, value: null, value_as_string: String(null) },
    ...join_enums(metadata)
  ];
  const value = options.find((d) => d.value === filter_value_raw)
    ?.value_as_string;

  const set_filter_value = useSetFilterValue(field_node);

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
        const value = options.find((d) => d.value_as_string === value_as_string)
          ?.value;
        console.log({ value });
        set_filter_value(value);
      }}
    >
      <SelectTrigger className="whitespace-nowrap text-[min(4.6cqi,1rem)]">
        <SelectValue />
      </SelectTrigger>
      <SelectContent position="popper">
        <SelectGroup>{items}</SelectGroup>
      </SelectContent>
    </Select>
  );
}
