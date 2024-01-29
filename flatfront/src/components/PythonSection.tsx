import { FLATHUB_API_BASE_URL, is_numeric_filter_value } from "@/utils";
import { useCatalogID } from "./contexts/CatalogIDContext";
import { useColumnNames } from "./contexts/ColumnsContext";
import {
  useFilterValues,
  useGetCurrentFilterMax,
  useGetCurrentFilterMin
} from "./contexts/FiltersContext";
import { useRandomConfig } from "./contexts/RandomContext";
import { useSort } from "./contexts/SortContext";
import { useCatalogMetadata } from "./contexts/CatalogMetadataContext";

export function PythonSection() {
  const endpoint = `${FLATHUB_API_BASE_URL}/api`;
  const column_names = useColumnNames();
  const catalog_name = useCatalogID();
  const random_config = useRandomConfig();
  const catalog_metadata = useCatalogMetadata();
  const filters_object = useFilterValues();
  const get_current_min = useGetCurrentFilterMin();
  const get_current_max = useGetCurrentFilterMax();
  let sample = ``;
  if (random_config?.sample < 1) {
    sample = `,\n  sample = ${random_config.sample}`;
  }
  let seed = ``;
  if (random_config?.seed) {
    seed = `,\n  seed = ${random_config.seed}`;
  }
  const sort_config = useSort();
  let sort = ``;
  if (sort_config?.length > 0) {
    const first = sort_config[0];
    const negative = first.order === `desc` ? `-` : ``;
    sort = `,\n  sort = ["${negative}${first.field}"]`;
  }
  let filters = ``;
  for (const [field_id, filter_value] of Object.entries(filters_object)) {
    const node = catalog_metadata?.get_node_from_id(field_id);
    if (node) {
      const name = node.data.name;
      let string_value = ``;
      if (is_numeric_filter_value(filter_value)) {
        const min = get_current_min(field_id);
        const max = get_current_max(field_id);
        string_value = `(${min}, ${max})`;
      } else {
        // TODO: Is this right?
        string_value = filter_value.toString();
      }
      filters += `,\n  ${name} = ${string_value}`;
    }
  }
  const code = `import flathub
${catalog_name} = flathub.Catalog("${catalog_name}", endpoint = "${endpoint}")
dat = ${catalog_name}.numpy(fields = ${JSON.stringify([...column_names])}${sample}${seed}${sort}${filters})`;
  return (
    <pre
      className="overflow-scroll border border-primary/10 bg-secondary p-4"
      style={{ overflowWrap: `break-word` }}
    >
      <code>{code}</code>
    </pre>
  );
}
