import type { CatalogHierarchyNode } from "../types";
import { create_context_helper } from "../shared";

const [useFieldNode, Provider] =
  create_context_helper<CatalogHierarchyNode>(`FieldNode`);

export { useFieldNode, Provider as FieldNodeProvider };
