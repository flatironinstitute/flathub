import {
  dispatch_action,
  hooks,
  assert_catalog_cell_id,
  log,
  Providers
} from "./shared";
import { BigButton, CellSection, CellWrapper, Placeholder } from "./Primitives";

export default function PlotSection() {
  return (
    <CellSection label="plot" className="space-y-4">
      <Placeholder className="h-12">TODO: Plot Controls</Placeholder>
      <Placeholder>TODO: Plot</Placeholder>
    </CellSection>
  );
}
