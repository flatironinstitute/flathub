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

export default function PlotSection() {
  return (
    <CellSection label="plot" className="space-y-4">
      <Placeholder className="h-12">TODO: Plot Controls</Placeholder>
      <Placeholder>TODO: Plot</Placeholder>
    </CellSection>
  );
}
