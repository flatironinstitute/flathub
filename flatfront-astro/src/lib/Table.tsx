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

export default function TableSection() {

  const table_controls = (
    <div>
      <div>rows</div>
    </div>
  )

  return (
    <CellSection label="table" className="space-y-4">
      <div className="grid grid-cols-2 gap-x-4">
        <BigButton className="w-full">Select Columns</BigButton>
        {table_controls}
      </div>
      <Placeholder>TODO: Table</Placeholder>
    </CellSection>
  );
}
