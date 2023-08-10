import GlobalControls from "./GlobalControls";
import Cells from "./Cells";

export default function App() {
  return (
    <main
      className="ms-auto me-auto flex flex-col gap-y-10"
      style={{ width: `min(900px, 90vw)` }}
    >
      <div className="h-10" />
      <GlobalControls />
      <Cells />
      <div className="h-10" />
    </main>
  );
}
