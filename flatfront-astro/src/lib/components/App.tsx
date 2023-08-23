import GlobalControls from "./GlobalControls";
import Cells from "./Cells";
import clsx from "clsx";

export default function App() {
  return (
    <main
      className={clsx(`ms-auto me-auto mt-10 mb-10`, `flex flex-col gap-y-10`)}
      style={{ width: `min(2000px, 90vw)` }}
    >
      <Cells />
      <GlobalControls />
    </main>
  );
}
