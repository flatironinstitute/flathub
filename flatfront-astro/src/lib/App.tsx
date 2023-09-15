import clsx from "clsx";
import GlobalControls from "./GlobalControls";
import Cells from "./Cells";

export default function App() {
  return (
    <main
      className={clsx(`mb-10 me-auto ms-auto mt-10`, `flex flex-col gap-y-10`)}
      style={{ width: `min(2000px, 90vw)` }}
    >
      <Cells />
      <GlobalControls />
    </main>
  );
}
