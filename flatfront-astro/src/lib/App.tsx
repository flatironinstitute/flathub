import { QueryClient, QueryClientProvider } from "@tanstack/react-query";
import clsx from "clsx";
import GlobalControls from "./GlobalControls";
import Cells from "./Cells";
import * as hooks from "./hooks";

const query_client = new QueryClient();

export default function App() {
  hooks.useToggleDarkMode();

  return (
    <QueryClientProvider client={query_client}>
      <main
        className={clsx(
          `mb-10 me-auto ms-auto mt-10`,
          `flex flex-col gap-y-10`
        )}
        style={{ width: `min(2000px, 90vw)` }}
      >
        <Cells />
        <GlobalControls />
      </main>
    </QueryClientProvider>
  );
}
