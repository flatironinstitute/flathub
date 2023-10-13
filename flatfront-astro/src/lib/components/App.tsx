import { QueryClient, QueryClientProvider } from "@tanstack/react-query";
import clsx from "clsx";
import {
  Provider as AppControllerProvider,
  useSaveAndRestoreState
} from "../app-state";
import GlobalControls from "./GlobalControls";
import Cells from "./Cells";
import { useToggleDarkMode } from "../dark-mode";

const query_client = new QueryClient();

export default function App() {
  return (
    <QueryClientProvider client={query_client}>
      <AppControllerProvider>
        <Main />
      </AppControllerProvider>
    </QueryClientProvider>
  );
}

function Main() {
  useToggleDarkMode();
  useSaveAndRestoreState();
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
