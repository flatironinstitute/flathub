import { QueryClient, QueryClientProvider } from "@tanstack/react-query";
import clsx from "clsx";
import * as hooks from "../hooks";
import GlobalControls from "./GlobalControls";
import Cells from "./Cells";
import {
  Provider as AppControllerProvider,
  useSaveAndRestoreState
} from "./AppController";

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
  hooks.useToggleDarkMode();
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
