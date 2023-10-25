import React from "react";
import { QueryClient, QueryClientProvider } from "@tanstack/react-query";
import clsx from "clsx";
import {
  Provider as AppControllerProvider,
  useSaveAndRestoreState,
  useAppState
} from "../contexts/AppStateContext";
import { useToggleDarkMode } from "../dark-mode";
import { log } from "../shared";
import GlobalControls from "./GlobalControls";
import Cells from "./Cells";

const query_client = new QueryClient({
  defaultOptions: {
    queries: {
      structuralSharing: false,
      staleTime: Infinity
    }
  }
});

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
  const app_state = useAppState();
  React.useEffect(() => {
    log(`app_state`, app_state);
  }, [app_state]);
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
