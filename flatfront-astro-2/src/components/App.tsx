import React from "react";
import { QueryClient, QueryClientProvider } from "@tanstack/react-query";
import clsx from "clsx";
import {
  AppStateProvider,
  useSaveStateInURL,
  useAppState
} from "@/components/contexts/AppStateContext";
import { log } from "@/utils";
import { GlobalControls } from "./GlobalControls";
import { Cells } from "./Cells";

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
      <AppStateProvider>
        <Main />
      </AppStateProvider>
    </QueryClientProvider>
  );
}

function Main() {
  useSaveStateInURL();
  const app_state = useAppState();
  React.useEffect(() => {
    log(`🌳 Current app state:`, app_state);
  }, [app_state]);
  return (
    <main
      className={`mb-10 mt-10 flex flex-col items-center justify-center gap-y-10`}
    >
      <Cells />
      <GlobalControls />
    </main>
  );
}
