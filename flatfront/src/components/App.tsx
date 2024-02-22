import React from "react";
import { QueryClient, QueryClientProvider } from "@tanstack/react-query";
import {
  AppStateProvider,
  useSaveStateInURL,
  useAppState
} from "@/components/contexts/AppStateContext";
import { log } from "@/utils";
import { GlobalControls } from "./GlobalControls";
import { Cells } from "./Cells";
import { Comparisons } from "./Comparisons";
import { Examples } from "./Examples";

const query_client = new QueryClient({
  defaultOptions: {
    queries: {
      structuralSharing: false,
      staleTime: Infinity
    }
  }
});

export default function App() {
  log(`App mode:`, import.meta.env.MODE);
  log(`Build time:`, import.meta.env.BUILD_TIME);
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
  const has_cells = Object.keys(app_state?.cells ?? {}).length > 0;
  return (
    <main
      className={`mb-10 mt-10 flex flex-col items-center justify-center gap-y-10`}
    >
      <GlobalControls />
      <Cells />
      {has_cells ? <Comparisons /> : null}
      <Examples />
    </main>
  );
}
