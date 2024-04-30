import React from "react";
import { QueryClientProvider } from "@tanstack/react-query";
import {
  AppStateProvider,
  useSaveStateInURL,
  useAppState
} from "@/components/contexts/AppStateContext";
import { log } from "@/utils";
import { GlobalControls } from "./GlobalControls";
import { Cells } from "./Cells";
import { ComparisonsCard } from "./Comparisons";
import { Examples } from "./Examples";
import { PlotDataProvider } from "./contexts/PlotDataContext";
import { query_client } from "../query_client";

export default function App() {
  log(`App mode:`, import.meta.env.MODE);
  log(`Build time:`, import.meta.env.BUILD_TIME);
  return (
    <QueryClientProvider client={query_client}>
      <AppStateProvider>
        <PlotDataProvider>
          <Main />
        </PlotDataProvider>
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
      {has_cells ? <ComparisonsCard /> : null}
      <Examples />
    </main>
  );
}
