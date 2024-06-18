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
import { AllCatalogMetadataQueriesProvider } from "./contexts/CatalogMetadataContext";
import { TriangleAlert } from "lucide-react";
import { Card, CardContent, CardHeader, CardTitle } from "./ui/card";

export default function App() {
  log(`App mode:`, import.meta.env.MODE);
  log(`Build time:`, import.meta.env.BUILD_TIME);
  return (
    <QueryClientProvider client={query_client}>
      <AppStateProvider>
        <PlotDataProvider>
          <AllCatalogMetadataQueriesProvider>
            <Main />
          </AllCatalogMetadataQueriesProvider>
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
  // If current url does not end in a slash, redirect to the same url with a slash
  React.useEffect(() => {
    if (!location.pathname.endsWith("/")) {
      location.replace(location.href + "/");
    }
  }, []);
  const has_cells = Object.keys(app_state?.cells ?? {}).length > 0;
  return (
    <main
      className={`mb-10 mt-10 flex flex-col items-center justify-center gap-y-10`}
    >
      <GlobalControls />
      <Cells />
      {has_cells ? <ComparisonsCard /> : null}
      <BetaCard />
      <Examples />
    </main>
  );
}

function BetaCard() {
  return (
    <Card className="max-w-[40vw]">
      <CardHeader>
        <CardTitle>
          <TriangleAlert
            size={35}
            strokeWidth={1.75}
            className="inline-block"
          />{" "}
          Work in Progress
        </CardTitle>
      </CardHeader>
      <CardContent>
        The new FlatHUB interface is a work in progress. Not all features have
        been implemented, and you may encounter bugs.
      </CardContent>
    </Card>
  );
}
