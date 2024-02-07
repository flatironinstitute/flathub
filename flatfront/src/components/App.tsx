import React from "react";
import { QueryClient, QueryClientProvider } from "@tanstack/react-query";
import {
  AppStateProvider,
  useSaveStateInURL,
  useAppState
} from "@/components/contexts/AppStateContext";
import { CardContent, Card, CardHeader, CardTitle } from "@/components/ui/card";
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
      <GlobalControls />
      <Cells />
      <Examples />
    </main>
  );
}

function Examples() {
  const links = Object.entries(examples).map(([name, state]) => {
    const url = new URL(location.pathname, location.href);
    const href = `${url.toString()}?app_state=${state}`;
    return (
      <a
        key={name}
        href={href}
        target="_blank"
        rel="noopener noreferrer"
        className="underline"
      >
        {name}
      </a>
    );
  });
  return (
    <Card>
      <CardHeader>
        <CardTitle>Examples</CardTitle>
      </CardHeader>
      <CardContent>{links}</CardContent>
    </Card>
  );
}

const examples = {
  "SC-SAM Example": `N4IgxgpgNlDOIC5RgIYBcVQPYHMD6kMeADIqGgJ4AOEi46muIANCAJYAmdqG2%2BhUEi3q9ceTnVhhYKALYgAvgtYAnFADsOWWXhmyqUWkhGN%2B0QaWN6DRgIytYECFwTElrA1jTxjPUwXMhY080PFsAdmJwgGYAJnDbAA5I%2BMkeNAgVEOEQsMiY2MS4gE4ANltiugALCHRZFCoc7FCIqOjogBZEso6O6Oq2WDRcNXl3EFywLHU0FSw4MgnmvLb4pJTwxYAPPBQtwbpZIZQVPHCAIxRY23OAVgAzYQpd-Z8QFXOAVygcCDwOCDRc7RDjEWisHZ7QZ4Ph4WRYAGIWafCCsZ5Q2AwsTwxEIZEQZRLLwrApFWJlCrbF4HBAgI4YU4XK43B5PalvD7fX7-QHA0HgkDo15Y-A4oz4iHskVwhHilQowm5VoxTrdUq9frGSGvQ7HRmXa53R6SjHSsVI%2BWo8BYT4zM2yxD3TCOcawKpYADueHubCgGRUPmQDFhAiCoE5Pz%2BAKBILBFpRrDQsky3OK92iEFKpXCsUdzqtOEweEoNDw53uHVsYGitlK8atAC88OosDQwHgIB0UN2UOdKggnXArQBVPAqCBDMtcv7FYodWrRYpgPND8Y%2Bv2ZPAAN0wKMDJhDgUsoELghLf3LlertcWG8QsUJEe50b5ceMOAyiFsSiUQA`
};
