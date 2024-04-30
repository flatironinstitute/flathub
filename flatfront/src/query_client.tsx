import { QueryClient } from "@tanstack/react-query";

export const query_client = new QueryClient({
  defaultOptions: {
    queries: {
      structuralSharing: false,
      staleTime: Infinity
    }
  }
});
