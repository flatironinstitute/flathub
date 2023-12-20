import {
  useQueryClient,
  type UseQueryOptions,
  type UseQueryResult
} from "@tanstack/react-query";
import { Info } from "lucide-react";
import { Alert, AlertDescription, AlertTitle } from "./ui/alert";

export function StatusBox({
  query,
  queryKey: query_key
}: {
  query: UseQueryResult;
  queryKey: UseQueryOptions[`queryKey`];
}) {
  const query_client = useQueryClient();
  const title = query.isFetching ? `Loading...` : `No Data`;
  const description = query.isFetching ? (
    <button
      onClick={() => query_client.cancelQueries({ queryKey: query_key })}
      className="cursor-pointer underline"
    >
      Cancel
    </button>
  ) : (
    `There is no data to display.`
  );
  return (
    <div className="grid h-[200px] items-center justify-center bg-secondary">
      <Alert>
        <Info className="h-4 w-4" />
        <AlertTitle>{title}</AlertTitle>
        <AlertDescription>{description}</AlertDescription>
      </Alert>
    </div>
  );
}
