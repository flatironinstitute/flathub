import {
  useQueryClient,
  type UseQueryOptions,
  type UseQueryResult
} from "@tanstack/react-query";
import { Info } from "lucide-react";
import { Alert, AlertDescription, AlertTitle } from "./ui/alert";
import { cn } from "@/utils";

export function StatusBoxQuery({
  query,
  queryKey: query_key,
  title,
  description
}: {
  query?: UseQueryResult<any>;
  queryKey?: UseQueryOptions[`queryKey`];
  title?: string;
  description?: string;
}) {
  const query_client = useQueryClient();
  const no_data = query?.isLoading && query?.data?.length === 0;
  if (title) {
    return <StatusBox title={title} description={description} />;
  } else if (query?.isFetching) {
    return (
      <StatusBox
        title={`Loading...`}
        description={
          <button
            onClick={() => query_client.cancelQueries({ queryKey: query_key })}
            className="cursor-pointer underline"
          >
            Cancel
          </button>
        }
      />
    );
  } else if (no_data) {
    return (
      <StatusBox
        title={`No data`}
        description={`The current filters returned an empty dataset`}
      />
    );
  } else {
    return null;
  }
}

export function CancelQueryButton({
  queryKey: query_key
}: {
  queryKey: UseQueryOptions[`queryKey`];
}) {
  const query_client = useQueryClient();
  return (
    <button
      onClick={() => query_client.cancelQueries({ queryKey: query_key })}
      className="cursor-pointer underline"
    >
      Cancel
    </button>
  );
}

export function StatusBox({
  title,
  description
}: {
  title: React.ReactNode;
  description?: React.ReactNode;
}) {
  return (
    <div
      className={`grid h-full w-full items-center justify-center bg-secondary/80`}
    >
      <Alert>
        <Info className="h-4 w-4" />
        <AlertTitle>{title}</AlertTitle>
        <AlertDescription>{description}</AlertDescription>
      </Alert>
    </div>
  );
}
