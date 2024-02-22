import {
  useQueryClient,
  type UseQueryOptions,
  type UseQueryResult
} from "@tanstack/react-query";
import { Info } from "lucide-react";
import { Alert, AlertDescription, AlertTitle } from "./ui/alert";
import type { useAxisConfig } from "./PlotHelpers";

export function StatusBox({
  title,
  description
}: {
  title?: React.ReactNode;
  description?: React.ReactNode;
}) {
  if (!title) return null;
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

export type StatusBoxProps = React.ComponentProps<typeof StatusBox>;

// Status could be:
// - Need to choose a field
// - Log mode error
// - Successful query, empty response
// - Loading
// - Some other custom message
export function useStatus({
  query,
  query_key,
  axes,
  no_data = false,
  message
}: {
  query: UseQueryResult;
  query_key: UseQueryOptions[`queryKey`];
  axes?: Array<ReturnType<typeof useAxisConfig>>;
  no_data?: boolean;
  message?: string;
}): StatusBoxProps | null {
  const missing_axis = axes?.find(
    (d) => typeof d.field_name === `undefined` || d.field_name === null
  );
  const log_error = axes?.find((d) => d.log_mode_error_message);
  if (message) {
    return { title: message };
  } else if (missing_axis) {
    return { title: `Choose a field for: ${missing_axis.key}` };
  } else if (log_error) {
    return {
      title: `Log Scale Error`,
      description: log_error.log_mode_error_message
    };
  } else if (query.isFetching) {
    return {
      title: `Loading...`,
      description: <CancelQueryButton queryKey={query_key} />
    };
  } else if (query.isSuccess && no_data) {
    return {
      title: `No data`,
      description: `The current filters result in an empty dataset`
    };
  } else {
    return null;
  }
}

function CancelQueryButton({
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
