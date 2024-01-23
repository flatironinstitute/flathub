import type * as Plot from "@observablehq/plot";

import React from "react";

type ObservablePlotType = ReturnType<typeof Plot.plot | typeof Plot.legend>;

export function ObservablePlot({
  plot,
  ...rest
}: { plot: ObservablePlotType } & React.HTMLAttributes<HTMLDivElement>) {
  const ref = React.useRef<HTMLDivElement>(null);
  useObservablePlot(ref, plot);
  return <div ref={ref} {...rest} />;
}

export function useObservablePlot(
  ref: React.MutableRefObject<any>,
  plot: ObservablePlotType
) {
  React.useEffect(() => {
    ref.current?.append(plot);
    return () => plot.remove();
  }, [ref.current, plot]);
}
