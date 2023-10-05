import type * as Plot from "@observablehq/plot";

import React from "react";

type ObservablePlotType = ReturnType<typeof Plot.plot>;

export default function ObservablePlot({ plot }: { plot: ObservablePlotType }) {
  const ref = React.useRef<HTMLDivElement>(null);
  React.useEffect(() => {
    ref.current.append(plot);
    return () => plot.remove();
  }, [plot]);
  return <div ref={ref} />;
}
