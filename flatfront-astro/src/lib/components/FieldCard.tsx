import type {
  CatalogHierarchyNode,
  schema,
  HistogramPostRequestBody,
  HistogramResponse
} from "../types";

import React from "react";
import clsx from "clsx";
import * as d3 from "d3";
import { useQuery } from "@tanstack/react-query";
import * as Plot from "@observablehq/plot";
import { useIntersectionObserver } from "@uidotdev/usehooks";
import {
  assert_numeric_field_stats,
  create_context_helper,
  fetch_api_post,
  format,
  get_field_type,
  has_numeric_field_stats,
  is_leaf_node,
  join_enums,
  log,
  should_use_log_scale
} from "../shared";
import ObservablePlot from "./ObservablePlot";
import Katex from "./Katex";
import { Placeholder } from "./Primitives";
import { RangeFilterControl, SelectFilterControl } from "./FilterControls";
import { useCatalogID } from "./CatalogContext";

const [useFieldNode, FieldNodeProvider] =
  create_context_helper<CatalogHierarchyNode>(`FieldNode`);

export { useFieldNode };

export default function FieldCard({
  filter,
  fieldNode: field_node
}: {
  filter?: boolean;
  fieldNode: CatalogHierarchyNode;
}) {
  const [show_details, set_show_details] = React.useState(!filter);

  const metadata = field_node.data;

  const field_type = get_field_type(metadata);

  const field_title = <Katex>{metadata.title}</Katex>;

  const field_units = metadata.units ? (
    <div>
      [<Katex>{metadata.units}</Katex>]
    </div>
  ) : null;

  const title_and_units = (
    <div>
      <div className="flex space-x-2">
        {field_title}
        {field_units}
      </div>
      <div className="opacity-40">{metadata.name}</div>
    </div>
  );

  const filter_control = (() => {
    if (!filter) return null;
    if (!is_leaf_node(field_node)) return null;
    switch (field_type) {
      case `INTEGER`:
      case `FLOAT`:
        return <RangeFilterControl />;
      case `ENUMERABLE_INTEGER`:
      case `LABELLED_ENUMERABLE_INTEGER`:
      case `LABELLED_ENUMERABLE_BOOLEAN`:
        return <SelectFilterControl />;
      default:
        return <div>not yet implemented</div>;
    }
  })();

  const details = (() => {
    if (!show_details) return null;
    const field_description = metadata.descr ? (
      <div className="overflow-hidden opacity-80">
        <Katex>{metadata.descr}</Katex>
      </div>
    ) : null;
    const stats = (() => {
      if (!metadata.stats) return null;
      switch (field_type) {
        case `INTEGER`:
        case `FLOAT`:
          if (!has_numeric_field_stats(metadata)) return null;
          return <NumericFieldStats />;
        case `ENUMERABLE_INTEGER`:
        case `LABELLED_ENUMERABLE_INTEGER`:
        case `LABELLED_ENUMERABLE_BOOLEAN`:
          return <EnumerableFieldStats />;
        default:
          return null;
      }
    })();
    const chart = has_numeric_field_stats(metadata) ? (
      <NumericFieldHistogram />
    ) : null;
    const debug_section = (
      <>
        <div>{field_type}</div>
        <pre>{JSON.stringify({ ...metadata, sub: undefined }, null, 2)}</pre>
      </>
    );
    return (
      <>
        {field_description}
        {stats}
        {chart}
        {debug_section}
      </>
    );
  })();

  const show_details_button = !show_details && (
    <button className="underline" onClick={() => set_show_details(true)}>
      Show details
    </button>
  );

  const contents = (
    <div className="space-y-4">
      {title_and_units}
      {filter_control}
      {details}
      {show_details_button}
    </div>
  );

  return (
    <FieldNodeProvider value={field_node}>
      <FieldCardWrapper>{contents}</FieldCardWrapper>
    </FieldNodeProvider>
  );
}

function FieldCardWrapper({
  children,
  className
}: {
  children: React.ReactNode;
  className?: string;
}): React.JSX.Element {
  const field_node = useFieldNode();
  const node_depth = (field_node?.depth ?? 0) - 1;
  return (
    <div
      data-type="FieldCardWrapper"
      className={clsx(FieldCardWrapper.className, className)}
      style={{
        marginLeft: `${node_depth * 2}ch`
      }}
    >
      {children}
    </div>
  );
}
FieldCardWrapper.className = clsx(
  `rounded-md px-4 pt-3 py-5 overflow-hidden`,
  `ring-1 ring-black/30 dark:ring-white/30`
);

function NumericFieldStats() {
  const field_node = useFieldNode();
  const metadata = field_node.data;
  assert_numeric_field_stats(metadata);

  const format = (number: number): string => {
    const exponent = Math.floor(Math.log10(Math.abs(number)));
    if (number === 0) return `0`;
    if (exponent >= 5) return d3.format(`,.2~e`)(number);
    if (exponent >= 3) return d3.format(`,.0~f`)(number);
    if (exponent >= -3) return d3.format(`,.4~r`)(number);
    return d3.format(`,.2~e`)(number);
  };

  const { min, max, avg } = metadata.stats;

  const numbers = [
    [min, `min`],
    [avg, `avg`],
    [max, `max`]
  ].map(([number, label]: [number, string]) => {
    return (
      <div key={label}>
        <div className="text-center">{format(number)}</div>
        <div className="text-center uppercase opacity-50">{label}</div>
      </div>
    );
  });

  return <div className="flex justify-center gap-x-10">{numbers}</div>;
}

function EnumerableFieldStats() {
  const field_node = useFieldNode();
  const metadata = field_node.data;

  const joined = join_enums(metadata);

  const pills = joined.map(({ text, count }, index) => {
    const count_string = count ? ` (${format.commas(count)} rows)` : ``;
    return (
      <div
        key={index}
        className={`rounded-full px-2 py-0.5 ring-1 ring-black/30 dark:ring-white/30`}
      >
        {text} {count_string}
      </div>
    );
  });

  return (
    <>
      <div className="uppercase">values</div>
      <div className="flex flex-wrap gap-x-2 gap-y-2">{pills}</div>
    </>
  );
}

function NumericFieldHistogram() {
  const catalog_id = useCatalogID();

  const field_node = useFieldNode();
  const metadata = field_node.data;
  assert_numeric_field_stats(metadata);

  const { ref, visible: should_fetch } = useDelayVisible();

  const { min, max, avg } = metadata.stats;
  const should_use_log = should_use_log_scale(min, max, avg);

  const histogram_query = useQuery({
    queryKey: [`field_histogram`, catalog_id, metadata.name],
    queryFn: () => {
      const histogram_field_config: schema.components["schemas"]["HistogramList"] =
        {
          field: metadata.name,
          size: 30
        };
      const request_body = {
        fields: [histogram_field_config]
      } as HistogramPostRequestBody;
      return fetch_api_post<HistogramPostRequestBody, HistogramResponse>(
        `/${catalog_id}/histogram`,
        request_body
      );
    },
    staleTime: Infinity,
    enabled: should_fetch
  });

  const histogram_size = histogram_query.data?.sizes[0];

  const histogram_data = (histogram_query.data?.buckets ?? []).map((d) => ({
    x: +d.key[0],
    y: d.count
  }));

  // log(`histogram data`, metadata.name, histogram_data);

  const plot = Plot.plot({
    marginLeft: 70,
    height: 150,
    style: {
      background: `transparent`,
      overflow: `visible`,
      fontSize: `100%`
    },
    y: {
      label: null,
      ticks: 2,
      nice: 3,
      tickFormat: (d) =>
        d === 0 || d === 1 ? "" : d3.format(".2~s")(d).replace("G", "B"),
      // type: should_use_log ? "log" : undefined,
      grid: true,
      type: should_use_log ? "log" : "linear"
      // insetBottom: 10
    },
    x: {
      label: null,
      ticks: 3,
      tickFormat: (d) => d3.format(".4~g")(d).replace("G", "B")
    },
    marks: [
      Plot.rectY(histogram_data, {
        x1: "x",
        x2: (d) => d.x + histogram_size,
        y1: should_use_log ? 1 : 0,
        y2: "y",
        insetLeft: 2,
        insetRight: 2
      }),
      Plot.ruleY([should_use_log ? 1 : 0])
      // Plot.dot(histogram_data, {
      //   x: "x",
      //   y: "y"
      // })
    ]
  });

  const contents = histogram_query.isLoading ? (
    <Placeholder>Loading histogram...</Placeholder>
  ) : (
    <ObservablePlot plot={plot} />
  );

  return <div ref={ref}>{contents}</div>;
}

function useDelayVisible() {
  const [visible, set_visible] = React.useState(false);
  const [ref, entry] = useIntersectionObserver();
  const on_screen = entry?.isIntersecting;
  const timeout_ref = React.useRef<number | undefined>(undefined);
  // If on_screen is `true` for at least one second, set visible to true.
  React.useEffect(() => {
    if (on_screen) {
      timeout_ref.current = window.setTimeout(() => {
        set_visible(true);
      }, 1000);
    }
    return () => {
      clearTimeout(timeout_ref.current);
    };
  }, [on_screen]);
  return { ref: ref as React.RefObject<HTMLDivElement>, visible };
}

// function NumericFieldStats() {
//   const field_node = useFieldNode()();

//   const metadata = field_node.data;

//   log(`🐛`, metadata);

//   assert_numeric_field_stats(metadata);

//   const { min, max, avg } = metadata.stats;

//   // const should_use_log = should_use_log_scale(min, max, avg);

//   // const tiny = 1e-12;

//   const domain = [min, max];
//   // const log_domain = [min === 0 ? (max > 0 ? tiny : -tiny) : min, max];

//   const range = [0, 100];

//   const linear_scale = d3.scaleLinear(domain, range).clamp(true);
//   // const log_scale = d3.scaleLog(log_domain, range).clamp(true);

//   const scale = d3.scaleLinear(domain, range).clamp(true);

//   const min_percent = scale(min);
//   const max_percent = scale(max);
//   const avg_percent = scale(avg);
//   const width_percent = Math.abs(max_percent - min_percent);

//   const horizontal_line = (
//     <div
//       className="absolute inset-y-1/2 border-t border-t-current opacity-40"
//       style={{ width: `${width_percent}%` }}
//     ></div>
//   );

//   const tick_values = linear_scale.ticks(5);

//   // if (should_use_log) {
//   //   log(`should use log!!`, metadata.name, {
//   //     min,
//   //     max,
//   //     avg,
//   //     avg_percent,
//   //     tick_values
//   //   });
//   // }

//   const tick_class = clsx(
//     `absolute inset-y-1/2 h-4 border-l border-l-current`,
//     `-translate-x-1/2 -translate-y-1/2 transform`
//   );

//   const lines = [avg_percent].map((percent, index) => {
//     return (
//       <div
//         key={index}
//         className={tick_class}
//         style={{ left: `${percent}%` }}
//       ></div>
//     );
//   });

//   const ticks = tick_values.map((value, index) => {
//     return (
//       <div
//         key={index}
//         className={clsx(tick_class, `opacity-40`)}
//         style={{ left: `${scale(value)}%` }}
//       ></div>
//     );
//   });

//   return (
//     <>
//       <div className="relative mx-10 h-20">
//         {horizontal_line}
//         {ticks}
//         {lines}
//       </div>
//     </>
//   );
// }
