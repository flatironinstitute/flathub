import type {
  schema,
  HistogramPostRequestBody,
  HistogramResponse
} from "./types";

import React from "react";
import clsx from "clsx";
import * as d3 from "d3";
import { useQuery } from "@tanstack/react-query";
import * as Plot from "@observablehq/plot";
import ObservablePlot from "./ObservablePlot";
import {
  hooks,
  get_field_type,
  assert_numeric_field_stats,
  log,
  should_use_log_scale,
  has_numeric_field_stats,
  format,
  fetch_api_post,
  is_leaf_node
} from "./shared";
import Katex from "./Katex";
import { Placeholder } from "./Primitives";
import { RangeFilterControl } from "./FilterControls";

export default function FieldCard({ filter }: { filter?: boolean }) {
  const field_node = hooks.useFieldNode();

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
      <div className="flex space-x-2 text-lg">
        {field_title}
        {field_units}
      </div>
      <div className="text-xs opacity-40">{metadata.name}</div>
    </div>
  );

  const filter_control = (() => {
    if (!filter) return null;
    if (!is_leaf_node(field_node)) return null;
    switch (field_type) {
      case `INTEGER`:
      case `FLOAT`:
        return <RangeFilterControl />;
      // case `ENUMERABLE_INTEGER`:
      // case `LABELLED_ENUMERABLE_INTEGER`:
      // case `LABELLED_ENUMERABLE_BOOLEAN`:
      //   return <EnumerableFieldStats />;
      default:
        return <div>not yet implemented</div>;
    }
  })();

  const field_description = metadata.descr ? (
    <div className="overflow-hidden text-xs opacity-80">
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

  const top_part = (
    <div className="space-y-4">
      {title_and_units}
      {filter_control}
      {field_description}
      {stats}
      {chart}
    </div>
  );

  const debug_section = (
    <>
      <div>{field_type}</div>
      <pre>{JSON.stringify({ ...metadata, sub: undefined }, null, 2)}</pre>
    </>
  );

  return <FieldCardWrapper>{top_part}</FieldCardWrapper>;
}

function FieldCardWrapper({
  children,
  className
}: {
  children: React.ReactNode;
  className?: string;
}): React.JSX.Element {
  const field_node = hooks.useFieldNode();
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
  `rounded-md text-md px-4 pt-3 py-5 overflow-hidden`,
  `ring-1 ring-black/30 dark:ring-white/30`
);

function NumericFieldStats() {
  const field_node = hooks.useFieldNode();
  const metadata = field_node.data;
  assert_numeric_field_stats(metadata);

  const format = (number): string => {
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
  ].map(([number, label]) => {
    return (
      <div key={label}>
        <div className="text-center text-lg">{format(number)}</div>
        <div className="text-center uppercase opacity-50">{label}</div>
      </div>
    );
  });

  return <div className="flex justify-center gap-x-10">{numbers}</div>;
}

function EnumerableFieldStats() {
  const field_node = hooks.useFieldNode();
  const metadata = field_node.data;
  const has_enum = metadata.enum && metadata.enum.length > 0 ? true : false;
  const has_terms =
    metadata.stats?.terms && metadata.stats.terms.length > 0 ? true : false;

  if (has_enum && !has_terms) {
    throw new Error(`Has enum but no terms: ${metadata.name}`);
  }

  const joined = has_enum
    ? metadata.enum.map((text, index) => {
        const count =
          metadata.stats.terms.find((term) => {
            const value_as_number = Number(term.value);
            if (Number.isNaN(value_as_number)) return false;
            return value_as_number === index;
          })?.count ?? null;
        return {
          text,
          count
        };
      })
    : metadata.stats.terms.map(({ value, count }) => {
        const text = value.toString();
        return {
          text,
          count
        };
      });

  const sorted = d3.sort(
    joined,
    has_enum ? (d) => -d.count : (d) => Number(d.text)
  );

  const pills = sorted.map(({ text, count }, index) => {
    const count_string = count ? ` (${format.commas(count)} rows)` : ``;
    return (
      <div
        key={index}
        className={`rounded-full px-2 py-0.5 text-xs ring-1 ring-black/30 dark:ring-white/30`}
      >
        {text} {count_string}
      </div>
    );
  });

  return (
    <>
      <div className="text-xs uppercase">values</div>
      <div className="flex flex-wrap gap-x-2 gap-y-2">{pills}</div>
    </>
  );
}

function NumericFieldHistogram() {
  const catalog_id = hooks.useCatalogID();

  const field_node = hooks.useFieldNode();
  const metadata = field_node.data;
  assert_numeric_field_stats(metadata);

  const delay = 1e3;

  const ref = React.useRef<HTMLDivElement>(null);

  const should_fetch = hooks.useDelayVisible(ref, delay);

  const { min, max, avg } = metadata.stats;
  const should_use_log = should_use_log_scale(min, max, avg);

  const histogram_query = useQuery({
    queryKey: [`field_histogram`, catalog_id, metadata.name],
    queryFn: () => {
      const histogram_field_config: schema.components["schemas"]["HistogramList"] =
        {
          field: metadata.name
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
    x: d.key[0],
    y: d.count
  }));

  // log(`histogram data`, metadata.name, histogram_data);

  const plot = Plot.plot({
    marginLeft: 70,
    height: 150,
    style: {
      background: `transparent`,
      overflow: `visible`,
      fontSize: `20px`
    },
    y: {
      label: null,
      ticks: 2,
      nice: 3,
      tickFormat: (d) =>
        d === 0 || d === 1 ? "" : d3.format(".2~s")(d).replace("G", "B"),
      // type: should_use_log ? "log" : undefined,
      grid: true,
      type: should_use_log ? "log" : "linear",
      // insetBottom: 10
    },
    x: {
      label: null,
      ticks: 3,
      tickFormat: (d) => d3.format(".2~s")(d).replace("G", "B")
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

// function NumericFieldStats() {
//   const field_node = hooks.useFieldNode();

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
