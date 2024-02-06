import React from "react";
import { format as d3Format } from "d3-format";
import { type Row } from "@tanstack/react-table";
import type { CatalogHierarchyNode } from "@/types";
import { Katex } from "@/components/ui/katex";
import { Button } from "@/components/ui/button";
import {
  Dialog,
  DialogClose,
  DialogContent,
  DialogFooter,
  DialogHeader,
  DialogTitle,
  DialogTrigger
} from "@/components/ui/dialog";
import { Label } from "./ui/label";

function DialogValue({ children }: { children: React.ReactNode }) {
  return <div className="text-primary/50">{children}</div>;
}

export function FieldInfoDialog({
  row,
  children
}: {
  row: Row<CatalogHierarchyNode>;
  children: React.ReactNode;
}) {
  const node = row.original;
  const metadata = node.data;
  const title = <Katex>{metadata.title}</Katex>;
  const children_list = (() => {
    const has_children = "sub" in metadata && metadata.sub.length > 0;
    if (!has_children) return null;
    const children = metadata.sub.map((d, i) => [
      i > 0 && ", ",
      <Katex key={d.name}>{d.title}</Katex>
    ]);
    return ["Children", children];
  })();
  const stats = (() => {
    const has_stats = "stats" in metadata && metadata.stats.count;
    if (!has_stats) return [];
    return (
      <>
        <div>
          <Label>Count</Label>
          <DialogValue>{d3Format(`,`)(metadata.stats.count)}</DialogValue>
        </div>
        <div className="grid grid-cols-3">
          {["min", "avg", "max"].map((k) => {
            const v = metadata.stats[k];
            return (
              <div key={k}>
                <Label className="capitalize">{k}</Label>
                <DialogValue>{d3Format(`,.8g`)(Number(v))}</DialogValue>
              </div>
            );
          })}
        </div>
      </>
    );
  })();
  const fields = [
    ["Variable Name", metadata.name],
    metadata.descr ? ["Description", <Katex>{metadata.descr}</Katex>] : null,
    metadata.units ? ["Units", <Katex>{metadata.units}</Katex>] : null,
    ["Type", metadata.type],
    children_list
  ]
    .filter(Boolean)
    .filter((d) => d[1]) as [string, React.ReactNode][];
  return (
    <Dialog>
      <DialogTrigger asChild>{children}</DialogTrigger>
      <DialogContent>
        <DialogHeader>
          <DialogTitle className="text-3xl">{title}</DialogTitle>
        </DialogHeader>
        {fields.map(([label, value]) => {
          return (
            <div key={label}>
              <Label>{label}</Label>
              <DialogValue>{value}</DialogValue>
            </div>
          );
        })}
        {stats}
        <DialogFooter>
          <DialogClose asChild>
            <Button type="button">Close</Button>
          </DialogClose>
        </DialogFooter>
      </DialogContent>
    </Dialog>
  );
}
