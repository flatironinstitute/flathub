import clsx from "clsx";
import type { CatalogID } from "@/types";
import { format } from "@/utils";
import { useCatalogQuery } from "@/components/contexts/CatalogMetadataContext";
import {
  Dialog,
  DialogContent,
  DialogHeader,
  DialogTitle,
  DialogTrigger
} from "@/components/ui/dialog";
import { Katex } from "@/components/ui/katex";
import { Button } from "@/components/ui/button";

export function AboutThisCatalog({ catalog_id }: { catalog_id: CatalogID }) {
  const catalog_query = useCatalogQuery(catalog_id);
  const { title, descr, count } = catalog_query?.data ?? {};
  return (
    <Dialog>
      <DialogTrigger asChild disabled={!catalog_query.data}>
        <Button variant="outline">About This Catalog</Button>
      </DialogTrigger>
      <DialogContent className="max-h-[80dvh] overflow-y-scroll">
        <DialogHeader>
          <DialogTitle>{title}</DialogTitle>
        </DialogHeader>
        <Katex
          className={clsx(
            `text-sm`,
            `space-y-4 leading-[1.4]`,
            `[&_a]:underline [&_ul]:list-disc [&_ul]:space-y-4 [&_ul]:pb-3 [&_ul]:ps-10`
          )}
        >
          {descr}
        </Katex>
        <p>Rows: {format.commas(count)}</p>
      </DialogContent>
    </Dialog>
  );
}
