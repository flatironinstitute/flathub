import clsx from "clsx";
import { format } from "@/utils";
import { useCatalogMetadata } from "@/components/contexts/CatalogMetadataContext";
import {
  Dialog,
  DialogContent,
  DialogHeader,
  DialogTitle,
  DialogTrigger
} from "@/components/ui/dialog";
import { Katex } from "@/components/ui/katex";
import { Button } from "@/components/ui/button";

export function AboutThisCatalog() {
  const catalog_metadata = useCatalogMetadata();
  const num_variables = catalog_metadata?.hierarchy?.leaves?.()?.length;
  const response = catalog_metadata?.response;
  const { title, descr, count } = response ?? {};
  return (
    <Dialog>
      <DialogTrigger asChild disabled={!response}>
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
        <p>Variables: {format.commas(num_variables)}</p>
      </DialogContent>
    </Dialog>
  );
}
