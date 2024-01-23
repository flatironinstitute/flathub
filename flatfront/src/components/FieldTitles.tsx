import { Katex } from "@/components/ui/katex";

export function FieldTitles({ titles }: { titles: string[] }) {
  return (
    <>
      {titles.map((title, index) => (
        <Katex key={`${title}-${index}`}>{title}</Katex>
      ))}
    </>
  );
}
