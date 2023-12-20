import { log } from "@/utils";
import React from "react";
import { FilterSection, AddFilterDropdown } from "./FilterSection";

export function FloatingSection({
  resultSectionRef: result_section_ref
}: {
  resultSectionRef: React.RefObject<HTMLDivElement>;
}) {
  const ref = React.useRef<HTMLDivElement>(null);
  const [results_rect, set_results_rect] = React.useState<DOMRect | null>(null);
  const [rect, set_rect] = React.useState<DOMRect | null>(null);
  const [window_height, set_window_height] = React.useState<number | null>(
    null
  );
  React.useEffect(() => {
    const measure = () => {
      const result_section = result_section_ref.current;
      if (result_section) {
        const result_section_rect = result_section.getBoundingClientRect();
        set_results_rect(result_section_rect);
      }
      const container = ref.current;
      if (container) {
        const container_rect = container.getBoundingClientRect();
        set_rect(container_rect);
      }
      set_window_height(window.innerHeight);
    };
    window.addEventListener(`scroll`, measure);
    window.addEventListener(`resize`, measure);
    return () => {
      window.removeEventListener(`scroll`, measure);
      window.addEventListener(`resize`, measure);
    };
  }, []);
  const top = (() => {
    if (!rect) return null;
    if (rect.height > results_rect.bottom)
      return results_rect.bottom - rect.height;
    if (results_rect.top > 0) return results_rect.top;
    return 0;
  })();
  const left = (() => {
    if (!rect) return null;
    return results_rect.right;
  })();
  return (
    <div
      ref={ref}
      className="fixed max-w-[500px] space-y-4 p-10"
      style={{
        top: `${top}px`,
        left: `${left}px`,
        right: 0
      }}
    >
      <AddFilterDropdown />
      <FilterSection />
    </div>
  );
}
