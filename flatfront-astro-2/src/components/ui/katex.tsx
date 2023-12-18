import React from "react";
import renderMathInElement from "katex/contrib/auto-render";

export const Katex = React.forwardRef<
  HTMLSpanElement,
  React.HTMLAttributes<HTMLSpanElement>
>(({ children, ...props }) => {
  const ref = React.useRef<HTMLSpanElement>(null);
  React.useEffect(() => {
    if (ref.current !== null) {
      renderMathInElement(ref.current, {
        output: "mathml",
        throwOnError: false,
        trust: true,
        delimiters: [
          { left: "$$", right: "$$", display: false },
          { left: "$", right: "$", display: false },
          { left: "\\(", right: "\\)", display: false }
        ]
      });
    }
  }, [ref, ref.current, children]);
  return (
    <span ref={ref} dangerouslySetInnerHTML={{ __html: children }} {...props} />
  );
});
Katex.displayName = "Katex";
