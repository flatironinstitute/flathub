import React from "react";
import renderMathInElement from "katex/contrib/auto-render";

export default function Katex({
  children,
  className,
}: {
  children: React.ReactNode;
  className?: string;
}): React.JSX.Element {
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
          { left: "\\(", right: "\\)", display: false },
        ],
      });
    }
  }, [ref]);
  return (
    <span ref={ref} className={className}>
      {children}
    </span>
  );
}
