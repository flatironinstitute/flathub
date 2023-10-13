import React from "react";

export function useDelayVisible(
  ref: React.RefObject<HTMLElement>,
  delay: number
): boolean {
  const [visible, setVisible] = React.useState(false);
  React.useEffect(() => {
    let timeout: number;
    const observer = new IntersectionObserver(
      (entries) => {
        if (entries[0].isIntersecting) {
          timeout = window.setTimeout(() => {
            setVisible(true);
          }, delay);
        }
        if (!entries[0].isIntersecting) {
          clearTimeout(timeout);
        }
      },
      { threshold: 0.5 }
    );
    if (ref.current) {
      observer.observe(ref.current);
    }
    return () => {
      if (ref.current) {
        observer.unobserve(ref.current);
      }
    };
  }, [ref, delay]);
  return visible;
}
