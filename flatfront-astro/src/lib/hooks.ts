import React from "react";
import { log } from "./shared";

export function useDebouncedValue<T>(value: T, delay: number): T {
  const [debounced, set_debounced] = React.useState(value);
  const timeout_ref = React.useRef<NodeJS.Timeout | null>(null);
  const string_ref = React.useRef<string | null>(null);

  React.useEffect(() => {
    if (string_ref.current === JSON.stringify(value)) {
      log(`Debounced value unchanged:`, value);
      return;
    }
    string_ref.current = JSON.stringify(value);
    if (timeout_ref.current) {
      clearTimeout(timeout_ref.current);
      timeout_ref.current = null;
    }

    timeout_ref.current = setTimeout(() => {
      log(`Debounced value changed:`, value);
      set_debounced(value);
    }, delay);

    return () => {
      if (timeout_ref.current) {
        clearTimeout(timeout_ref.current);
        timeout_ref.current = null;
      }
    };
  }, [value, delay]);

  return debounced;
}

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
