import React from "react";
import { Input, type InputProps } from "@/components/ui/input";

export function NumberInput({
  value,
  min,
  max,
  onNumberInput,
  ...rest
}: InputProps & { onNumberInput?: (value: number) => void }) {
  const ref = React.useRef<HTMLInputElement>(null);
  const [internal, set_internal] = React.useState<string>(String(value) ?? ``);

  const get_validity_message = (string: string) => {
    const number = Number(string);
    if (!Number.isFinite(number)) return `Invalid number`;
    if (number < Number(min)) return `Must be greater than ${min.toString()}`;
    if (number > Number(max)) return `Must be less than ${max.toString()}`;
    return null;
  };

  const on_valid_input = (string: string) => {
    const number = Number(string);
    if (!Number.isFinite(number)) return;
    if (string?.toString() === value?.toString()) {
      // Not updating filter because it didn't change
      return;
    }
    onNumberInput?.(number);
  };

  const on_input = (string: string) => {
    set_internal(string);
    const message = get_validity_message(string);
    if (message) {
      ref.current.setCustomValidity(message);
    } else {
      ref.current.setCustomValidity("");
      on_valid_input(string);
    }
    ref.current.reportValidity();
  };

  React.useEffect(() => {
    on_input(value?.toString() ?? ``);
  }, [value]);

  return (
    <Input
      ref={ref}
      type="number"
      step="any"
      value={internal}
      onInput={(event: React.FormEvent<HTMLInputElement>) =>
        on_input(event.currentTarget.value)
      }
      {...rest}
    />
  );
}
