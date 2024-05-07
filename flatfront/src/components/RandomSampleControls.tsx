import React from "react";
import { useDebounce } from "@uidotdev/usehooks";
import { Card } from "@/components/ui/card";
import { Label } from "./ui/label";
import { NumberInput } from "./NumberInput";
import { Slider } from "./ui/slider";
import { useRandomConfig, useSetRandomConfig } from "./contexts/RandomContext";

export function RandomSampleControls() {
  const set_random_config = useSetRandomConfig();
  const random_config = useRandomConfig();
  const [seed, set_seed] = React.useState(random_config?.seed ?? 0);
  const debounced_seed = useDebounce(seed, 500);
  React.useEffect(() => {
    set_random_config(`seed`, debounced_seed);
  }, [debounced_seed]);

  return (
    <div className="grid grid-cols-1 gap-4 @xl:grid-cols-2 @5xl:grid-cols-1">
      <Card className="h-[min-content] space-y-4 bg-secondary p-4 @container/filter">
        <Label className="block space-y-3">
          <span className="font-mono text-[15px]">Sample</span>
          <SliderWithText
            min={1e-9}
            max={1}
            value={random_config?.sample ?? 1}
            debounce={500}
            onValueChange={(new_value) =>
              set_random_config(`sample`, new_value)
            }
          />
        </Label>
      </Card>
      <Card className="h-[min-content] bg-secondary p-4 @container/filter">
        <Label className="block space-y-3">
          <span className="font-mono text-[15px]">Seed</span>
          <NumberInput
            value={seed}
            min="0"
            max="18446744073709552000"
            onNumberInput={(new_value) => set_seed(new_value)}
          />
        </Label>
      </Card>
    </div>
  );
}

function SliderWithText(props: {
  min: number;
  max: number;
  value: number;
  debounce?: number;
  onValueChange?: (value: number) => void;
}) {
  const { min, max, onValueChange, debounce } = props;
  const [internal_value, set_internal_value] = React.useState<number>(
    props.value
  );
  const debounced_value = useDebounce(internal_value, debounce ?? 0);

  React.useEffect(() => {
    set_internal_value(props.value);
  }, [props.value]);

  React.useEffect(() => {
    onValueChange?.(debounced_value);
  }, [debounced_value]);

  return (
    <div data-type="SliderWithText" className="@container">
      <div className="grid items-center gap-x-4 gap-y-4 @xs:grid-cols-2">
        <NumberInput
          value={internal_value?.toString()}
          min={min}
          max={max}
          onNumberInput={(value) => set_internal_value(value)}
        />
        <Slider
          min={min}
          max={max}
          step={0.01}
          value={[internal_value]}
          onValueChange={([new_value]) => {
            if (new_value !== internal_value) {
              set_internal_value(new_value);
            }
          }}
          className="my-2"
        />
      </div>
    </div>
  );
}
