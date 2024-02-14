import { useAppState } from "./contexts/AppStateContext";
import { Card, CardContent, CardHeader, CardTitle } from "./ui/card";

export function Comparisons() {
  const app_state = useAppState();
  return (
    <Card>
      <CardHeader>
        <CardTitle>Comparison</CardTitle>
      </CardHeader>
      <CardContent>[TO DO]</CardContent>
    </Card>
  );
}
