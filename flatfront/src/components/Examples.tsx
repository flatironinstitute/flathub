import { CardContent, Card, CardHeader, CardTitle } from "@/components/ui/card";

export function Examples() {
  const links = Object.entries(examples).map(([name, state]) => {
    const url = new URL(location.pathname, location.href);
    const href = `${url.toString()}?app_state=${state}`;
    return (
      <a
        key={name}
        href={href}
        target="_blank"
        rel="noopener noreferrer"
        className="underline"
      >
        {name}
      </a>
    );
  });
  return (
    <Card>
      <CardHeader>
        <CardTitle>Examples</CardTitle>
      </CardHeader>
      <CardContent>{links}</CardContent>
    </Card>
  );
}
const examples = {
  "SC-SAM Example": `N4IgxgpgNlDOIC5RgIYBcVQPYHMD6kMeADIqGgJ4AOEi46muIANCAJYAmdqG2%2BhUEi3q9ceTnVhhYKALYgAvgtYAnFADsOWWXhmyqUWkhGN%2B0QaWN6DRgIytYECFwTElrA1jTxjPUwXMhY080PFsAdmJwgGYAJnDbAA5I%2BMkeNAgVEOEQsMiY2MS4gE4ANltiugALCHRZFCoc7FCIqOjogBZEso6O6Oq2WDRcNXkPZrziRNLY6Kip21s6ACMsAA9s9xBcsCx1NBUsODJtidaChOTiVOM1vBQ1wbpZIZQVPHDllFjbZYBWABmwgo90ePhAKmWAFcoDgIHgOBBostohxiLRWHcHoM8Hw8LIsIjEAcoRBWCDsbBcWICUSECSIMpTl5JgUirEyhUTliwc9Xu9Pt9foDgaCnggIdDYfDEcjUejRZTqfhaUYGZixVS8ariSpSUzcud2l0en1uZq%2BRgBV8fv8gRqldrCWq9WTwFgoftlfjnYgAZhHAazpFprN5olFubKZa3h8bcL7SAKbyJZCYXCEUiUWjaEGWa1Q3NitFikkoymQC8rXGhXbFRW09LM3Kc8IAF4WiWwKhsdR4AHhSCxYhlrawKpYADu-bYUAyKh8yAYeIEQVAjYzsuzCvprtYaFkmQzxQB0QgpVK4VifoDbpwmDwlBoeGWAI6tjA0VspV1pNYHfULAaDAPAIA6FAIJQZZKgQf04DdABVPAVAgIYX3TeFimKDpahLMAb3grYAVnec8AAN0wUlFxMFdAksUB70EJ94Vfd9P2-E45yMWImQ3GUs3lIwGIyRBbCUJQgA`
};
