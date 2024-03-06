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
  "SC-SAM Example": `N4IgxgpgNlDOIC5RgIYBcVQPYHMD6kMeAjAOwAMAHAGwDMxVArLQJwBMiy0UeAlgCaJw6TLgLcSFGvSasOAGmEZs%2BAUNhhYKALYhFAB2xp4SEIaxpJVOg0ot2xTmaN40AT30QhAC16w0uABOOnrOFgRYAHZogVhwTgAeeCgJfkLa-iiBeKQARihsxLmMAGahSSl%2BeCp42lj8XggxAK4QAL5tBi5k1vQM5GxsLE7mlu6ePn4BOMG6XeFgUTFxJqAVqSYggbnNUDgQeA20ubT85F6K61U1dQ2ILe2dYZY90sTvbNSUpCMu440gbwQdDaFD6UKjCLRWLxUxXTYZDDZPIFIqlUJuZIbITbXb7Q4QY6nc7lLHXMS3RoPRSYyqwaoU%2BpUwKtDrzF5SGzEWiMcjUH6mSH-IS5LAJUYQlyLaErRJkhGZZH5QrFMo0%2BU4nZ7A5HE5nC4geEM-CU%2B4siDqunG2pMs2sp6Q15cxjctiUX7hYUIEAadBoCCBCXsqHLWFrDXexFZHLKtFqkC07He3HaglE-Wkq03W1Nc2WjbW025%2B1PWDeLAAdzwJV4UH9gVWICjStRqrtFq2WvxuuJzNZimCkX4WG0eC02kMjVA48niGIilgEAgggQ5CeNbrAbwADdMK1Gynu4S9STTDh-XOOlegA`
};
