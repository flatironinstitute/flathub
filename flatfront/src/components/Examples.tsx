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
  "SC-SAM Example": `N4IgxgpgNlDOIC5RgIYBcVQPYHMD6kMeAjAOwAMAHAGwDMxVArLQJwBMiy0UeAlgCaJw6TLgLcSFGvSasOAGmEZs+AUNhhYKALYhFAB2xp4SEIaxpJVOg0ot2xTmaN40AT30QhAC16w0uABOOnrOFgRYAHZogVhwTgAeeCgJfkLa-iiBoUkpfngqeNpY-F4IMQCuEAC+1QYuZNb0DORsbCxO5pbunj5+ATjBuvXhYFExcSaguakmIIEARhVQOF6KM-mFxaWIlTV1YZaN0sSnbNSUpJ0uPWUg3hDo2ij6oV0R0bHxphtzGRjZRRuZKzISLZarHIgzZibZlPZA6GwAqwkrwwJVWojI5SGzEWiMcjUK6md63IQLLAJLpvFxjT6TRJI9KZQEgYF5ObglZrEC-FH4OG7DEQRGcgVFNHCzEHd7HPGMfFsSjXcLkhAgDToNAQQI07EfCbfabMjX-LKhDmgjXcyHrJESoXlEVi2aOqXOmUHWDeLAAdzwADNeFAdYEpiBzdlPaL5ksedKaopgpF+FhtHgtNpDGVQFmc4hiIpYBAIIIEOQDsHQ7q8AA3TBVCO23MgHA6wu1LtAA`
};
