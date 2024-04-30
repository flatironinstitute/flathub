import { CardContent, Card, CardHeader, CardTitle } from "@/components/ui/card";

export function Examples() {
  const links = Object.entries(examples).map(([name, state]) => {
    const url = new URL(location.pathname, location.href);
    const href = `${url.toString()}?app_state=${state}`;
    return (
      <li key={name} className="list-none">
        <a
          href={href}
          target="_blank"
          rel="noopener noreferrer"
          className="underline"
        >
          {name}
        </a>
      </li>
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
  "SC-SAM Example": `N4IgxgpgNlDOIC5RgIYBcVQPYHMD6kMeAjAOwAMAHAGwDMxVArLQJwBMiy0UeAlgCaJw6TLgLcSFGvSasOAGmEZs+AUNhhYKALYhFAB2xp4SEIaxpJVOg0ot2xTmaN40AT30QhAC16w0uABOOnrOFgRYAHZogVhwTgAeeCgJfkLa-iiBoUkpfngqeNpY-F4IMQCuEAC+1QYuZNb0DORsbCxO5pbunj5+ATjBuvXhYFExcSaguakmIIEARhVQOF6KM-mFxaWIlTV1YZaN0sSnbNSUpJ0uPWUg3hDo2ij6oV0R0bHxphtzGRjZRRuZKzISLZarHIgzZibZlPZA6GwAqwkrwwJVWojI5SGzEWiMcjUK6md63IQLLAJLpvFxjT6TRJI9KZQEgYF5ObglZrEC-FH4OG7DEQRGcgVFNHCzEHd7HPGMfFsSjXcLkhAgDToNAQQI07EfCbfabMjX-LKhDmgjXcyHrJESoXlEVi2aOqXOmUHWDeLAAdzwADNeFAdYEpiBzdlPaL5ksedKaopgpF+FhtHgtNpDGVQFmc4hiIpYBAIIIEOQDsHQ7q8AA3TBVCO23MgHA6wu1LtAA`,
  "Comparison Example": `N4IgxgpgNlDOIC5RgIYBcVQPYHMD6kMeAjAOzEDMAnABwBMADAwCwOkOLLRR4CWAJonDpMuAtxLlq9Jq3YgANMIzZ8AobDCwUAW0UgATigB2-LDrzadABygROIK7fsJiS2BAiCEDAL5LbLDR4JBBAtElKWjpiZmZ6Ugdwvm8w7AiyKPpY+LpEgPS8NABPaxcQAAteWDRcIz0CoIIsYzQDLDgHAA88FC7qoR0alAN9Hr7qvFU8HSx+FzaAVwglYt7+2CmxWfnEJYhfQ-cKrAB3PAAzXig0CAMQ0CGMUYR9o+VRfEIeTOkYgFZWFQKA5vikhKgVGIwb9osRAQxgfpIZ9wQhHLoALSLG5GTGnAT2JRGUzmSy6ZwOJx2RBuRyebx+RrBJKFWHZABslEorKa6nRyXZMS5FB5zKKpXKVRqdV0+mSYBabQ6DxA4w2g2GoyU6sm0x2CwMy1W6z12zmhuWh38jhO5yuNzuqqeIz2RoO1qAA`,
  "Comparison Example 2": `N4IgxgpgNlDOIC5RgIYBcVQPYHMD6kMeAjAOzEDMAnABwBMADAwCwOkOLLRR4CWAJonDpMuAtxLlq9Jq3YgANMIzZ8AobDCwUAW0UgATigB2-LDrzadABygROIK7fsJiS2BAiCEDAL5LbLDR4JBBAtElKWjpiZmZ6Ugdwvm8w7AiyKPpY+LpEgPS8NABPaxcQAAteWDRcIz0CoIIsYzQDLDgHAA88FC7qoR0alAN9Hr7qvFU8HSx+FzaAVwglYt7+2CmxWfnEJYhfQ-cKrAB3PAAzXig0CAMQ0CGMUYR9o+VRfEIeTOkYgFZWFQKA5vikhKgVGIwb9osRAQxgfpIZ9wQhHLoALSLG5GTGnAT2JRGUzmSy6ZwOJx2RBuRyebx+RrBJKFWHZABslEorKa6nRyXZMS5FB5zKKpXKVRqdV0+mSYBabQ6DxA4w2g2GoyU6sm0x2CwMy1W6z12zmhuWh38jhO5yuNzuqqeIz2RpWIAAXnhjFgymBEBdMB4bbA7c0oIsdMZVd7ff63ValA7bgY8AA3TDLB7vFHTGHkOJUZhUBj-QE0UESfkffMSTJFktlivIkTTGtWTEAK1ONXxhP0JLMFmpLlAo9p7gZiCZaSCqsFhZLJf+NAYxEroWSNcXOWLVFX68r4pKZSE0tqOHq8sKita7U6oV1IRALu1atNm31FsTHrWEy-c1dled1rWOM5LmuVNnS1X8wPAcxrBGaoWlVRUbGQ2AWkiOIaDXBg6DyZhQUQzDsJrdCkIMFDjBw+J8MI0hiKUSiyNo09ynQgAjXhjC8PAL1lBo5xZLc2SkOFcLyX9xSFYgRR5BAgzgD1d0bA81w3OCbVY6isNohsV3XZg6AoOgSIwvTyNSXSaLog9jNM8yWNIqz2MlCFzB4vj+AE6pL2vZkF3EvcV00zd9lkiTOW5EEQONETIj+HIEm0w4gA`,
  "Comparison Example 3": `N4IgxgpgNlDOIC5RgIYBcVQPYHMD6kMeAjAOzEAsArAAwUAcFppAnPYstFHgJYAmicOky4CXEuWp1GzNiAA0QjNnz9BPGAFdYaAE494i3SgB2fLAFs8sFBYAOUCBxA37jxMUWwIEAQhoAvooAZhpoELp4AG6YmhDwSCBoAJ52Tgh6cYoAypoARgAWIngAsiiwsAAqqRDWGLoJoDjhHgFBLgVYAO54oVDhDc65hcVlFdVpdSiDGbpx7Q5YaI0gi2gSlLQMpBQAbDRUzmu8fqvY62Sb0jv7h4rHKWmCBQZouMYWCmdLBFgmelg4M4AB54FDAgyCYZFbClcpVGpTQaKUHggx4FR4CxYPjpTIQRTJMEQ2AY0TY3GIfFtNqKVDKUSEbiXKQUFg0Fg3ZxMk6CekifA8llbdmcvZffmYtQIJImHBfYxmSzWWwOdKgVxqjxeHx+QL3c4rY7CugsADMuzNdCO515MuNkhFFqtFC+Dxqz1e71sbttYD+AKBiVRJKh+RhWDh40ROmmhhAIfRmIpeLmEBpXk6PT6AxW0NG8ImtVjM2pITCEWisXiQ3DBejkxLK2a6WI7Ueqfm7UljPEJrZVAoZoATMPueJpUoBWIiP2WIOR2O6cIpacICgcO4jKZzFZNe5EvvWzrfIh9d9ljafnPdixh1ar+tJw6rmzb-eaL6fh3PTpvZ8DR+f1-l0QEVkTBIQHzWExgRSZsnqL4ILJfAUypNMMw6bpegrGZQGgyNYKLPAEOmdD5nLfpKxiKA4jzOsYMLRFSN0ZwW1aTD-XsaYDD+FYuLsHjYD+DYpFoYd53odhEgEoSRMnWT9GEkxRNocTJPYOlLEEpSRJ-GUuLyHgTF8PAXj-HAPi-S9EhfVl6BuA5yIJC9VNNZ1rVmLJXJvO8Hy89MaSAA`
};
