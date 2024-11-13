# Flatfront: A New FlatHUB Frontend

Flatfront is a new frontend for FlatHUB, the Flathub client. It is built with [Astro](https://astro.build), static site generator that generates static sites using modern web technologies — in this case, React.

# Technologies Used:

- [Astro](https://astro.build)
- [React](https://reactjs.org)
- [Tailwind CSS](https://tailwindcss.com)
- [shadcn/ui](https://ui.shadcn.com/): A UI component library
- [Observable Plot](https://observablehq.com/plot/): A plotting library
- [TanStack Query](https://tanstack.com/query/latest): A data fetching library
- [TanStack Table](https://tanstack.com/table/latest): A React table component library
- [D3](https://d3js.org/): A data visualization library
- [Highcharts](https://www.highcharts.com/): Another data visualization library

# Development

Within the `flatfront` directory:

```bash
npm install
npm run dev
```

# Details

The app is built with Astro, and the entry point is a single Astro file: `src/pages/index.astro`. This file is the only page of the app, and it imports a single `App` component from the `src/components` directory. Flatfront app is a "single-page" app, meaning it does not use any HTTP routing. Everything is rendered on the client side.

## Contexts

The `App` component is wrapped in several "context providers." Context providers are a way to share state between components without having to pass props down through the component tree. There are several different Context Providers:

- `QueryClientProvider` is part of the [TanStack Query](https://tanstack.com/query/latest/docs/framework/react/overview) library. This is used for all data fetching. It handles a number of useful query-related features, like caching, pending states, and query cancellation.
- `AppStateProvider` is the primary holder of "App State." In Flatfront, "App State" does _not_ include the data fetched from the server. App State basically encapsulates all of the actions the user has taken, but without including actual data. Importantly, the entire app state is converted to a JSON string, compressed, and added to the URL whenever app state changes. This allows the user to bookmark or share the URL and have the app return to the same state when the URL is visited.
- `PlotDataProvider` is a mechanism for storing data used for plotting globally. This is an important aspect of the `Comparisons` component, which combines data from multiple sources into a single plot.
- `AllCatalogMetadataQueriesProvider` is a mechanism for storing the catalog metadata queries globally. This is used to populate the "Catalogs" dropdown in the `Comparisons` component.

## Cells

A major feature of `Flatfront` as compared to the existing FlatHUB UI is the ability to view data from multiple Catalogs on the same page. There are examples on the main page that illustrate this, for instance the example called `Comparison: Subhalo Mass` shows data from `Illustris`, `IllustrisTNG`, and `EAGLE` all on the same page. Each Catalog that the user loads is represented by a "Cell." The logic for rendering these is contained in [`Cells.tsx`](flatfront/src/components/Cells.tsx) and [`CatalogCell.tsx`](flatfront/src/components/CatalogCell.tsx). Note: An earlier version of Flatfront used several different types of Cells, but this was simplified to a single type of Cell, the `CatalogCell`.

## Catalog Cells

The [`CatalogCell.tsx`](flatfront/src/components/CatalogCell.tsx) component is where "most of the action is" in Flatfront.
