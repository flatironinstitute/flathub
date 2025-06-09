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

A major feature of `Flatfront` as compared to the existing FlatHUB UI is the ability to view data from multiple Catalogs on the same page. There are examples on the main page that illustrate this, for instance the example called `Comparison: Subhalo Mass` shows data from `Illustris`, `IllustrisTNG`, and `EAGLE` all on the same page. Each Catalog that the user loads is represented by a "Cell." The logic for rendering these is contained in [`Cells.tsx`](src/components/Cells.tsx) and [`CatalogCell.tsx`](src/components/CatalogCell.tsx). Note: An earlier version of Flatfront used several different types of Cells, but this was simplified to a single type of Cell, the `CatalogCell`.

## Catalog Cells

The [`CatalogCell.tsx`](src/components/CatalogCell.tsx) component is where "most of the action is" in Flatfront. The `CatalogCell` component has a lot of context providers of its own, separate from the "global" context providers that wrap the entire App. These providers are responsible for fetching data from the server, as well as parsing and filtering this data. The providers allow for storing the data in a way that is accessible to the rest of the `CatalogCell` component.

Each Catalog Cell has the following sections:

- `Fields`: ([FieldsBrowser.tsx](src/components/FieldsBrowser.tsx)) FlatHUB datasets can have hundereds of variables, and the fields browser lets you select which fields to display. Selected fields will be shown in the various filter drop-down menus, as well as in the `Table` section.

- `Filters`: ([FilterSection.tsx](src/components/FilterSection.tsx)) These are the various filter drop-down menus and inputs that allow you to filter the data. The filters are generated based on the fields that are selected in the `Fields` section.

- `Random`: ([RandomSampleControls.tsx](src/components/RandomSampleControls.tsx)) This allows you to select a random subset of the data, as well as set a random seed.

- `Plots`: ([PlotSection.tsx](src/components/PlotSection.tsx)) This is where you can add plots of various types. Another major feature of Flatfront is the ability to add multiple plots to the same page.

- `Table`: ([TableSection.tsx](src/components/TableSection.tsx)) This is where the data is displayed in tabular form. The table is paginated and sortable. It uses the `TanStack Table` library.

- `Python`: ([PythonSection.tsx](src/components/PythonSection.tsx)) This shows the Python code that can be used to query the current data (with filters) using the `flathub` Python client.

## Comparisons

The `Comparisons` section ([Comparisons.tsx](src/components/Comparisons.tsx)) is a special section that appears below the Catalog Cells whenever there is more than one catalog on the page. Comparisons allow you to create plots using data from multiple plots on the same page.