import CatalogSelect from "./CatalogSelect";

export default async function Home() {
  const catalogs_response = await fetch(
    `https://flathub.flatironinstitute.org/api`
  );

  const catalogs = await catalogs_response.json();

  return (
    <main>
      <CatalogSelect catalogs={catalogs} />
      <pre>{JSON.stringify(catalogs, null, 2)}</pre>
    </main>
  );
}
