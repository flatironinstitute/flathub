const people = [
  {
    name: 'Calvin Hawkins',
    city: 'calvin.hawkins@example.com',
    logo: 'https://images.unsplash.com/photo-1491528323818-fdd1faba62cc?ixlib=rb-1.2.1&ixid=eyJhcHBfaWQiOjEyMDd9&auto=format&fit=facearea&facepad=2&w=256&h=256&q=80',
  },
  {
    name: 'Kristen Ramos',
    city: 'kristen.ramos@example.com',
    logo: 'https://images.unsplash.com/photo-1550525811-e5869dd03032?ixlib=rb-1.2.1&ixid=eyJhcHBfaWQiOjEyMDd9&auto=format&fit=facearea&facepad=2&w=256&h=256&q=80',
  },
  {
    name: 'Ted Fox',
    city: 'ted.fox@example.com',
    logo: 'https://images.unsplash.com/photo-1500648767791-00dcc994a43e?ixlib=rb-1.2.1&ixid=eyJhcHBfaWQiOjEyMDd9&auto=format&fit=facearea&facepad=2&w=256&h=256&q=80',
  },
];

function CatalogItem({ catalog }) {
  return (
    <li className="py-4 flex">
      <img className="h-10 w-10 rounded-full" src={catalog.logo} alt="" />
      <div className="ml-3">
        <p className="text-sm font-medium text-gray-900">{catalog.name}</p>
        <p className="text-sm text-gray-500">{catalog.city}</p>
      </div>
    </li>
  );
}

export default function Catalogs({ catalogs }) {
  return (
    <ul className="divide-y divide-gray-200">
      {catalogs.map((catalog) => (
        <CatalogItem key={catalog.id} team={catalog} />
      ))}
    </ul>
  );
}
