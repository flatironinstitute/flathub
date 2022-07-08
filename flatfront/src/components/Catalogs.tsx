import * as React from 'react';
import { CatalogsProps, Catalog } from '../types/Catalog';

type CatalogProps = {
  name: string;
  city: string;
  logo: string;
};

function CatalogItem({ name, city, logo }: CatalogProps) {
  return (
    <li className="py-4 flex">
      <img className="h-10 w-10 rounded-full" src={logo} alt="" />
      <div className="ml-3">
        <p className="text-sm font-medium text-gray-900">{name}</p>
        <p className="text-sm text-gray-500">{city}</p>
      </div>
    </li>
  );
}

export default function Catalogs({ catalogs }: CatalogsProps) {
  return (
    <ul className="divide-y divide-gray-200">
      {catalogs.map((catalog: Catalog) => (
        <CatalogItem
          key={catalog.name}
          name={catalog.name}
          city={catalog.city}
          logo={catalog.logo}
        />
      ))}
    </ul>
  );
}
