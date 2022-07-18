import * as React from 'react';
import { CatalogsProps, Catalog } from '../types/Catalog';
import logo from '../assets/cca-logo.jpg';

function CatalogItem({ descr, name, order, title }: Catalog) {
  return (
    <li className="py-4 flex">
      <img className="h-10 w-10 rounded-full" src={logo} alt="order" />
      <div className="ml-3">
        <p className="text-sm font-medium text-gray-900">{name}</p>
        <p className="text-sm text-gray-500">{title}</p>
      </div>
    </li>
  );
}

export default function Catalogs({ catalogs }: CatalogsProps) {
  return (
    <ul className="divide-y divide-gray-200">
      {catalogs.map((catalog: Catalog) => (
        <CatalogItem
          descr={catalog.descr}
          name={catalog.name}
          order={catalog.order}
          title={catalog.title}
          key={catalog.name}
        />
      ))}
    </ul>
  );
}
