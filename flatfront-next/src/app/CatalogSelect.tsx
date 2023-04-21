"use client";

import { Fragment, useState } from "react";
import { Listbox, Transition } from "@headlessui/react";
import { CheckIcon, ChevronUpDownIcon } from "@heroicons/react/20/solid";

const people = [
  { name: "Wade Cooper" },
  { name: "Arlene Mccoy" },
  { name: "Devon Webb" },
  { name: "Tom Cook" },
  { name: "Tanya Fox" },
  { name: "Hellen Schmidt" },
];

type CatalogMeta = {
  name: string;
  order: string;
  title: string;
  descr: string;
};

type CatalogList = Array<CatalogMeta>;

export default function CatalogSelect({ catalogs }: { catalogs: CatalogList }) {
  const [selected, setSelected] = useState(catalogs[0]);

  return (
    <Listbox value={selected} onChange={setSelected}>
      <div className="relative max-w-5xl">
        <Listbox.Button className="relative w-full cursor-default rounded-lg bg-gray-100 py-2 pl-3 pr-10 text-left shadow-md focus:outline-none focus-visible:border-indigo-500 focus-visible:ring-2 focus-visible:ring-white focus-visible:ring-opacity-75 focus-visible:ring-offset-2 focus-visible:ring-offset-orange-300 sm:text-sm">
          <span className="block">{selected.title}</span>
          <span className="pointer-events-none absolute inset-y-0 right-0 flex items-center pr-2">
            <ChevronUpDownIcon
              className="h-5 w-5 text-gray-400"
              aria-hidden="true"
            />
          </span>
        </Listbox.Button>
        <Listbox.Options className="absolute mt-1 max-h-60 w-full overflow-auto rounded-md bg-white py-1 text-base shadow-lg ring-1 ring-black ring-opacity-5 focus:outline-none sm:text-sm">
          {catalogs.map((catalog: CatalogMeta) => (
            <Listbox.Option
              key={catalog.name}
              value={catalog}
              className={({ active }) =>
                `relative cursor-default select-none py-2 pl-10 pr-4 ${
                  active ? "bg-amber-100 text-amber-900" : "text-gray-900"
                }`
              }
            >
              {catalog.title}
            </Listbox.Option>
          ))}
        </Listbox.Options>
      </div>
    </Listbox>
  );
}
