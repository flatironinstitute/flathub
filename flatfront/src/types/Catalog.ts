export interface ResponseCatalog {
  name: 'string';
  title: 'string';
  synopsis: 'string';
  order: 'string';
}

export type Catalog = {
  name: string;
  city: string;
  logo: string;
};

export type CatalogsProps = {
  catalogs: Catalog[];
};
