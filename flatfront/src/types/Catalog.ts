export type Catalog = {
  descr: string;
  name: string;
  order: string;
  title: string;
};

export type CatalogsProps = {
  catalogs: Catalog[];
};
