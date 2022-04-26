"use strict";

export type Dict<T> = { [name: string]: T };

/* a single field within a category. fields are nested into hiearchical groups */
export type Field = {
  key: string; /* local name of field within this group */
  name: string; /* global unique ("variable") name of field within the catalog */
  title: string; /* display name of the field within the group */
  descr?: null | string; /* description of field within the group */
  type: 'keyword'|'long'|'integer'|'short'|'byte'|'double'|'float'|'half_float'|'boolean'|'void'; /* raw storage type */
  base: "f" | "i" | "b" | "s" | "v"; /* base storage type (floating, integral, boolean, string, void) */
  enum?: null | string[]; /* if non-null, display values as these keywords instead (integral or boolean: enum[<int>value]) */
  disp: boolean; /* include field in data display by default */
  units?: null | string; /* display units */
  required?: null | boolean; /* true = required filter; false = top-level (default) optional filter; null = normal */
  terms?: boolean; /* display dynamically as a dropdown of values */
  dict?: null | string; /* unique key index to global field dictionary (for compare) */
  scale?: number; /* scale factor to dict-comparable units, display  value*scale (for compare) */
  reversed?: boolean; /* display axes and ranges in reverse (high-low) */
  attachment?: boolean; /* this is a meta field for a downloadable attachment (type boolean, indicating presence) */
  wildcard?: boolean; /* allow wildcard prefix searching on keyword field ("xy*") */
  sub?: Field[]; /* sub-fields */
};

export type Catalog = {
  name: string; /* globally unique catalog key used in urls */
  order: string; /* sort key for display order */
  title: string; /* display name for catalog */
  synopsis: string; /* short description */
  descr: null | string; /* long description (html) */
  fields: Field[]; /* field groups */
  fieldMap?: Dict<Field>; /* expanded (ungrouped) fields keyed by name (populated by populateCatalogFieldMap on client) */
  count?: number; /* total number of rows (if known) */
  sort?: string[]; /* default sort fields */
};

function populateCatalogFieldMap(cat: Catalog): void {
  const defaults = {units: null}; /* fields to propagate down bc may be specified on parent not child*/
  function populateFieldMap(map: Dict<Field>, fields: Field[], parent: Field): void {
    for (let f of fields) {
      map[f.name] = f;
      for (let k in defaults)
        f[k] = f[k] || parent[k];
      if (f.sub)
        populateFieldMap(map, f.sub, f);
    }
  }
  populateFieldMap(cat.fieldMap = {}, cat.fields, <Field>defaults);
}

export type AggrStats = {
  min: number;
  max: number;
  avg: number;
};
type Bucket<T> = {
  key: T;
  from?: T;
  to?: T;
  doc_count: number; /* number of items in this bucket */
  hist?: AggrTerms<any>; /* any sub-aggregation (histogram), e.g. y-axis data */
  pct?: { values: Dict<number> }; /* percentiles */
};
export type AggrTerms<T> = {
  buckets: Array<Bucket<T>>;
}; //AggrTerms = plot data or hist data.


export type Aggr = AggrStats | AggrTerms<string | number>;  

/* server responses to queries */
export type CatalogResponse = {
  hits: { // hits limited to paginated chunk
    total: number; /* total matching items */
    hits: Dict<any>[]; /* individual matches */
  };
  aggregations?: Dict<Aggr>; /* requested aggregate/histogram data keyed by field name */
  histsize: Dict<number>; /* imputed histogram bucket size */
};
