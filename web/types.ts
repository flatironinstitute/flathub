"use strict";

export function assert(x: null|undefined): never;
export function assert(x: object): void;
export function assert(x: any): void {
  if (x == null)
    throw new Error("assertion failure");
}

export type Dict<T> = { [name: string]: T };

export type Field = {
  name: string,
  type: string,
  title: string,
  descr?: null|string,
  units?: null|string,
  top?: boolean,
  disp: boolean,
  base: 'f'|'i'|'b'|'s',
  terms?: boolean,
  enum?: null|string[],
  dict?: null|string
};

export type Catalog = {
  name: string,
  title: string,
  descr: null|string,
  uri: string,
  bulk: string[],
  fields: Field[],
  count?: number,
};

