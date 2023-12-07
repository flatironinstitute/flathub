import { type ClassValue, clsx } from "clsx";
import * as d3 from "d3";
import { twMerge } from "tailwind-merge";
import type { CatalogHierarchyNode } from "@/types";

export const FLATHUB_API_BASE_URL = `https://flathub.flatironinstitute.org`;

export function cn(...inputs: ClassValue[]) {
  return twMerge(clsx(inputs));
}

export function log(...args: any[]) {
  console.log(`🌔`, ...args);
}

export const format = {
  concise: (d: number) => {
    if (d < 1e4) return d3.format(`,.4~g`)(d);
    return d3.format(`.2~e`)(d);
  },
  commas: (d: number) => {
    return d3.format(`,`)(d);
  }
};

export function is_root_node(node: CatalogHierarchyNode): boolean {
  return node.depth === 0;
}

export function is_leaf_node(node: CatalogHierarchyNode): boolean {
  return node.height === 0;
}

export async function fetch_api_get<T>(path: string): Promise<T> {
  const url = new URL(`/api${path}`, FLATHUB_API_BASE_URL);
  log(`💥 Fetching:`, url.toString());
  const response = await fetch(url.toString(), {
    method: `GET`,
    headers: new Headers({
      "Content-Type": `application/json`
    })
  });
  if (!response.ok) {
    throw new Error(`API Fetch Error: ${response.status}`);
  }
  const json: T = await response.json();
  log(`💥 Got Response:`, url.toString(), json);
  return json;
}

export async function fetch_api_post<T, U>(
  path: string,
  body: T,
  options: RequestInit = {}
): Promise<U> {
  const url = new URL(`/api${path}`, FLATHUB_API_BASE_URL);
  log(`💥 Fetching:`, url.toString(), body);
  const response = await fetch(url.toString(), {
    method: `POST`,
    headers: new Headers({
      "Content-Type": `application/json`
    }),
    body: JSON.stringify(body),
    ...options
  });
  if (!response.ok) {
    throw new Error(`API Fetch Error: ${response.status}`);
  }
  const json: U = await response.json();
  log(`💥 Got Response:`, url.toString(), json);
  return json;
}
