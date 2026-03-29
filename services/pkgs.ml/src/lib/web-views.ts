import { getConfig } from "./config.ts";
import type {
  CategoriesIndexDocument,
  OwnerPackagesDocument,
  PackageOverviewDocument,
  PackageRelationsDocument,
  PopularPackagesDocument,
  RecentPackagesDocument,
} from "./types.ts";

function viewUrl(path: string): string {
  const config = getConfig();
  return `${config.cdnBaseUrl}/${config.viewsBasePath}/${path}`;
}

async function fetchJsonOrNull<T>(url: string): Promise<T | null> {
  const response = await fetch(url, {
    headers: {
      accept: "application/json",
    },
  });

  if (response.status === 404) {
    return null;
  }

  if (!response.ok) {
    throw new Error(`View request failed for ${url}: ${response.status}`);
  }

  return (await response.json()) as T;
}

export async function fetchPackageOverview(packageName: string): Promise<PackageOverviewDocument | null> {
  return await fetchJsonOrNull<PackageOverviewDocument>(
    viewUrl(`packages/${encodeURIComponent(packageName)}/overview.json`),
  );
}

export async function fetchPackageRelations(packageName: string): Promise<PackageRelationsDocument | null> {
  return await fetchJsonOrNull<PackageRelationsDocument>(
    viewUrl(`packages/${encodeURIComponent(packageName)}/relations.json`),
  );
}

export async function fetchRecentPackages(): Promise<RecentPackagesDocument | null> {
  return await fetchJsonOrNull<RecentPackagesDocument>(viewUrl("recent/packages.json"));
}

export async function fetchPopularPackages(): Promise<PopularPackagesDocument | null> {
  return await fetchJsonOrNull<PopularPackagesDocument>(viewUrl("popular/packages.json"));
}

export async function fetchCategoriesIndex(): Promise<CategoriesIndexDocument | null> {
  return await fetchJsonOrNull<CategoriesIndexDocument>(viewUrl("categories/index.json"));
}

export async function fetchOwnerPackages(ownerGithubLogin: string): Promise<OwnerPackagesDocument | null> {
  return await fetchJsonOrNull<OwnerPackagesDocument>(
    viewUrl(`owners/${encodeURIComponent(ownerGithubLogin.toLowerCase())}/packages.json`),
  );
}
