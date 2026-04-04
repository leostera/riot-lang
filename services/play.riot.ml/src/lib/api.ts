const API_BASE_URL = "https://api.pkgs.ml";

export interface PackageOverviewDocument {
  package_name: string;
  latest_version: string;
  description?: string;
  repository?: string;
  owner_github_login: string;
}

export interface PackageExampleDocument {
  name: string;
  path: string;
  source_code: string;
}

export interface PackageExamplesDocument {
  package_name: string;
  package_version: string;
  source_key: string;
  examples: PackageExampleDocument[];
}

export async function fetchPackageOverview(packageName: string): Promise<PackageOverviewDocument | null> {
  return await fetchJsonOrNull<PackageOverviewDocument>(
    `${API_BASE_URL}/v1/views/packages/${encodeURIComponent(packageName)}/overview`,
  );
}

export async function fetchPackageExamples(
  packageName: string,
  options?: { version?: string },
): Promise<PackageExamplesDocument | null> {
  const params = new URLSearchParams();
  if (options?.version !== undefined) {
    params.set("version", options.version);
  }

  const url = `${API_BASE_URL}/v1/views/packages/${encodeURIComponent(packageName)}/examples${
    params.size > 0 ? `?${params.toString()}` : ""
  }`;

  return await fetchJsonOrNull<PackageExamplesDocument>(url);
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
    throw new Error(`Request failed for ${url}: ${response.status}`);
  }

  return (await response.json()) as T;
}
