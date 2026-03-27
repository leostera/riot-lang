import type {
  IndexedPackageRelease,
  PackageIndexDocument,
  SearchPackageRow,
} from "./types.ts";

export function buildSearchRow(document: PackageIndexDocument): SearchPackageRow {
  const latestRelease = document.releases.find((release) => release.version === document.latest);
  if (latestRelease === undefined) {
    throw new Error(`Package document ${document.name} does not include latest release ${document.latest}.`);
  }

  const parsed = parseCanonicalLocator(latestRelease.canonical_locator);

  return {
    package_name: document.name,
    normalized_name: normalizeName(document.name),
    latest_version: latestRelease.version,
    description: latestRelease.description ?? null,
    license: latestRelease.license ?? null,
    homepage: latestRelease.homepage ?? null,
    repository: latestRelease.repository ?? null,
    root_module: latestRelease.root_module ?? null,
    canonical_locator: latestRelease.canonical_locator,
    repo_url: latestRelease.repo_url,
    repo_owner: parsed.owner,
    repo_name: parsed.repo,
    subdir: latestRelease.subdir,
    release_count: document.releases.length,
    updated_at: document.updated_at,
  };
}

export function rankQueryResult(
  row: SearchPackageRow,
  query: string,
  ftsRank: number | null,
): number {
  const normalizedQuery = normalizeName(query);
  const normalizedName = row.normalized_name;

  if (normalizedName === normalizedQuery) {
    return 10_000;
  }

  if (normalizedName.startsWith(normalizedQuery)) {
    return 5_000;
  }

  if (normalizedName.includes(normalizedQuery)) {
    return 2_500;
  }

  return Math.max(0, 1_000 - Math.round((ftsRank ?? 1_000) * 100));
}

export function buildFtsQuery(query: string): string {
  const tokens = tokenizeQuery(query);
  if (tokens.length === 0) {
    return "";
  }

  return tokens.map((token) => `${escapeFtsToken(token)}*`).join(" AND ");
}

function parseCanonicalLocator(locator: string): {
  owner: string;
  repo: string;
} {
  const parts = locator.split("/");
  if (parts.length < 3) {
    throw new Error(`Invalid canonical locator ${locator}.`);
  }

  return {
    owner: parts[1] ?? "",
    repo: parts[2] ?? "",
  };
}

function tokenizeQuery(query: string): string[] {
  return query
    .toLowerCase()
    .replace(/[^a-z0-9/_-]+/g, " ")
    .split(/\s+/)
    .map((token) => token.trim())
    .filter((token) => token.length > 0);
}

function escapeFtsToken(token: string): string {
  return token.replace(/"/g, "\"\"");
}

function normalizeName(value: string): string {
  return value.toLowerCase().replace(/[\s_-]+/g, "");
}
