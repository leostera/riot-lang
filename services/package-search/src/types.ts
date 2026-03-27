export interface Env {
  ML_PKGS_CDN: R2Bucket;
  SEARCH_DB: D1Database;
  CDN_BASE_URL?: string;
  INDEX_BASE_PATH?: string;
}

export interface SearchConfig {
  cdnBaseUrl: string;
  indexBasePath: string;
}

export interface PackageIndexedEvent {
  type: "package.indexed";
  package_name: string;
  package_version: string;
  package_locator: string;
  resolved_sha: string;
  package_index_key: string;
  package_index_url: string;
  latest: string;
  indexed_at: string;
}

export interface IndexedPackageRelease {
  version: string;
  published_at: string;
  canonical_locator: string;
  repo_url: string;
  subdir: string;
  sha: string;
  description?: string;
  license?: string;
  homepage?: string;
  repository?: string;
  root_module?: string;
  manifest_key: string;
  source_key: string;
  dependencies: Array<Record<string, unknown>>;
}

export interface PackageIndexDocument {
  schema_version: 1;
  name: string;
  latest: string;
  updated_at: string;
  releases: IndexedPackageRelease[];
}

export interface SearchPackageRow {
  package_name: string;
  normalized_name: string;
  latest_version: string;
  description: string | null;
  license: string | null;
  homepage: string | null;
  repository: string | null;
  root_module: string | null;
  canonical_locator: string;
  repo_url: string;
  repo_owner: string;
  repo_name: string;
  subdir: string;
  release_count: number;
  updated_at: string;
}

export interface SearchResult extends SearchPackageRow {}
