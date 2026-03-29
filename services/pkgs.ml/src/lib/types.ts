export interface SearchResult {
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

export interface SearchResponse {
  query: string;
  count: number;
  results: SearchResult[];
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

export interface PackageFact {
  label: string;
  value: string;
  href?: string;
  code?: boolean;
}

export interface UserRecord {
  user_id: string;
  github_id: number;
  github_login: string;
  github_name?: string;
  github_avatar_url?: string;
  created_at: string;
  updated_at: string;
}

export interface SessionResponse {
  authenticated: boolean;
  user?: UserRecord;
}

export interface ApiTokenSummary {
  token_id: string;
  user_id: string;
  github_login: string;
  name: string;
  capabilities: string[];
  created_at: string;
  last_used_at?: string;
  revoked_at?: string;
}

export interface ApiTokensResponse {
  user: UserRecord;
  tokens: ApiTokenSummary[];
}

export interface CreateApiTokenResponse {
  plaintext_token: string;
  token: ApiTokenSummary;
}
