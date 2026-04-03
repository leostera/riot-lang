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
  artifact_sha256: string;
  description?: string;
  license?: string;
  homepage?: string;
  repository?: string;
  root_module?: string;
  categories?: string[];
  keywords?: string[];
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
  github_email?: string;
  github_email_verified?: boolean;
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

export interface PackageOverviewDocument {
  schema_version: 1;
  package_name: string;
  latest_version: string;
  updated_at: string;
  published_at: string;
  description?: string;
  license?: string;
  homepage?: string;
  repository?: string;
  root_module?: string;
  canonical_locator: string;
  repo_url: string;
  subdir: string;
  source_key: string;
  manifest_key: string;
  artifact_sha256: string;
  owner_github_login: string;
  owner_github_avatar_url?: string;
  release_count: number;
  dependency_count: number;
  dependent_count: number;
  categories: string[];
  keywords: string[];
}

export interface PackageRelationDependency {
  package_name: string;
  requirement: string;
}

export interface PackageRelationDependent {
  package_name: string;
  latest_version: string;
  requirement: string;
}

export interface PackageRelationsDocument {
  schema_version: 1;
  package_name: string;
  updated_at: string;
  dependencies: PackageRelationDependency[];
  dependents: PackageRelationDependent[];
}

export interface WebPackageListItem {
  package_name: string;
  latest_version: string;
  description?: string;
  license?: string;
  owner_github_login: string;
  owner_github_avatar_url?: string;
  categories: string[];
  updated_at: string;
  repo_url: string;
  repository?: string;
  subdir: string;
  release_count: number;
  package_path: string;
}

export interface RecentPackagesDocument {
  schema_version: 1;
  generated_at: string;
  packages: WebPackageListItem[];
}

export interface PopularPackageListItem extends WebPackageListItem {
  dependent_count: number;
  release_count: number;
}

export interface PopularPackagesDocument {
  schema_version: 1;
  generated_at: string;
  packages: PopularPackageListItem[];
}

export interface CategorySummary {
  name: string;
  slug: string;
  package_count: number;
  packages: string[];
}

export interface CategoriesIndexDocument {
  schema_version: 1;
  generated_at: string;
  categories: CategorySummary[];
}

export interface OwnerPackagesDocument {
  schema_version: 1;
  generated_at: string;
  owner_github_login: string;
  owner_github_avatar_url?: string;
  package_count: number;
  latest_update_at?: string;
  packages: WebPackageListItem[];
}

export type RegistryEventType =
  | "package.submitted"
  | "package.verified"
  | "package.indexed"
  | "package.searchable"
  | "package.published";

export interface RegistryEventRecord {
  event_id: string;
  event_type: RegistryEventType;
  package_name?: string;
  package_version?: string;
  package_locator?: string;
  payload: Record<string, unknown>;
  created_at: string;
}

export interface RegistryEventsDocument {
  limit?: number;
  after?: string;
  events: RegistryEventRecord[];
}
