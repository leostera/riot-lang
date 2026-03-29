export interface Env {
  ML_PKGS_CDN: R2Bucket;
  SEARCH_DB: D1Database;
  PACKAGE_PUBLISHED_QUEUE: Queue<PackagePublishedEvent>;
  PACKAGE_INDEXED_QUEUE: Queue<PackageIndexedEvent>;
  PUBLICATION_COORDINATOR: DurableObjectNamespace;
  CDN_BASE_URL?: string;
  INDEX_BASE_PATH?: string;
  GITHUB_TOKEN?: string;
  GITHUB_API_BASE_URL?: string;
  GITHUB_OAUTH_CLIENT_ID?: string;
  GITHUB_OAUTH_CLIENT_SECRET?: string;
  ROOT_AUTH_TOKEN?: string;
  AUTH_COOKIE_DOMAIN?: string;
  PKGS_WEB_BASE_URL?: string;
}

export interface PackageLocator {
  raw: string;
  normalized: string;
  provider: string;
  owner: string;
  repo: string;
  subpath: string | null;
}

export interface RegistryConfig {
  cdnBaseUrl: string;
  indexBasePath: string;
  authCookieDomain: string;
  pkgsWebBaseUrl: string;
}

export type IndexConfig = RegistryConfig;

export interface PackagePublicationManifest {
  package_locator: string;
  source_url: string;
  package_subdir: string;
  selector: string;
  resolved_sha: string;
  package_name: string;
  package_version: string;
  package_public: boolean;
  package_description?: string;
  package_license?: string;
  package_homepage?: string;
  package_repository?: string;
  package_root_module?: string;
  dependencies: Array<Record<string, unknown>>;
  source_archive_key: string;
  manifest_key: string;
  materialized_at: string;
}

export interface PackageClaimRecord {
  package_name: string;
  package_locator: string;
  source_url: string;
  package_subdir: string;
  owner_user_id?: string;
  owner_github_login?: string;
  claimed_at: string;
  updated_at: string;
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

export interface UserLoginRecord {
  github_login: string;
  user_id: string;
  updated_at: string;
}

export interface OAuthStateRecord {
  state_id: string;
  return_to: string;
  created_at: string;
}

export interface SessionRecord {
  session_id: string;
  user_id: string;
  github_login: string;
  created_at: string;
  expires_at: string;
}

export type ApiTokenCapability = "publish";

export interface ApiTokenRecord {
  token_id: string;
  user_id: string;
  github_login: string;
  name: string;
  secret_hash: string;
  capabilities: ApiTokenCapability[];
  created_at: string;
  last_used_at?: string;
  revoked_at?: string;
}

export interface ApiTokenLookupRecord {
  token_id: string;
  user_id: string;
  github_login: string;
  capabilities: ApiTokenCapability[];
  revoked_at?: string;
}

export interface SessionResponse {
  authenticated: boolean;
  user?: UserRecord;
}

export interface AuthenticatedActorRoot {
  kind: "root";
}

export interface AuthenticatedActorUser {
  kind: "user";
  userId: string;
  githubLogin: string;
  tokenId?: string;
}

export type AuthenticatedActor = AuthenticatedActorRoot | AuthenticatedActorUser;

export interface PublishedReleaseRecord {
  package_name: string;
  package_version: string;
  package_locator: string;
  source_url: string;
  package_subdir: string;
  selector: string;
  resolved_sha: string;
  package_description?: string;
  package_license?: string;
  package_homepage?: string;
  package_repository?: string;
  package_root_module?: string;
  dependencies: Array<Record<string, unknown>>;
  source_archive_key: string;
  manifest_key: string;
  published_at: string;
}

export interface PackagePublishedEvent extends PublishedReleaseRecord {
  type: "package.published";
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

export interface IndexConfigDocument {
  schema_version: 1;
  kind: "sparse";
  package_path_strategy: "cargo-lowercase-v1";
  index_base_url: string;
  artifact_base_url: string;
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

export interface SelectorResolutionRecord {
  package_locator: string;
  selector: string;
  resolved_sha: string;
  frozen: boolean;
  recorded_at: string;
}

export interface RequestLogEntry {
  request_id: string;
  request_timestamp: string;
  method: string;
  path: string;
  route: string;
  package_locator?: string;
  selector?: string;
  resolved_sha?: string;
  status: number;
  success: boolean;
  error_category?: string;
  error_message?: string;
  user_agent?: string | null;
}

export interface ResolvedPublication {
  selector: string;
  resolvedSha: string;
  sourceKey: string;
  manifestKey: string;
  manifestCreated: boolean;
  sourceCreated: boolean;
}

export interface PublishedPackageRelease extends ResolvedPublication {
  packageName: string;
  packageVersion: string;
  claimKey: string;
  releaseKey: string;
  claimCreated: boolean;
  releaseCreated: boolean;
  indexChanged: boolean;
}
