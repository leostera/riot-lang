export interface Env {
  ML_PKGS_CDN: R2Bucket;
  ML_PKGS_BACKUPS?: R2Bucket;
  SEARCH_DB: D1Database;
  PACKAGE_PUBLISHED_QUEUE: Queue<PackagePublishedEvent>;
  PACKAGE_INDEXED_QUEUE: Queue<PackageIndexedEvent>;
  PUBLICATION_COORDINATOR: DurableObjectNamespace;
  REGISTRY_D1_BACKUP?: Workflow<{ accountId: string; databaseId: string; bucketPrefix?: string }>;
  CDN_BASE_URL?: string;
  INDEX_BASE_URL?: string;
  INDEX_BASE_PATH?: string;
  INDEX_ROUTE_PATH?: string;
  VIEWS_BASE_PATH?: string;
  GITHUB_API_BASE_URL?: string;
  GITHUB_OAUTH_CLIENT_ID?: string;
  GITHUB_OAUTH_CLIENT_SECRET?: string;
  ROOT_AUTH_TOKEN?: string;
  AUTH_COOKIE_DOMAIN?: string;
  PKGS_WEB_BASE_URL?: string;
  D1_REST_API_TOKEN?: string;
  D1_BACKUP_ACCOUNT_ID?: string;
  D1_BACKUP_DATABASE_ID?: string;
  D1_BACKUP_BUCKET_PREFIX?: string;
  D1_BACKUP_ENABLED?: string;
}

export interface RegistryConfig {
  cdnBaseUrl: string;
  indexBaseUrl: string;
  indexBasePath: string;
  indexRoutePath: string;
  viewsBasePath: string;
  authCookieDomain: string;
  pkgsWebBaseUrl: string;
}

export type IndexConfig = RegistryConfig;

export interface PackagePublicationManifest {
  package_locator: string;
  source_url: string;
  package_subdir: string;
  artifact_sha256: string;
  package_name: string;
  package_version: string;
  package_public: boolean;
  package_description?: string;
  package_license?: string;
  package_homepage?: string;
  package_repository?: string;
  package_root_module?: string;
  package_categories?: string[];
  package_keywords?: string[];
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
  github_email?: string;
  github_email_verified?: boolean;
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

export interface IndexReadRecord {
  read_id: string;
  document_key: string;
  package_name?: string;
  read_at: string;
}

export interface PackageDownloadRecord {
  download_id: string;
  package_name: string;
  package_version: string;
  artifact_sha256: string;
  source_archive_key: string;
  downloaded_at: string;
}

export interface RegistryEventsDocument {
  limit?: number;
  after?: string;
  events: RegistryEventRecord[];
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
  artifact_sha256: string;
  package_description?: string;
  package_license?: string;
  package_homepage?: string;
  package_repository?: string;
  package_root_module?: string;
  package_categories?: string[];
  package_keywords?: string[];
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
  artifact_sha256: string;
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

export interface PopularPackagesDocument {
  schema_version: 1;
  generated_at: string;
  packages: Array<WebPackageListItem & { dependent_count: number; release_count: number }>;
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

export interface PublishedPackageRelease {
  artifactSha256: string;
  sourceKey: string;
  manifestKey: string;
  manifestCreated: boolean;
  sourceCreated: boolean;
  packageName: string;
  packageVersion: string;
  claimKey: string;
  releaseKey: string;
  claimCreated: boolean;
  releaseCreated: boolean;
  indexChanged: boolean;
}
