export interface Env {
  ML_PKGS_CDN: R2Bucket;
  PACKAGE_PUBLISHED_QUEUE: Queue<PackagePublishedEvent>;
  PUBLICATION_COORDINATOR: DurableObjectNamespace;
  CDN_BASE_URL?: string;
  GITHUB_TOKEN?: string;
  GITHUB_API_BASE_URL?: string;
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
}

export interface PackagePublicationManifest {
  package_locator: string;
  source_url: string;
  package_subdir: string;
  selector: string;
  resolved_sha: string;
  package_name: string;
  package_version: string;
  dependencies: Array<Record<string, unknown>>;
  source_archive_key: string;
  manifest_key: string;
  published_at: string;
}

export interface PackagePublishedEvent extends PackagePublicationManifest {
  type: "package.published";
}

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
