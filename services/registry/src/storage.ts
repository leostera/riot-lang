import type {
  IndexConfig,
  IndexConfigDocument,
  PackageIndexDocument,
  PackagePublicationManifest,
  RegistryConfig,
} from "./types.ts";

export function artifactSourceArchiveKey(
  packageName: string,
  packageVersion: string,
  artifactDigest: string,
): string {
  return `sources/${encodePathSegment(packageName)}/${encodePathSegment(packageVersion)}/${artifactDigest}.tar.gz`;
}

export function artifactManifestKey(
  packageName: string,
  packageVersion: string,
  artifactDigest: string,
): string {
  return `packages/${encodePathSegment(packageName)}/${encodePathSegment(packageVersion)}/${artifactDigest}.manifest.json`;
}

export function packageClaimKey(packageName: string): string {
  return `claims/${encodePathSegment(packageName)}.json`;
}

export function publishedReleaseKey(packageName: string, version: string): string {
  return `releases/${encodePathSegment(packageName)}/${encodePathSegment(version)}.json`;
}

export function userAvatarKey(githubLogin: string): string {
  return `avatars/${encodePathSegment(githubLogin.toLowerCase())}`;
}

export function userAvatarUrl(config: RegistryConfig, githubLogin: string): string {
  return `${config.cdnBaseUrl}/${userAvatarKey(githubLogin)}`;
}

export function cdnObjectUrl(config: RegistryConfig, key: string): string {
  return `${config.cdnBaseUrl}/${key}`;
}

export function indexConfigKey(config: IndexConfig): string {
  return `${config.indexBasePath}/config.json`;
}

export function packageIndexKey(config: IndexConfig, packageName: string): string {
  const normalized = packageName.toLowerCase();

  if (normalized.length === 1) {
    return `${config.indexBasePath}/1/${normalized}.json`;
  }

  if (normalized.length === 2) {
    return `${config.indexBasePath}/2/${normalized}.json`;
  }

  if (normalized.length === 3) {
    return `${config.indexBasePath}/3/${normalized[0]}/${normalized}.json`;
  }

  return `${config.indexBasePath}/${normalized.slice(0, 2)}/${normalized.slice(2, 4)}/${normalized}.json`;
}

export function packageIndexUrl(config: IndexConfig, packageName: string): string {
  return `${config.cdnBaseUrl}/${packageIndexKey(config, packageName)}`;
}

export function buildIndexConfigDocument(config: IndexConfig): IndexConfigDocument {
  return {
    schema_version: 1,
    kind: "sparse",
    package_path_strategy: "cargo-lowercase-v1",
    index_base_url: `${config.cdnBaseUrl}/${config.indexBasePath}`,
    artifact_base_url: config.cdnBaseUrl,
  };
}

export async function writeIndexConfig(bucket: R2Bucket, config: IndexConfig): Promise<void> {
  await bucket.put(indexConfigKey(config), JSON.stringify(buildIndexConfigDocument(config), null, 2), {
    httpMetadata: {
      contentType: "application/json; charset=utf-8",
    },
  });
}

export async function readPackageIndexDocument(
  bucket: R2Bucket,
  config: IndexConfig,
  packageName: string,
): Promise<PackageIndexDocument | null> {
  const object = await bucket.get(packageIndexKey(config, packageName));
  if (object === null) {
    return null;
  }

  return await object.json<PackageIndexDocument>();
}

export async function writePackageIndexDocument(
  bucket: R2Bucket,
  config: IndexConfig,
  document: PackageIndexDocument,
): Promise<void> {
  await bucket.put(packageIndexKey(config, document.name), JSON.stringify(document, null, 2), {
    httpMetadata: {
      contentType: "application/json; charset=utf-8",
    },
  });
}

export async function readPublicationManifest(
  bucket: R2Bucket,
  key: string,
): Promise<PackagePublicationManifest | null> {
  const object = await bucket.get(key);
  if (object === null) {
    return null;
  }

  return await object.json<PackagePublicationManifest>();
}

function encodePathSegment(value: string): string {
  return encodeURIComponent(value);
}
