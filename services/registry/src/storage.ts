import type {
  ApiTokenLookupRecord,
  ApiTokenRecord,
  IndexConfig,
  IndexConfigDocument,
  CategoriesIndexDocument,
  OAuthStateRecord,
  OwnerPackagesDocument,
  PackageIndexDocument,
  PackageOverviewDocument,
  PackageClaimRecord,
  PackageLocator,
  PackageRelationsDocument,
  PackagePublicationManifest,
  PopularPackagesDocument,
  PublishedReleaseRecord,
  RecentPackagesDocument,
  RegistryConfig,
  RequestLogEntry,
  SelectorResolutionRecord,
  SessionRecord,
  UserLoginRecord,
  UserRecord,
} from "./types.ts";

export function sourceArchiveKey(locator: PackageLocator, sha: string): string {
  return `sources/${locator.provider}/${locator.owner}/${locator.repo}/${sha}.tar.gz`;
}

export function manifestKey(locator: PackageLocator, sha: string): string {
  return `packages/${locator.normalized}/${sha}.manifest.json`;
}

export function requestLogKey(entry: RequestLogEntry): string {
  const timestamp = new Date(entry.request_timestamp);
  const year = timestamp.getUTCFullYear().toString().padStart(4, "0");
  const month = (timestamp.getUTCMonth() + 1).toString().padStart(2, "0");
  const day = timestamp.getUTCDate().toString().padStart(2, "0");
  const hour = timestamp.getUTCHours().toString().padStart(2, "0");
  return `requests/${year}/${month}/${day}/${hour}/${entry.request_id}.json`;
}

export function selectorResolutionKey(locator: PackageLocator, selector: string): string {
  return `selectors/${locator.normalized}/${encodeSelector(selector)}.json`;
}

export function packageClaimKey(packageName: string): string {
  return `claims/${encodePathSegment(packageName)}.json`;
}

export function publishedReleaseKey(packageName: string, version: string): string {
  return `releases/${encodePathSegment(packageName)}/${encodePathSegment(version)}.json`;
}

export function userRecordKey(userId: string): string {
  return `auth/users/by-id/${encodePathSegment(userId)}.json`;
}

export function userLoginKey(githubLogin: string): string {
  return `auth/users/by-login/${encodePathSegment(githubLogin.toLowerCase())}.json`;
}

export function oauthStateKey(stateId: string): string {
  return `auth/oauth-states/${encodePathSegment(stateId)}.json`;
}

export function sessionKey(sessionId: string): string {
  return `auth/sessions/${encodePathSegment(sessionId)}.json`;
}

export function apiTokenKey(userId: string, tokenId: string): string {
  return `auth/tokens/by-user/${encodePathSegment(userId)}/${encodePathSegment(tokenId)}.json`;
}

export function apiTokenLookupKey(tokenHash: string): string {
  return `auth/tokens/by-secret/${encodePathSegment(tokenHash)}.json`;
}

export function userAvatarKey(githubLogin: string): string {
  return `avatars/${encodePathSegment(githubLogin.toLowerCase())}`;
}

export function userAvatarUrl(config: RegistryConfig, githubLogin: string): string {
  return `${config.cdnBaseUrl}/${userAvatarKey(githubLogin)}`;
}

export function manifestRoutePath(locator: PackageLocator, sha: string): string {
  return `/package/${locator.normalized}/-/manifest/${sha}.json`;
}

export function sourceRoutePath(locator: PackageLocator, sha: string): string {
  return `/package/${locator.normalized}/-/source/${sha}.tar.gz`;
}

export function prettyManifestUrl(
  config: RegistryConfig,
  locator: PackageLocator,
  sha: string,
): string {
  return `${config.cdnBaseUrl}/${manifestKey(locator, sha)}`;
}

export function prettySourceUrl(
  config: RegistryConfig,
  locator: PackageLocator,
  sha: string,
): string {
  return `${config.cdnBaseUrl}/${sourceArchiveKey(locator, sha)}`;
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

export function packageOverviewKey(config: RegistryConfig, packageName: string): string {
  return `${config.viewsBasePath}/packages/${encodePathSegment(packageName)}/overview.json`;
}

export function packageRelationsKey(config: RegistryConfig, packageName: string): string {
  return `${config.viewsBasePath}/packages/${encodePathSegment(packageName)}/relations.json`;
}

export function recentPackagesKey(config: RegistryConfig): string {
  return `${config.viewsBasePath}/recent/packages.json`;
}

export function popularPackagesKey(config: RegistryConfig): string {
  return `${config.viewsBasePath}/popular/packages.json`;
}

export function categoriesIndexKey(config: RegistryConfig): string {
  return `${config.viewsBasePath}/categories/index.json`;
}

export function ownerPackagesKey(config: RegistryConfig, ownerGithubLogin: string): string {
  return `${config.viewsBasePath}/owners/${encodePathSegment(ownerGithubLogin.toLowerCase())}/packages.json`;
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

export async function writePackageOverviewDocument(
  bucket: R2Bucket,
  config: RegistryConfig,
  document: PackageOverviewDocument,
): Promise<void> {
  await bucket.put(packageOverviewKey(config, document.package_name), JSON.stringify(document, null, 2), {
    httpMetadata: {
      contentType: "application/json; charset=utf-8",
    },
  });
}

export async function writePackageRelationsDocument(
  bucket: R2Bucket,
  config: RegistryConfig,
  document: PackageRelationsDocument,
): Promise<void> {
  await bucket.put(packageRelationsKey(config, document.package_name), JSON.stringify(document, null, 2), {
    httpMetadata: {
      contentType: "application/json; charset=utf-8",
    },
  });
}

export async function writeRecentPackagesDocument(
  bucket: R2Bucket,
  config: RegistryConfig,
  document: RecentPackagesDocument,
): Promise<void> {
  await bucket.put(recentPackagesKey(config), JSON.stringify(document, null, 2), {
    httpMetadata: {
      contentType: "application/json; charset=utf-8",
    },
  });
}

export async function writePopularPackagesDocument(
  bucket: R2Bucket,
  config: RegistryConfig,
  document: PopularPackagesDocument,
): Promise<void> {
  await bucket.put(popularPackagesKey(config), JSON.stringify(document, null, 2), {
    httpMetadata: {
      contentType: "application/json; charset=utf-8",
    },
  });
}

export async function writeCategoriesIndexDocument(
  bucket: R2Bucket,
  config: RegistryConfig,
  document: CategoriesIndexDocument,
): Promise<void> {
  await bucket.put(categoriesIndexKey(config), JSON.stringify(document, null, 2), {
    httpMetadata: {
      contentType: "application/json; charset=utf-8",
    },
  });
}

export async function writeOwnerPackagesDocument(
  bucket: R2Bucket,
  config: RegistryConfig,
  document: OwnerPackagesDocument,
): Promise<void> {
  await bucket.put(ownerPackagesKey(config, document.owner_github_login), JSON.stringify(document, null, 2), {
    httpMetadata: {
      contentType: "application/json; charset=utf-8",
    },
  });
}

export async function readSelectorResolution(
  bucket: R2Bucket,
  locator: PackageLocator,
  selector: string,
): Promise<SelectorResolutionRecord | null> {
  const object = await bucket.get(selectorResolutionKey(locator, selector));
  if (object === null) {
    return null;
  }

  return (await object.json()) as SelectorResolutionRecord;
}

export async function writeSelectorResolution(
  bucket: R2Bucket,
  locator: PackageLocator,
  record: SelectorResolutionRecord,
): Promise<void> {
  await bucket.put(selectorResolutionKey(locator, record.selector), JSON.stringify(record, null, 2), {
    httpMetadata: {
      contentType: "application/json; charset=utf-8",
    },
  });
}

export async function readPackageClaim(
  bucket: R2Bucket,
  packageName: string,
): Promise<PackageClaimRecord | null> {
  const object = await bucket.get(packageClaimKey(packageName));
  if (object === null) {
    return null;
  }

  return (await object.json()) as PackageClaimRecord;
}

export async function listPackageIndexDocuments(
  bucket: R2Bucket,
  config: RegistryConfig,
): Promise<PackageIndexDocument[]> {
  const listing = await bucket.list({
    prefix: `${config.indexBasePath}/`,
  });

  const keys = listing.objects
    .map((object) => object.key)
    .filter((key) => key.endsWith(".json") && key !== indexConfigKey(config));

  const documents = await Promise.all(
    keys.map(async (key) => {
      const object = await bucket.get(key);
      if (object === null) {
        throw new Error(`Expected package index document ${key} to exist.`);
      }

      return await object.json<PackageIndexDocument>();
    }),
  );

  return documents.sort((left, right) => left.name.localeCompare(right.name));
}

export async function writePackageClaim(
  bucket: R2Bucket,
  record: PackageClaimRecord,
): Promise<void> {
  await bucket.put(packageClaimKey(record.package_name), JSON.stringify(record, null, 2), {
    httpMetadata: {
      contentType: "application/json; charset=utf-8",
    },
  });
}

export async function readPublishedRelease(
  bucket: R2Bucket,
  packageName: string,
  version: string,
): Promise<PublishedReleaseRecord | null> {
  const object = await bucket.get(publishedReleaseKey(packageName, version));
  if (object === null) {
    return null;
  }

  return (await object.json()) as PublishedReleaseRecord;
}

export async function writePublishedRelease(
  bucket: R2Bucket,
  record: PublishedReleaseRecord,
): Promise<void> {
  await bucket.put(
    publishedReleaseKey(record.package_name, record.package_version),
    JSON.stringify(record, null, 2),
    {
      httpMetadata: {
        contentType: "application/json; charset=utf-8",
      },
    },
  );
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

export async function readUserRecord(bucket: R2Bucket, userId: string): Promise<UserRecord | null> {
  const object = await bucket.get(userRecordKey(userId));
  if (object === null) {
    return null;
  }

  return await object.json<UserRecord>();
}

export async function writeUserRecord(bucket: R2Bucket, record: UserRecord): Promise<void> {
  await bucket.put(userRecordKey(record.user_id), JSON.stringify(record, null, 2), {
    httpMetadata: {
      contentType: "application/json; charset=utf-8",
    },
  });
}

export async function readUserLoginRecord(
  bucket: R2Bucket,
  githubLogin: string,
): Promise<UserLoginRecord | null> {
  const object = await bucket.get(userLoginKey(githubLogin));
  if (object === null) {
    return null;
  }

  return await object.json<UserLoginRecord>();
}

export async function writeUserLoginRecord(
  bucket: R2Bucket,
  record: UserLoginRecord,
): Promise<void> {
  await bucket.put(userLoginKey(record.github_login), JSON.stringify(record, null, 2), {
    httpMetadata: {
      contentType: "application/json; charset=utf-8",
    },
  });
}

export async function readOAuthStateRecord(
  bucket: R2Bucket,
  stateId: string,
): Promise<OAuthStateRecord | null> {
  const object = await bucket.get(oauthStateKey(stateId));
  if (object === null) {
    return null;
  }

  return await object.json<OAuthStateRecord>();
}

export async function writeOAuthStateRecord(
  bucket: R2Bucket,
  record: OAuthStateRecord,
): Promise<void> {
  await bucket.put(oauthStateKey(record.state_id), JSON.stringify(record, null, 2), {
    httpMetadata: {
      contentType: "application/json; charset=utf-8",
    },
  });
}

export async function deleteOAuthStateRecord(bucket: R2Bucket, stateId: string): Promise<void> {
  await bucket.delete(oauthStateKey(stateId));
}

export async function readSessionRecord(
  bucket: R2Bucket,
  sessionId: string,
): Promise<SessionRecord | null> {
  const object = await bucket.get(sessionKey(sessionId));
  if (object === null) {
    return null;
  }

  return await object.json<SessionRecord>();
}

export async function writeSessionRecord(bucket: R2Bucket, record: SessionRecord): Promise<void> {
  await bucket.put(sessionKey(record.session_id), JSON.stringify(record, null, 2), {
    httpMetadata: {
      contentType: "application/json; charset=utf-8",
    },
  });
}

export async function deleteSessionRecord(bucket: R2Bucket, sessionId: string): Promise<void> {
  await bucket.delete(sessionKey(sessionId));
}

export async function readApiTokenRecord(
  bucket: R2Bucket,
  userId: string,
  tokenId: string,
): Promise<ApiTokenRecord | null> {
  const object = await bucket.get(apiTokenKey(userId, tokenId));
  if (object === null) {
    return null;
  }

  return await object.json<ApiTokenRecord>();
}

export async function writeApiTokenRecord(
  bucket: R2Bucket,
  record: ApiTokenRecord,
): Promise<void> {
  await bucket.put(apiTokenKey(record.user_id, record.token_id), JSON.stringify(record, null, 2), {
    httpMetadata: {
      contentType: "application/json; charset=utf-8",
    },
  });
}

export async function listApiTokenRecords(
  bucket: R2Bucket,
  userId: string,
): Promise<ApiTokenRecord[]> {
  const listed = await bucket.list({
    prefix: `auth/tokens/by-user/${encodePathSegment(userId)}/`,
  });

  const records = await Promise.all(
    listed.objects.map(async (object) => {
      const token = await bucket.get(object.key);
      return token === null ? null : await token.json<ApiTokenRecord>();
    }),
  );

  return records
    .filter((record): record is ApiTokenRecord => record !== null)
    .sort((left, right) => right.created_at.localeCompare(left.created_at));
}

export async function readApiTokenLookupRecord(
  bucket: R2Bucket,
  tokenHash: string,
): Promise<ApiTokenLookupRecord | null> {
  const object = await bucket.get(apiTokenLookupKey(tokenHash));
  if (object === null) {
    return null;
  }

  return await object.json<ApiTokenLookupRecord>();
}

export async function writeApiTokenLookupRecord(
  bucket: R2Bucket,
  tokenHash: string,
  record: ApiTokenLookupRecord,
): Promise<void> {
  await bucket.put(apiTokenLookupKey(tokenHash), JSON.stringify(record, null, 2), {
    httpMetadata: {
      contentType: "application/json; charset=utf-8",
    },
  });
}

function encodeSelector(selector: string): string {
  return encodeURIComponent(selector);
}

function encodePathSegment(value: string): string {
  return encodeURIComponent(value);
}
