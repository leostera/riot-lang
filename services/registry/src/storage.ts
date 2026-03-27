import type {
  PackageClaimRecord,
  PackageLocator,
  PublishedReleaseRecord,
  RegistryConfig,
  RequestLogEntry,
  SelectorResolutionRecord,
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

function encodeSelector(selector: string): string {
  return encodeURIComponent(selector);
}

function encodePathSegment(value: string): string {
  return encodeURIComponent(value);
}
