import type { PackageLocator, RegistryConfig, RequestLogEntry } from "./types.ts";
import { publicLocatorPath } from "./locator.ts";

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
  return `requests/${year}/${month}/${day}/${entry.request_id}.json`;
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
  return `${config.cdnBaseUrl}/packages/${publicLocatorPath(locator)}/-/${sha}.manifest.json`;
}

export function prettySourceUrl(
  config: RegistryConfig,
  locator: PackageLocator,
  sha: string,
): string {
  return `${config.cdnBaseUrl}/packages/${publicLocatorPath(locator)}/-/${sha}.tar.gz`;
}
