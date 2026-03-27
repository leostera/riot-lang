import { HttpError } from "./errors.ts";
import type { PackageLocator } from "./types.ts";

const FULL_SHA = /^[0-9a-f]{40}$/i;
const SEMVER_LIKE_TAG = /^\d+\.\d+\.\d+(?:[-+][0-9A-Za-z.-]+)?$/;

export function normalizeLocator(rawValue: string): PackageLocator {
  const stripped = stripProtocol(rawValue).replace(/^\/+|\/+$/g, "");
  if (stripped.length === 0) {
    throw new HttpError(400, "invalid_locator", "Package locator cannot be empty.");
  }

  const segments = stripped.split("/").filter(Boolean);
  const normalizedSegments =
    segments[0]?.includes(".") ?? false ? segments : ["github.com", ...segments];

  if (normalizedSegments.length < 3) {
    throw new HttpError(
      400,
      "invalid_locator",
      "Package locator must include at least provider, owner, and repo.",
    );
  }

  const provider = normalizedSegments[0];
  const owner = normalizedSegments[1];
  const repo = normalizedSegments[2];
  const rest = normalizedSegments.slice(3);

  if (provider === undefined || owner === undefined || repo === undefined) {
    throw new HttpError(
      400,
      "invalid_locator",
      "Package locator must include at least provider, owner, and repo.",
    );
  }

  return {
    raw: rawValue,
    normalized: normalizedSegments.join("/"),
    provider,
    owner,
    repo,
    subpath: rest.length > 0 ? rest.join("/") : null,
  };
}

export function packageSubdir(locator: PackageLocator): string {
  return locator.subpath ?? ".";
}

export function canonicalSourceUrl(locator: PackageLocator): string {
  return `https://${locator.provider}/${locator.owner}/${locator.repo}`;
}

export function isFullSha(value: string): boolean {
  return FULL_SHA.test(value);
}

export function isSemverLikeTag(value: string): boolean {
  return SEMVER_LIKE_TAG.test(value);
}

export function publicLocatorPath(locator: PackageLocator): string {
  if (locator.provider === "github.com") {
    return [locator.owner, locator.repo, locator.subpath].filter(Boolean).join("/");
  }

  return locator.normalized;
}

function stripProtocol(value: string): string {
  return value.replace(/^https?:\/\//, "");
}
