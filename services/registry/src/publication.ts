import { buildPublicationManifest } from "./manifest.ts";
import { HttpError } from "./errors.ts";
import { isFullSha, isSemverLikeTag } from "./locator.ts";
import {
  manifestKey,
  packageClaimKey,
  publishedReleaseKey,
  readPackageClaim,
  readPublishedRelease,
  readSelectorResolution,
  sourceArchiveKey,
  writePackageClaim,
  writePublishedRelease,
  writeSelectorResolution,
} from "./storage.ts";
import type {
  Env,
  PackageClaimRecord,
  PackageLocator,
  PackagePublicationManifest,
  PackagePublishedEvent,
  PublishedPackageRelease,
  PublishedReleaseRecord,
  ResolvedPublication,
} from "./types.ts";
import {
  assertGitHubRepositoryAccess,
  fetchGitHubTarball,
  resolveGitHubSelector,
} from "./github.ts";

export async function ensureSourceMaterialization(
  env: Env,
  locator: PackageLocator,
  selector: string,
): Promise<ResolvedPublication> {
  await assertGitHubRepositoryAccess(env, locator);

  const materializedAt = new Date().toISOString();
  const freezeSelector = isSemverLikeTag(selector);
  const selectorRecord = freezeSelector
    ? await readSelectorResolution(env.ML_PKGS_CDN, locator, selector)
    : null;

  const resolvedSha =
    selectorRecord?.resolved_sha ??
    (isFullSha(selector) ? selector : await resolveGitHubSelector(env, locator, selector));

  const sourceKey = sourceArchiveKey(locator, resolvedSha);
  const targetManifestKey = manifestKey(locator, resolvedSha);
  const existingSource = await env.ML_PKGS_CDN.head(sourceKey);
  const existingManifest = await env.ML_PKGS_CDN.head(targetManifestKey);

  let archiveBytes: Uint8Array<ArrayBuffer> | null = null;
  let manifest: PackagePublicationManifest | null = null;
  let sourceCreated = false;
  let manifestCreated = false;

  if (existingSource === null) {
    archiveBytes = await fetchGitHubTarball(env, locator, resolvedSha);
    if (existingManifest === null) {
      manifest = await buildPublicationManifest({
        locator,
        selector,
        resolvedSha,
        archiveBytes,
        materializedAt,
      });
    }

    await env.ML_PKGS_CDN.put(sourceKey, archiveBytes, {
      httpMetadata: {
        contentType: "application/gzip",
      },
    });
    sourceCreated = true;
  }

  if (existingManifest === null) {
    if (archiveBytes === null) {
      const sourceObject = await env.ML_PKGS_CDN.get(sourceKey);
      if (sourceObject === null) {
        throw new Error(`Expected source archive ${sourceKey} to exist.`);
      }

      archiveBytes = new Uint8Array(await sourceObject.arrayBuffer());
    }

    if (manifest === null) {
      manifest = await buildPublicationManifest({
        locator,
        selector,
        resolvedSha,
        archiveBytes,
        materializedAt,
      });
    }

    await env.ML_PKGS_CDN.put(targetManifestKey, JSON.stringify(manifest, null, 2), {
      httpMetadata: {
        contentType: "application/json; charset=utf-8",
      },
    });

    manifestCreated = true;
  }

  if (freezeSelector && selectorRecord === null) {
    await writeSelectorResolution(env.ML_PKGS_CDN, locator, {
      package_locator: locator.normalized,
      selector,
      resolved_sha: resolvedSha,
      frozen: true,
      recorded_at: materializedAt,
    });
  }

  return {
    selector,
    resolvedSha,
    sourceKey,
    manifestKey: targetManifestKey,
    sourceCreated,
    manifestCreated,
  };
}

export async function publishPackageRelease(
  env: Env,
  locator: PackageLocator,
  selector: string,
): Promise<PublishedPackageRelease> {
  const materialization = await ensureSourceMaterialization(env, locator, selector);
  const manifest = await readMaterializationManifest(env, materialization.manifestKey);
  assertPublishableManifest(manifest);

  const now = new Date().toISOString();
  const existingClaim = await readPackageClaim(env.ML_PKGS_CDN, manifest.package_name);
  const claimRecord = buildClaimRecord(manifest, existingClaim, now);

  if (existingClaim === null) {
    await writePackageClaim(env.ML_PKGS_CDN, claimRecord);
  }

  const existingRelease = await readPublishedRelease(
    env.ML_PKGS_CDN,
    manifest.package_name,
    manifest.package_version,
  );

  let releaseCreated = false;
  if (existingRelease === null) {
    const releaseRecord = buildPublishedReleaseRecord(manifest, now);
    await writePublishedRelease(env.ML_PKGS_CDN, releaseRecord);
    await env.PACKAGE_PUBLISHED_QUEUE.send({
      type: "package.published",
      ...releaseRecord,
    } satisfies PackagePublishedEvent);
    releaseCreated = true;
  } else {
    assertMatchingRelease(existingRelease, manifest);
  }

  return {
    ...materialization,
    packageName: manifest.package_name,
    packageVersion: manifest.package_version,
    claimKey: packageClaimKey(manifest.package_name),
    releaseKey: publishedReleaseKey(manifest.package_name, manifest.package_version),
    claimCreated: existingClaim === null,
    releaseCreated,
  };
}

export async function readCachedMaterialization(
  env: Env,
  locator: PackageLocator,
  selector: string,
): Promise<ResolvedPublication | null> {
  const resolvedSha =
    isFullSha(selector)
      ? selector
      : isSemverLikeTag(selector)
        ? (await readSelectorResolution(env.ML_PKGS_CDN, locator, selector))?.resolved_sha ?? null
        : null;

  if (resolvedSha === null) {
    return null;
  }

  const sourceKey = sourceArchiveKey(locator, resolvedSha);
  const targetManifestKey = manifestKey(locator, resolvedSha);
  const [sourceObject, manifestObject] = await Promise.all([
    env.ML_PKGS_CDN.head(sourceKey),
    env.ML_PKGS_CDN.head(targetManifestKey),
  ]);

  if (sourceObject === null || manifestObject === null) {
    return null;
  }

  return {
    selector,
    resolvedSha,
    sourceKey,
    manifestKey: targetManifestKey,
    sourceCreated: false,
    manifestCreated: false,
  };
}

async function readMaterializationManifest(
  env: Env,
  key: string,
): Promise<PackagePublicationManifest> {
  const object = await env.ML_PKGS_CDN.get(key);
  if (object === null) {
    throw new Error(`Expected manifest ${key} to exist.`);
  }

  return await object.json<PackagePublicationManifest>();
}

function assertPublishableManifest(manifest: PackagePublicationManifest): void {
  if (!manifest.package_public) {
    throw new HttpError(
      422,
      "package_not_public",
      `Package ${manifest.package_locator} is not publishable because package.public is false.`,
    );
  }

  if (!isValidSemver(manifest.package_version)) {
    throw new HttpError(
      422,
      "invalid_package_version",
      `Package ${manifest.package_locator} has non-semver version ${manifest.package_version}.`,
    );
  }
}

function buildClaimRecord(
  manifest: PackagePublicationManifest,
  existingClaim: PackageClaimRecord | null,
  timestamp: string,
): PackageClaimRecord {
  if (existingClaim !== null) {
    if (existingClaim.package_locator !== manifest.package_locator) {
      throw new HttpError(
        409,
        "package_name_taken",
        `Package name ${manifest.package_name} is already claimed by ${existingClaim.package_locator}.`,
      );
    }

    return {
      ...existingClaim,
      updated_at: timestamp,
    };
  }

  return {
    package_name: manifest.package_name,
    package_locator: manifest.package_locator,
    source_url: manifest.source_url,
    package_subdir: manifest.package_subdir,
    claimed_at: timestamp,
    updated_at: timestamp,
  };
}

function buildPublishedReleaseRecord(
  manifest: PackagePublicationManifest,
  publishedAt: string,
): PublishedReleaseRecord {
  return {
    package_name: manifest.package_name,
    package_version: manifest.package_version,
    package_locator: manifest.package_locator,
    source_url: manifest.source_url,
    package_subdir: manifest.package_subdir,
    selector: manifest.selector,
    resolved_sha: manifest.resolved_sha,
    dependencies: manifest.dependencies,
    source_archive_key: manifest.source_archive_key,
    manifest_key: manifest.manifest_key,
    published_at: publishedAt,
  };
}

function assertMatchingRelease(
  existingRelease: PublishedReleaseRecord,
  manifest: PackagePublicationManifest,
): void {
  if (
    existingRelease.package_locator !== manifest.package_locator ||
    existingRelease.resolved_sha !== manifest.resolved_sha
  ) {
    throw new HttpError(
      409,
      "version_already_published",
      `Version ${manifest.package_name}@${manifest.package_version} is already published.`,
    );
  }
}

function isValidSemver(value: string): boolean {
  return /^(0|[1-9]\d*)\.(0|[1-9]\d*)\.(0|[1-9]\d*)(?:-[0-9A-Za-z.-]+)?(?:\+[0-9A-Za-z.-]+)?$/.test(
    value,
  );
}
