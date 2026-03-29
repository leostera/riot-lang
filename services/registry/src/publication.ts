import spdxExpressionValidate from "spdx-expression-validate";

import { buildPublicationManifest } from "./manifest.ts";
import { HttpError } from "./errors.ts";
import { indexPublishedRelease } from "./indexing.ts";
import { isFullSha, isSemverLikeTag } from "./locator.ts";
import {
  applyMetadataMigrations,
  readPackageClaim,
  readPublishedRelease,
  readSelectorResolution,
  writePackageClaim,
  writePublishedRelease,
  writeSelectorResolution,
} from "./metadata-db.ts";
import {
  manifestKey,
  packageClaimKey,
  publishedReleaseKey,
  readPublicationManifest,
  sourceArchiveKey,
} from "./storage.ts";
import type {
  AuthenticatedActor,
  Env,
  PackageClaimRecord,
  PackageLocator,
  PackagePublicationManifest,
  PackagePublishedEvent,
  PublishedPackageRelease,
  PublishedReleaseRecord,
  ResolvedPublication,
} from "./types.ts";
import { assertGitHubRepositoryAccess, fetchGitHubTarball, resolveGitHubSelector } from "./github.ts";

export async function ensureSourceMaterialization(
  env: Env,
  locator: PackageLocator,
  selector: string,
): Promise<ResolvedPublication> {
  await assertGitHubRepositoryAccess(env, locator);
  await applyMetadataMigrations(env.SEARCH_DB);

  const materializedAt = new Date().toISOString();
  const freezeSelector = isSemverLikeTag(selector);
  const selectorRecord = freezeSelector
    ? await readSelectorResolution(env.SEARCH_DB, locator.normalized, selector)
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
    await writeSelectorResolution(env.SEARCH_DB, {
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
  actor: AuthenticatedActor,
): Promise<PublishedPackageRelease> {
  const materialization = await ensureSourceMaterialization(env, locator, selector);
  const manifest = await readMaterializationManifest(env, materialization.manifestKey);
  assertPublishableManifest(manifest);
  await applyMetadataMigrations(env.SEARCH_DB);

  const now = new Date().toISOString();
  const existingClaim = await readPackageClaim(env.SEARCH_DB, manifest.package_name);
  const claimRecord = buildClaimRecord(locator, manifest, existingClaim, actor, now);

  if (existingClaim === null || !claimMatches(existingClaim, claimRecord)) {
    await writePackageClaim(env.SEARCH_DB, claimRecord);
  }

  const existingRelease = await readPublishedRelease(
    env.SEARCH_DB,
    manifest.package_name,
    manifest.package_version,
  );

  let releaseCreated = false;
  let indexChanged = false;
  if (existingRelease === null) {
    const releaseRecord = buildPublishedReleaseRecord(manifest, now);
    await writePublishedRelease(env.SEARCH_DB, releaseRecord);
    const indexResult = await indexPublishedRelease(env, releaseRecord, manifest);
    indexChanged = indexResult.changed;
    await env.PACKAGE_PUBLISHED_QUEUE.send({
      type: "package.published",
      ...releaseRecord,
    } satisfies PackagePublishedEvent);
    releaseCreated = true;
  } else {
    const releaseRecord = releaseMatchesManifest(existingRelease, manifest)
      ? existingRelease
      : buildPublishedReleaseRecord(manifest, now);

    if (releaseRecord !== existingRelease) {
      await writePublishedRelease(env.SEARCH_DB, releaseRecord);
    }

    const indexResult = await indexPublishedRelease(env, releaseRecord, manifest);
    indexChanged = indexResult.changed;
  }

  return {
    ...materialization,
    packageName: manifest.package_name,
    packageVersion: manifest.package_version,
    claimKey: packageClaimKey(manifest.package_name),
    releaseKey: publishedReleaseKey(manifest.package_name, manifest.package_version),
    claimCreated: existingClaim === null,
    releaseCreated,
    indexChanged,
  };
}

export async function readCachedMaterialization(
  env: Env,
  locator: PackageLocator,
  selector: string,
): Promise<ResolvedPublication | null> {
  await applyMetadataMigrations(env.SEARCH_DB);
  const resolvedSha =
    isFullSha(selector)
      ? selector
      : isSemverLikeTag(selector)
        ? (await readSelectorResolution(env.SEARCH_DB, locator.normalized, selector))?.resolved_sha ?? null
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
  const manifest = await readPublicationManifest(env.ML_PKGS_CDN, key);
  if (manifest === null) {
    throw new Error(`Expected manifest ${key} to exist.`);
  }

  return manifest;
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

  if (manifest.package_description === undefined) {
    throw new HttpError(
      422,
      "missing_package_description",
      `Package ${manifest.package_locator} must declare package.description to be publishable.`,
    );
  }

  if (manifest.package_license === undefined) {
    throw new HttpError(
      422,
      "missing_package_license",
      `Package ${manifest.package_locator} must declare package.license to be publishable.`,
    );
  }

  if (!spdxExpressionValidate(manifest.package_license)) {
    throw new HttpError(
      422,
      "invalid_package_license",
      `Package ${manifest.package_locator} must declare an SPDX-compatible package.license value.`,
    );
  }
}

function buildClaimRecord(
  locator: PackageLocator,
  manifest: PackagePublicationManifest,
  existingClaim: PackageClaimRecord | null,
  actor: AuthenticatedActor,
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

    if (actor.kind === "user") {
      const actorOwnsSource = ownsSourceLocator(actor.githubLogin, locator);
      const ownerUserIdMatches =
        existingClaim.owner_user_id !== undefined && existingClaim.owner_user_id === actor.userId;
      const ownerLoginMatches =
        existingClaim.owner_github_login !== undefined &&
        existingClaim.owner_github_login.toLowerCase() === actor.githubLogin.toLowerCase();
      const claimUnowned =
        existingClaim.owner_user_id === undefined && existingClaim.owner_github_login === undefined;

      if (claimUnowned) {
        if (!actorOwnsSource) {
          throw new HttpError(
            403,
            "package_claim_forbidden",
            `Package ${manifest.package_name} can only be claimed by GitHub user ${locator.owner}.`,
          );
        }

        return {
          ...existingClaim,
          owner_user_id: actor.userId,
          owner_github_login: actor.githubLogin,
          updated_at: timestamp,
        };
      }

      if (!ownerUserIdMatches && !ownerLoginMatches) {
        throw new HttpError(
          403,
          "package_claim_forbidden",
          `Package ${manifest.package_name} is owned by another publisher.`,
        );
      }
    }

    return {
      ...existingClaim,
      updated_at: timestamp,
    };
  }

  if (actor.kind === "user" && !ownsSourceLocator(actor.githubLogin, locator)) {
    throw new HttpError(
      403,
      "package_claim_forbidden",
      `Package ${manifest.package_name} can only be claimed by GitHub user ${locator.owner}.`,
    );
  }

  return {
    package_name: manifest.package_name,
    package_locator: manifest.package_locator,
    source_url: manifest.source_url,
    package_subdir: manifest.package_subdir,
    owner_user_id: actor.kind === "user" ? actor.userId : undefined,
    owner_github_login: actor.kind === "user" ? actor.githubLogin : undefined,
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
    package_description: manifest.package_description,
    package_license: manifest.package_license,
    package_homepage: manifest.package_homepage,
    package_repository: manifest.package_repository,
    package_root_module: manifest.package_root_module,
    package_categories: manifest.package_categories,
    package_keywords: manifest.package_keywords,
    dependencies: manifest.dependencies,
    source_archive_key: manifest.source_archive_key,
    manifest_key: manifest.manifest_key,
    published_at: publishedAt,
  };
}

function releaseMatchesManifest(
  existingRelease: PublishedReleaseRecord,
  manifest: PackagePublicationManifest,
): boolean {
  return (
    existingRelease.package_locator === manifest.package_locator &&
    existingRelease.resolved_sha === manifest.resolved_sha &&
    existingRelease.package_description === manifest.package_description &&
    existingRelease.package_license === manifest.package_license &&
    existingRelease.package_homepage === manifest.package_homepage &&
    existingRelease.package_repository === manifest.package_repository &&
    existingRelease.package_root_module === manifest.package_root_module &&
    JSON.stringify(existingRelease.package_categories ?? []) ===
      JSON.stringify(manifest.package_categories ?? []) &&
    JSON.stringify(existingRelease.package_keywords ?? []) ===
      JSON.stringify(manifest.package_keywords ?? []) &&
    JSON.stringify(existingRelease.dependencies) === JSON.stringify(manifest.dependencies) &&
    existingRelease.source_archive_key === manifest.source_archive_key &&
    existingRelease.manifest_key === manifest.manifest_key
  );
}

function isValidSemver(value: string): boolean {
  return /^(0|[1-9]\d*)\.(0|[1-9]\d*)\.(0|[1-9]\d*)(?:-[0-9A-Za-z.-]+)?(?:\+[0-9A-Za-z.-]+)?$/.test(
    value,
  );
}

function ownsSourceLocator(githubLogin: string, locator: PackageLocator): boolean {
  return locator.owner.toLowerCase() === githubLogin.toLowerCase();
}

function claimMatches(left: PackageClaimRecord, right: PackageClaimRecord): boolean {
  return (
    left.package_name === right.package_name &&
    left.package_locator === right.package_locator &&
    left.source_url === right.source_url &&
    left.package_subdir === right.package_subdir &&
    left.owner_user_id === right.owner_user_id &&
    left.owner_github_login === right.owner_github_login
  );
}
