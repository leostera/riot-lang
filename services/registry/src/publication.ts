import gitUrlParse from "git-url-parse";
import semver from "semver";
import spdxExpressionValidate from "spdx-expression-validate";
import { v7 as uuidv7 } from "uuid";

import { buildPublicationManifest } from "./manifest.ts";
import { HttpError } from "./errors.ts";
import { indexPublishedRelease } from "./indexing.ts";
import { isFullSha, isSemverLikeTag } from "./locator.ts";
import {
  applyMetadataMigrations,
  hasPublishedRelease,
  prepareWritePackageClaim,
  prepareWritePublishedRelease,
  prepareWriteRegistryEvent,
  readPackageClaim,
  readPublishedRelease,
  readSelectorResolution,
  writeRegistryEvent,
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

const OCAML_BUILTIN_DEPENDENCIES = new Set([
  "stdlib",
  "unix",
  "dynlink",
]);

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
  await applyMetadataMigrations(env.SEARCH_DB);
  const existingRelease = await readPublishedRelease(
    env.SEARCH_DB,
    manifest.package_name,
    manifest.package_version,
  );

  if (existingRelease !== null) {
    if (
      existingRelease.package_locator !== manifest.package_locator ||
      existingRelease.resolved_sha !== manifest.resolved_sha
    ) {
      throw new HttpError(
        409,
        "package_version_already_published",
        `Package ${manifest.package_name}@${manifest.package_version} is already published from ${existingRelease.package_locator} at ${existingRelease.resolved_sha}.`,
      );
    }

    return {
      ...materialization,
      packageName: manifest.package_name,
      packageVersion: manifest.package_version,
      claimKey: packageClaimKey(manifest.package_name),
      releaseKey: publishedReleaseKey(manifest.package_name, manifest.package_version),
      claimCreated: false,
      releaseCreated: false,
      indexChanged: false,
    };
  }

  assertPublishableManifest(manifest);
  await assertPublishableDependencies(env.SEARCH_DB, manifest);
  const submittedAt = new Date().toISOString();
  const verifiedAt = addMilliseconds(submittedAt, 1);
  const publishedEventAt = addMilliseconds(submittedAt, 2);
  await writeRegistryEvent(
    env.SEARCH_DB,
    makePackageEvent("package.submitted", submittedAt, manifest, {
      selector,
      actor_kind: actor.kind,
    }),
  );

  const now = publishedEventAt;
  await writeRegistryEvent(
    env.SEARCH_DB,
    makePackageEvent("package.verified", verifiedAt, manifest, {
      selector,
      dependency_count: manifest.dependencies.length,
    }),
  );
  const existingClaim = await readPackageClaim(env.SEARCH_DB, manifest.package_name);
  const claimRecord = buildClaimRecord(locator, manifest, existingClaim, actor, now);

  if (existingClaim === null || !claimMatches(existingClaim, claimRecord)) {
    await env.SEARCH_DB.batch([
      prepareWritePackageClaim(env.SEARCH_DB, claimRecord),
    ]);
  }

  let releaseCreated = false;
  let indexChanged = false;
  const releaseRecord = buildPublishedReleaseRecord(manifest, now);
  await env.SEARCH_DB.batch([
    prepareWritePublishedRelease(env.SEARCH_DB, releaseRecord),
    prepareWriteRegistryEvent(
      env.SEARCH_DB,
      makePackageEvent("package.published", publishedEventAt, manifest, {
        selector,
        resolved_sha: releaseRecord.resolved_sha,
        claim_created: existingClaim === null,
        release_created: true,
      }),
    ),
  ]);
  const indexResult = await indexPublishedRelease(env, releaseRecord, manifest);
  indexChanged = indexResult.changed;
  await env.PACKAGE_PUBLISHED_QUEUE.send({
    type: "package.published",
    ...releaseRecord,
  } satisfies PackagePublishedEvent);
  releaseCreated = true;

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

async function assertPublishableDependencies(db: D1Database, manifest: PackagePublicationManifest): Promise<void> {
  for (const dependency of manifest.dependencies) {
    const normalized = normalizeDependencyRecord(dependency);
    if (normalized === null) {
      throw new HttpError(
        422,
        "invalid_dependency_reference",
        `Invalid dependency entry in ${manifest.package_name}: dependency declarations must include a package name.`,
      );
    }

    if (normalized.kind === "path") {
      continue;
    }

    if (normalized.kind === "git") {
      continue;
    }

    if (isBuiltinOcamlDependency(normalized.name)) {
      continue;
    }

    if (!(await hasPublishedRelease(db, normalized.name))) {
      throw new HttpError(
        422,
        "missing_dependency",
        `Dependency ${normalized.name}@${normalized.requirement} is missing from the registry.`,
      );
    }
  }
}

function normalizeDependencyRecord(dependency: Record<string, unknown>): DependencyReference | null {
  const packageName = readDependencyPackageName(dependency);
  if (packageName === null) {
    return null;
  }

  if (typeof dependency.path === "string") {
    if (dependency.path.trim().length === 0) {
      return null;
    }

    return {
      name: packageName,
      kind: "path",
    };
  }

  const gitReference = readStringField(dependency, "git") ?? readStringField(dependency, "url");
  if (gitReference !== null) {
    if (!isValidGitReference(gitReference)) {
      throw new HttpError(
        422,
        "invalid_dependency_reference",
        `Dependency ${packageName} has invalid git reference ${gitReference}.`,
      );
    }

    return {
      name: packageName,
      kind: "git",
      requirement: gitReference,
    };
  }

  const requirement = readStringField(dependency, "requirement") ??
    readStringField(dependency, "version") ??
    readStringField(dependency, "raw");

  if (requirement === null) {
    throw new HttpError(
      422,
      "invalid_dependency_reference",
      `Dependency ${packageName} must declare a requirement, path, or git reference.`,
    );
  }

  if (!isValidSemverRange(requirement)) {
    throw new HttpError(
      422,
      "invalid_dependency_reference",
      `Dependency ${packageName} has non-semver requirement ${requirement}.`,
    );
  }

  return {
    name: packageName,
    requirement,
    requirementKind: "semver",
    kind: "registry",
  };
}

function readDependencyPackageName(dependency: Record<string, unknown>): string | null {
  if (typeof dependency.package === "string") {
    return dependency.package.trim().length > 0 ? dependency.package.trim() : null;
  }

  if (typeof dependency.name === "string") {
    return dependency.name.trim().length > 0 ? dependency.name.trim() : null;
  }

  return null;
}

function readStringField(dependency: Record<string, unknown>, key: string): string | null {
  const value = dependency[key];
  return typeof value === "string" && value.trim().length > 0 ? value.trim() : null;
}

function isValidSemverRange(value: string): boolean {
  const trimmed = value.trim();
  if (trimmed.toLowerCase() === "latest") {
    return true;
  }

  return semver.validRange(trimmed) !== null;
}

function isValidGitReference(value: string): boolean {
  const trimmed = value.trim();
  if (trimmed.length === 0) {
    return false;
  }

  try {
    const parsed = gitUrlParse(trimmed);
    return parsed.owner.length > 0 && parsed.name.length > 0;
  } catch {
    return false;
  }
}

function isBuiltinOcamlDependency(value: string): boolean {
  return OCAML_BUILTIN_DEPENDENCIES.has(value.trim().toLowerCase());
}

type DependencyReference =
  | { name: string; kind: "path" }
  | { name: string; kind: "git"; requirement: string }
  | { name: string; kind: "registry"; requirement: string; requirementKind: "semver" };

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

function isValidSemver(value: string): boolean {
  return semver.valid(value.trim()) !== null;
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

function makePackageEvent(
  eventType: "package.submitted" | "package.verified" | "package.published",
  createdAt: string,
  manifest: PackagePublicationManifest,
  payload: Record<string, unknown>,
) {
  return {
    event_id: uuidv7({
      msecs: Date.parse(createdAt),
    }),
    event_type: eventType,
    package_name: manifest.package_name,
    package_version: manifest.package_version,
    package_locator: manifest.package_locator,
    payload,
    created_at: createdAt,
  } as const;
}

function addMilliseconds(timestamp: string, milliseconds: number): string {
  return new Date(Date.parse(timestamp) + milliseconds).toISOString();
}
