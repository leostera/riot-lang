import { buildPublicationManifest } from "./manifest.ts";
import { isFullSha, isSemverLikeTag } from "./locator.ts";
import { manifestKey, readSelectorResolution, sourceArchiveKey, writeSelectorResolution } from "./storage.ts";
import type { Env, PackageLocator, PackagePublishedEvent, ResolvedPublication } from "./types.ts";
import { assertGitHubRepositoryAccess, fetchGitHubTarball, resolveGitHubSelector } from "./github.ts";

export async function ensurePublication(
  env: Env,
  locator: PackageLocator,
  selector: string,
): Promise<ResolvedPublication> {
  await assertGitHubRepositoryAccess(env, locator);

  const publishedAt = new Date().toISOString();
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
  let sourceCreated = false;
  let manifestCreated = false;

  if (existingSource === null) {
    archiveBytes = await fetchGitHubTarball(env, locator, resolvedSha);
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

    const manifest = await buildPublicationManifest({
      locator,
      selector,
      resolvedSha,
      archiveBytes,
      publishedAt,
    });

    await env.ML_PKGS_CDN.put(targetManifestKey, JSON.stringify(manifest, null, 2), {
      httpMetadata: {
        contentType: "application/json; charset=utf-8",
      },
    });

    await env.PACKAGE_PUBLISHED_QUEUE.send({
      type: "package.published",
      ...manifest,
    } satisfies PackagePublishedEvent);

    manifestCreated = true;
  }

  if (freezeSelector && selectorRecord === null) {
    await writeSelectorResolution(env.ML_PKGS_CDN, locator, {
      package_locator: locator.normalized,
      selector,
      resolved_sha: resolvedSha,
      frozen: true,
      recorded_at: publishedAt,
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

export async function readCachedPublication(
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
