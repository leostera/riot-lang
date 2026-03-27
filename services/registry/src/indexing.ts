import { getConfig } from "./config.ts";
import { buildIndexedRelease, upsertPackageDocument } from "./index-document.ts";
import {
  packageIndexKey,
  packageIndexUrl,
  readPackageIndexDocument,
  readPublicationManifest,
  writeIndexConfig,
  writePackageIndexDocument,
} from "./storage.ts";
import type {
  Env,
  PackageIndexedEvent,
  PackagePublicationManifest,
  PublishedReleaseRecord,
} from "./types.ts";

export async function indexPublishedRelease(
  env: Env,
  releaseRecord: PublishedReleaseRecord,
  manifest: PackagePublicationManifest,
): Promise<{
  changed: boolean;
  latest: string;
  indexedAt: string;
  packageIndexKey: string;
  packageIndexUrl: string;
}> {
  const config = getConfig(env);
  await writeIndexConfig(env.ML_PKGS_CDN, config);

  const currentDocument = await readPackageIndexDocument(
    env.ML_PKGS_CDN,
    config,
    releaseRecord.package_name,
  );

  const release = buildIndexedRelease(releaseRecord, manifest);
  const { document, changed } = upsertPackageDocument({
    existing: currentDocument,
    packageName: releaseRecord.package_name,
    release,
    updatedAt: releaseRecord.published_at,
  });

  const key = packageIndexKey(config, releaseRecord.package_name);
  const url = packageIndexUrl(config, releaseRecord.package_name);

  if (!changed) {
    await env.PACKAGE_INDEXED_QUEUE.send({
      type: "package.indexed",
      package_name: releaseRecord.package_name,
      package_version: releaseRecord.package_version,
      package_locator: releaseRecord.package_locator,
      resolved_sha: releaseRecord.resolved_sha,
      package_index_key: key,
      package_index_url: url,
      latest: document.latest,
      indexed_at: document.updated_at,
    } satisfies PackageIndexedEvent);

    return {
      changed: false,
      latest: document.latest,
      indexedAt: document.updated_at,
      packageIndexKey: key,
      packageIndexUrl: url,
    };
  }

  await writePackageIndexDocument(env.ML_PKGS_CDN, config, document);
  await env.PACKAGE_INDEXED_QUEUE.send({
    type: "package.indexed",
    package_name: releaseRecord.package_name,
    package_version: releaseRecord.package_version,
    package_locator: releaseRecord.package_locator,
    resolved_sha: releaseRecord.resolved_sha,
    package_index_key: key,
    package_index_url: url,
    latest: document.latest,
    indexed_at: document.updated_at,
  } satisfies PackageIndexedEvent);

  return {
    changed: true,
    latest: document.latest,
    indexedAt: document.updated_at,
    packageIndexKey: key,
    packageIndexUrl: url,
  };
}
