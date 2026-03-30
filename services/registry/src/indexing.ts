import { getConfig } from "./config.ts";
import { buildIndexedRelease, upsertPackageDocument } from "./index-document.ts";
import { applySearchMigrations, prepareUpsertSearchRowStatements } from "./search-db.ts";
import { buildSearchRow } from "./search-document.ts";
import { applyMetadataMigrations, prepareWriteRegistryEvent, writeRegistryEvent } from "./metadata-db.ts";
import { v7 as uuidv7 } from "uuid";
import {
  packageIndexKey,
  packageIndexUrl,
  readPackageIndexDocument,
  writeIndexConfig,
  writePackageIndexDocument,
} from "./storage.ts";
import { rebuildWebViews } from "./web-views.ts";
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
  await applyMetadataMigrations(env.SEARCH_DB);
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
  const searchRow = buildSearchRow(document);
  const searchableAt = addMilliseconds(document.updated_at, 1);
  const indexedAt = addMilliseconds(document.updated_at, 2);

  if (!changed) {
    await applySearchMigrations(env.SEARCH_DB);
    await env.SEARCH_DB.batch([
      ...prepareUpsertSearchRowStatements(env.SEARCH_DB, searchRow),
      prepareWriteRegistryEvent(
        env.SEARCH_DB,
        makeIndexEvent("package.searchable", releaseRecord, searchableAt, {
          latest: document.latest,
          package_index_key: key,
          package_index_url: url,
          changed: false,
        }),
      ),
    ]);
    await rebuildWebViews(env);
    await writeRegistryEvent(
      env.SEARCH_DB,
      makeIndexEvent("package.indexed", releaseRecord, indexedAt, {
        latest: document.latest,
        package_index_key: key,
        package_index_url: url,
        changed: false,
      }),
    );

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
  await applySearchMigrations(env.SEARCH_DB);
  await env.SEARCH_DB.batch([
    ...prepareUpsertSearchRowStatements(env.SEARCH_DB, searchRow),
    prepareWriteRegistryEvent(
      env.SEARCH_DB,
      makeIndexEvent("package.searchable", releaseRecord, searchableAt, {
        latest: document.latest,
        package_index_key: key,
        package_index_url: url,
        changed: true,
      }),
    ),
  ]);
  await rebuildWebViews(env);
  await writeRegistryEvent(
    env.SEARCH_DB,
    makeIndexEvent("package.indexed", releaseRecord, indexedAt, {
      latest: document.latest,
      package_index_key: key,
      package_index_url: url,
      changed: true,
    }),
  );
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

function makeIndexEvent(
  eventType: "package.indexed" | "package.searchable",
  releaseRecord: PublishedReleaseRecord,
  createdAt: string,
  payload: Record<string, unknown>,
) {
  return {
    event_id: uuidv7({
      msecs: Date.parse(createdAt),
    }),
    event_type: eventType,
    package_name: releaseRecord.package_name,
    package_version: releaseRecord.package_version,
    package_locator: releaseRecord.package_locator,
    payload: {
      resolved_sha: releaseRecord.resolved_sha,
      ...payload,
    },
    created_at: createdAt,
  } as const;
}

function addMilliseconds(timestamp: string, milliseconds: number): string {
  return new Date(Date.parse(timestamp) + milliseconds).toISOString();
}
