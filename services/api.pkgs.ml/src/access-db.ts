import { packageDownloads, indexReads } from "./schema.ts";
import { registryDb } from "./db.ts";
import type { IndexReadRecord, PackageDownloadRecord } from "./types.ts";

export async function writeIndexReadRecord(
  db: D1Database,
  record: IndexReadRecord,
): Promise<void> {
  const database = registryDb(db);
  await database.insert(indexReads).values({
    readId: record.read_id,
    documentKey: record.document_key,
    packageName: record.package_name ?? null,
    readAt: record.read_at,
  });
}

export async function writePackageDownloadRecord(
  db: D1Database,
  record: PackageDownloadRecord,
): Promise<void> {
  const database = registryDb(db);
  await database.insert(packageDownloads).values({
    downloadId: record.download_id,
    packageName: record.package_name,
    packageVersion: record.package_version,
    artifactSha256: record.artifact_sha256,
    sourceArchiveKey: record.source_archive_key,
    downloadedAt: record.downloaded_at,
  });
}
