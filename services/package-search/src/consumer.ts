import { getConfig } from "./config.ts";
import { applyMigrations, upsertSearchRow } from "./db.ts";
import { buildSearchRow } from "./search-document.ts";
import { readPackageIndexDocument } from "./storage.ts";
import type { Env, PackageIndexedEvent } from "./types.ts";

export async function consumeIndexedBatch(
  batch: MessageBatch<PackageIndexedEvent>,
  env: Env,
  _ctx: ExecutionContext,
): Promise<void> {
  await applyMigrations(env.SEARCH_DB);

  for (const message of batch.messages) {
    const event = message.body;
    if (event.type !== "package.indexed") {
      message.ack();
      continue;
    }

    const document =
      (await readPackageIndexDocument(env.ML_PKGS_CDN, event.package_index_key)) ??
      (await readPackageIndexDocument(env.ML_PKGS_CDN, packageIndexKeyFromEvent(env, event)));

    if (document === null) {
      throw new Error(`Indexed package document ${event.package_index_key} was not found.`);
    }

    const row = buildSearchRow(document);
    await upsertSearchRow(env.SEARCH_DB, row);
    message.ack();
  }
}

function packageIndexKeyFromEvent(env: Env, event: PackageIndexedEvent): string {
  const config = getConfig(env);
  const prefix = `${config.cdnBaseUrl}/`;
  return event.package_index_url.startsWith(prefix)
    ? event.package_index_url.slice(prefix.length)
    : event.package_index_key;
}
