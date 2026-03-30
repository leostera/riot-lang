import type { Env, RequestLogEntry } from "./types.ts";
import {
  applyMetadataMigrations,
  writeRequestLog as writeRequestLogToDb,
} from "./metadata-db.ts";

export async function writeRequestLog(env: Env, entry: RequestLogEntry): Promise<void> {
  await applyMetadataMigrations(env.SEARCH_DB);
  await writeRequestLogToDb(env.SEARCH_DB, entry);
}
