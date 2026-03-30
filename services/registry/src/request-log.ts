import type { Env, RequestLogEntry } from "./types.ts";
import { writeRequestLog as writeRequestLogToDb } from "./metadata-db.ts";

export async function writeRequestLog(env: Env, entry: RequestLogEntry): Promise<void> {
  await writeRequestLogToDb(env.SEARCH_DB, entry);
}
