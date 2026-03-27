import { requestLogKey } from "./storage.ts";
import type { Env, RequestLogEntry } from "./types.ts";

export async function writeRequestLog(env: Env, entry: RequestLogEntry): Promise<void> {
  await env.ML_PKGS_CDN.put(requestLogKey(entry), JSON.stringify(entry, null, 2), {
    httpMetadata: {
      contentType: "application/json; charset=utf-8",
    },
  });
}
