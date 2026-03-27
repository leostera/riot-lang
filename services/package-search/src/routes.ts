import { getConfig } from "./config.ts";
import { applyMigrations, searchPackages } from "./db.ts";
import { json, methodNotAllowed } from "./http.ts";
import type { Env } from "./types.ts";

export async function handleRequest(request: Request, env: Env): Promise<Response> {
  if (request.method !== "GET") {
    return methodNotAllowed(["GET"]);
  }

  const url = new URL(request.url);
  const query = url.searchParams.get("q");

  if (query === null || query.trim().length === 0) {
    return json({
      service: "riot-package-search",
      route: "/?q=<query>",
      source: {
        package_index_base_url: `${getConfig(env).cdnBaseUrl}/${getConfig(env).indexBasePath}`,
        queue_consumer: "package.indexed",
      },
    });
  }

  await applyMigrations(env.SEARCH_DB);
  const limit = clampInteger(url.searchParams.get("limit"), 20, 1, 100);
  const offset = clampInteger(url.searchParams.get("offset"), 0, 0, 10_000);
  const results = await searchPackages(env.SEARCH_DB, query, limit, offset);

  return json({
    query,
    count: results.length,
    results,
  });
}

function clampInteger(
  value: string | null,
  fallback: number,
  minimum: number,
  maximum: number,
): number {
  if (value === null) {
    return fallback;
  }

  const parsed = Number.parseInt(value, 10);
  if (!Number.isFinite(parsed)) {
    return fallback;
  }

  return Math.min(maximum, Math.max(minimum, parsed));
}
