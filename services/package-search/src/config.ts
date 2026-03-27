import type { Env, SearchConfig } from "./types.ts";

export function getConfig(env: Env): SearchConfig {
  return {
    cdnBaseUrl: trimTrailingSlash(env.CDN_BASE_URL ?? "https://cdn.pkgs.ml"),
    indexBasePath: trimSlashes(env.INDEX_BASE_PATH ?? "index/v1"),
  };
}

function trimTrailingSlash(value: string): string {
  return value.endsWith("/") ? value.slice(0, -1) : value;
}

function trimSlashes(value: string): string {
  return value.replace(/^\/+|\/+$/g, "");
}
