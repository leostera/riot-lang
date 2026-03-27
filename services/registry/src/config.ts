import type { Env, RegistryConfig } from "./types.ts";

export function getConfig(env: Env): RegistryConfig {
  return {
    cdnBaseUrl: trimTrailingSlash(env.CDN_BASE_URL ?? "https://cdn.pkgs.ml"),
    indexBasePath: trimSlashes(env.INDEX_BASE_PATH ?? "index/v1"),
  };
}

export function getGitHubApiBaseUrl(env: Env): string {
  return trimTrailingSlash(env.GITHUB_API_BASE_URL ?? "https://api.github.com");
}

function trimTrailingSlash(value: string): string {
  return value.endsWith("/") ? value.slice(0, -1) : value;
}

function trimSlashes(value: string): string {
  return value.replace(/^\/+|\/+$/g, "");
}
