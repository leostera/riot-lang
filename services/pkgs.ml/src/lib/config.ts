export interface WebConfig {
  searchBaseUrl: string;
  cdnBaseUrl: string;
  indexBasePath: string;
  registryBaseUrl: string;
}

export function getConfig(): WebConfig {
  return {
    searchBaseUrl:
      import.meta.env.PUBLIC_SEARCH_BASE_URL?.trim() || "https://search.pkgs.ml",
    cdnBaseUrl: import.meta.env.PUBLIC_CDN_BASE_URL?.trim() || "https://cdn.pkgs.ml",
    indexBasePath: import.meta.env.PUBLIC_INDEX_BASE_PATH?.trim() || "index/v1",
    registryBaseUrl:
      import.meta.env.PUBLIC_REGISTRY_BASE_URL?.trim() || "https://registry.pkgs.ml",
  };
}
