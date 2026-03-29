export interface WebConfig {
  searchApiBaseUrl: string;
  cdnBaseUrl: string;
  indexBasePath: string;
  viewsBasePath: string;
  registryBaseUrl: string;
}

export function getConfig(): WebConfig {
  const registryBaseUrl =
    import.meta.env.PUBLIC_REGISTRY_BASE_URL?.trim() || "https://registry.pkgs.ml";

  return {
    searchApiBaseUrl:
      import.meta.env.PUBLIC_SEARCH_API_BASE_URL?.trim() || `${registryBaseUrl}/api/v1/search`,
    cdnBaseUrl: import.meta.env.PUBLIC_CDN_BASE_URL?.trim() || "https://cdn.pkgs.ml",
    indexBasePath: import.meta.env.PUBLIC_INDEX_BASE_PATH?.trim() || "index/v1",
    viewsBasePath: import.meta.env.PUBLIC_VIEWS_BASE_PATH?.trim() || "views/v1",
    registryBaseUrl,
  };
}
