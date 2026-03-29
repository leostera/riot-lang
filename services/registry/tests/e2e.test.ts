import { describe, expect, test } from "bun:test";

const baseUrl = trimTrailingSlash(process.env.REGISTRY_E2E_BASE_URL);
const packageLocator =
  process.env.REGISTRY_E2E_PACKAGE_LOCATOR ?? "github.com/leostera/riot-new/packages/kernel";
const selector = process.env.REGISTRY_E2E_SELECTOR ?? "main";
const publishPackageLocator = process.env.REGISTRY_E2E_PUBLISH_PACKAGE_LOCATOR ?? packageLocator;
const liveTest = baseUrl === null ? test.skip : test;
const rootAuthToken = process.env.REGISTRY_E2E_ROOT_AUTH_TOKEN ?? null;
const livePublishTest = baseUrl === null || rootAuthToken === null ? test.skip : test;
const sessionCookie = process.env.REGISTRY_E2E_SESSION_COOKIE ?? null;
const githubLogin = process.env.REGISTRY_E2E_GITHUB_LOGIN ?? null;
const liveAuthTest = baseUrl === null ? test.skip : test;
const liveAuthenticatedTest =
  baseUrl === null || sessionCookie === null || githubLogin === null ? test.skip : test;
const cdnBaseUrl = trimTrailingSlash(process.env.REGISTRY_INDEX_E2E_CDN_BASE_URL) ?? "https://cdn.pkgs.ml";
const indexBasePath = trimSlashes(process.env.REGISTRY_INDEX_E2E_BASE_PATH) ?? "index/v1";

describe("riot package registry live e2e", () => {
  liveTest("root route returns service metadata", async () => {
    const response = await fetch(`${baseUrl}/`);
    expect(response.status).toBe(200);

    const payload = (await response.json()) as Record<string, unknown>;
    expect(payload.service).toBe("riot-package-registry");
    expect(payload.routes).toEqual({
      resolve: "/package/<locator>/-/resolve?ref=<selector>",
      manifest: "/package/<locator>/-/manifest/<sha>.json",
      source: "/package/<locator>/-/source/<sha>.tar.gz",
      publish: "/package/<locator>/-/publish?ref=<selector>",
      auth_github_start: "/auth/github/start?return_to=<url>",
      auth_github_callback: "/auth/github/callback?code=<code>&state=<state>",
      auth_logout: "/auth/logout",
      me: "/api/v1/me",
      tokens: "/api/v1/me/tokens",
      search: "/api/v1/search?q=<query>",
    });
  });

  liveAuthTest("github auth start redirects to GitHub authorize", async () => {
    const response = await fetch(
      `${baseUrl}/auth/github/start?return_to=${encodeURIComponent("https://pkgs.ml/login")}`,
      {
        redirect: "manual",
      },
    );

    expect(response.status).toBe(302);
    const location = response.headers.get("location");
    expect(location).not.toBeNull();

    const redirectUrl = new URL(location ?? "");
    expect(redirectUrl.origin).toBe("https://github.com");
    expect(redirectUrl.pathname).toBe("/login/oauth/authorize");
    expect(redirectUrl.searchParams.get("client_id")).not.toBeNull();
    expect(redirectUrl.searchParams.get("redirect_uri")).toBe(
      `${baseUrl}/auth/github/callback`,
    );
    expect(redirectUrl.searchParams.get("state")).not.toBeNull();
  });

  liveAuthTest("anonymous session and token routes behave as expected", async () => {
    const meResponse = await fetch(`${baseUrl}/api/v1/me`, {
      headers: {
        accept: "application/json",
      },
    });

    expect(meResponse.status).toBe(200);
    const mePayload = (await meResponse.json()) as Record<string, unknown>;
    expect(mePayload).toEqual({
      authenticated: false,
    });

    const tokenListResponse = await fetch(`${baseUrl}/api/v1/me/tokens`, {
      headers: {
        accept: "application/json",
      },
    });

    expect(tokenListResponse.status).toBe(401);
    expect(await tokenListResponse.json()).toMatchObject({
      error: "unauthorized",
    });
  });

  liveTest("search miss returns an empty result set", async () => {
    const query = `definitely-not-a-package-${Date.now()}`;
    const response = await fetch(`${baseUrl}/api/v1/search?q=${encodeURIComponent(query)}`, {
      headers: {
        accept: "application/json",
      },
    });

    expect(response.status).toBe(200);
    const payload = (await response.json()) as Record<string, unknown>;
    expect(payload).toEqual({
      query,
      count: 0,
      results: [],
    });
  });

  liveAuthenticatedTest("authenticated session can list and manage publish tokens", async () => {
    const tokenName = `e2e-${Date.now()}`;

    const meResponse = await fetch(`${baseUrl}/api/v1/me`, {
      headers: authenticatedHeaders(),
    });
    expect(meResponse.status).toBe(200);
    expect(await meResponse.json()).toMatchObject({
      authenticated: true,
      user: {
        github_login: githubLogin,
      },
    });

    const createResponse = await fetch(`${baseUrl}/api/v1/me/tokens`, {
      method: "POST",
      headers: {
        ...authenticatedHeaders(),
        "content-type": "application/json",
      },
      body: JSON.stringify({ name: tokenName }),
    });
    expect(createResponse.status).toBe(201);
    const created = (await createResponse.json()) as {
      plaintext_token: string;
      token: { token_id: string; name: string };
    };
    expect(created.plaintext_token.startsWith("rpk_")).toBe(true);
    expect(created.token.name).toBe(tokenName);

    const listResponse = await fetch(`${baseUrl}/api/v1/me/tokens`, {
      headers: authenticatedHeaders(),
    });
    expect(listResponse.status).toBe(200);
    const listed = (await listResponse.json()) as {
      user: { github_login: string };
      tokens: Array<{ token_id: string; name: string }>;
    };
    expect(listed.user.github_login).toBe(githubLogin ?? "");
    expect(listed.tokens.some((token) => token.token_id === created.token.token_id)).toBe(true);

    const revokeResponse = await fetch(
      `${baseUrl}/api/v1/me/tokens/${encodeURIComponent(created.token.token_id)}`,
      {
        method: "DELETE",
        headers: authenticatedHeaders(),
      },
    );
    expect(revokeResponse.status).toBe(200);
    expect(await revokeResponse.json()).toMatchObject({
      token: {
        token_id: created.token.token_id,
        revoked_at: expect.any(String),
      },
    });
  });

  liveTest("resolve returns a concrete source materialization", async () => {
    const publication = await resolvePublication();

    expect(publication.package).toBe(packageLocator);
    expect(publication.source_url).toBe("https://github.com/leostera/riot-new");
    expect(publication.package_subdir).toBe("packages/kernel");
    expect(publication.selector).toBe(selector);
    expect(publication.resolved_sha).toMatch(/^[0-9a-f]{40}$/);
    expect(publication.manifest.url).toContain(`/package/${packageLocator}/-/manifest/`);
    expect(publication.source_archive.url).toContain(`/package/${packageLocator}/-/source/`);
  });

  liveTest("manifest route returns immutable source metadata", async () => {
    const publication = await resolvePublication();

    const response = await fetch(publication.manifest.url);
    expect(response.status).toBe(200);
    expect(response.headers.get("content-type")).toContain("application/json");

    const manifest = (await response.json()) as Record<string, unknown>;
    expect(manifest.package_locator).toBe(packageLocator);
    expect(manifest.source_url).toBe("https://github.com/leostera/riot-new");
    expect(manifest.package_subdir).toBe("packages/kernel");
    expect(manifest.resolved_sha).toBe(publication.resolved_sha);
    expect(typeof manifest.package_name).toBe("string");
    expect(typeof manifest.package_version).toBe("string");
  });

  liveTest("source route redirects to the immutable CDN object", async () => {
    const publication = await resolvePublication();

    const redirectResponse = await fetch(publication.source_archive.url, {
      redirect: "manual",
    });

    expect(redirectResponse.status).toBe(307);
    expect(redirectResponse.headers.get("location")).toBe(publication.source_archive.cdn_url);

    const cdnResponse = await fetch(publication.source_archive.cdn_url);
    expect(cdnResponse.status).toBe(200);
  });

  livePublishTest("publish returns a named release for the source package", async () => {
    const publication = await publishPackage();

    expect(publication.package).toBe(publishPackageLocator);
    expect(publication.source_url).toMatch(/^https:\/\/github\.com\/[^/]+\/[^/]+$/);
    expect(typeof publication.package_subdir).toBe("string");
    expect(publication.selector).toBe(selector);
    expect(publication.resolved_sha).toMatch(/^[0-9a-f]{40}$/);
    expect(typeof publication.package_name).toBe("string");
    expect(typeof publication.package_version).toBe("string");
    expect(publication.claim.key).toBe(`claims/${publication.package_name}.json`);
    expect(publication.release.key).toBe(
      `releases/${publication.package_name}/${publication.package_version}.json`,
    );
    expect(typeof publication.claim.created).toBe("boolean");
    expect(typeof publication.release.created).toBe("boolean");
    expect(publication.manifest.url).toContain(`/package/${publishPackageLocator}/-/manifest/`);
    expect(publication.source_archive.url).toContain(`/package/${publishPackageLocator}/-/source/`);

    const config = await pollJson<Record<string, unknown>>(
      `${cdnBaseUrl}/${indexBasePath}/config.json`,
      (value) => value !== null && value.kind === "sparse",
    );
    expect(config.index_base_url).toBe(`${cdnBaseUrl}/${indexBasePath}`);

    const packageDocument = await pollJson<Record<string, unknown>>(
      `${cdnBaseUrl}/${packageIndexKey(publication.package_name)}`,
      (value) => {
        if (value === null || value.name !== publication.package_name || !Array.isArray(value.releases)) {
          return false;
        }

        return value.releases.some(
          (release) =>
            release !== null &&
            typeof release === "object" &&
            "version" in release &&
            "sha" in release &&
            release.version === publication.package_version &&
            release.sha === publication.resolved_sha,
        );
      },
    );

    expect(packageDocument.latest).toBe(publication.package_version);
  });

  livePublishTest("published package exposes a complete sparse-index install fast path", async () => {
    const publication = await publishPackage();

    const config = await pollJson<IndexConfigPayload>(
      `${cdnBaseUrl}/${indexBasePath}/config.json`,
      (value) => value !== null && value.kind === "sparse",
    );

    expect(config).toEqual({
      schema_version: 1,
      kind: "sparse",
      package_path_strategy: "cargo-lowercase-v1",
      index_base_url: `${cdnBaseUrl}/${indexBasePath}`,
      artifact_base_url: cdnBaseUrl,
    });

    const packageDocumentUrl = `${cdnBaseUrl}/${packageIndexKey(publication.package_name)}`;
    const packageDocument = await pollJson<PackageIndexDocumentPayload>(
      packageDocumentUrl,
      (value) =>
        value !== null &&
        value.name === publication.package_name &&
        Array.isArray(value.releases) &&
        value.releases.some(
          (release) =>
            release.version === publication.package_version &&
            release.sha === publication.resolved_sha,
        ),
    );

    expect(packageDocument.name).toBe(publication.package_name);
    expect(packageDocument.latest).toBe(publication.package_version);

    const indexedRelease = packageDocument.releases.find(
      (release) =>
        release.version === publication.package_version &&
        release.sha === publication.resolved_sha,
    );

    expect(indexedRelease).toBeDefined();
    expect(indexedRelease?.canonical_locator).toBe(publication.package);
    expect(indexedRelease?.repo_url).toBe(publication.source_url);
    expect(indexedRelease?.subdir).toBe(publication.package_subdir);

    const manifestResponse = await fetch(`${cdnBaseUrl}/${indexedRelease!.manifest_key}`);
    expect(manifestResponse.status).toBe(200);
    expect(manifestResponse.headers.get("content-type")).toContain("application/json");

    const manifest = (await manifestResponse.json()) as Record<string, unknown>;
    expect(manifest.package_locator).toBe(publication.package);
    expect(manifest.package_name).toBe(publication.package_name);
    expect(manifest.package_version).toBe(publication.package_version);
    expect(manifest.resolved_sha).toBe(publication.resolved_sha);

    const sourceResponse = await fetch(`${cdnBaseUrl}/${indexedRelease!.source_key}`);
    expect(sourceResponse.status).toBe(200);
  });

  livePublishTest("published package is immediately searchable through the registry api", async () => {
    const publication = await publishPackage();

    const searchResponse = await pollJson<SearchResponsePayload>(
      `${baseUrl}/api/v1/search?q=${encodeURIComponent(publication.package_name)}`,
      (value) =>
        value !== null &&
        Array.isArray(value.results) &&
        value.results.some(
          (result) =>
            result.package_name === publication.package_name &&
            result.latest_version === publication.package_version &&
            result.canonical_locator === publication.package &&
            result.repo_owner.length > 0 &&
            result.repo_name.length > 0,
        ),
    );

    expect(searchResponse.query).toBe(publication.package_name);
    expect(searchResponse.count).toBeGreaterThanOrEqual(1);
    expect(searchResponse.results[0]?.package_name).toBe(publication.package_name);
    expect(searchResponse.results[0]?.latest_version).toBe(publication.package_version);
    expect(searchResponse.results[0]?.canonical_locator).toBe(publication.package);
  });
});

async function resolvePublication(): Promise<ResolvePayload> {
  if (baseUrl === null) {
    throw new Error("REGISTRY_E2E_BASE_URL must be set to resolve live publications.");
  }

  const response = await fetch(
    `${baseUrl}/package/${packageLocator}/-/resolve?ref=${encodeURIComponent(selector)}`,
  );

  expect(response.status).toBe(200);
  return (await response.json()) as ResolvePayload;
}

async function publishPackage(): Promise<PublishPayload> {
  if (baseUrl === null || rootAuthToken === null) {
    throw new Error(
      "REGISTRY_E2E_BASE_URL and REGISTRY_E2E_ROOT_AUTH_TOKEN must be set to publish live packages.",
    );
  }

  const response = await fetch(
    `${baseUrl}/package/${publishPackageLocator}/-/publish?ref=${encodeURIComponent(selector)}`,
    {
      method: "POST",
      headers: {
        authorization: `Bearer ${rootAuthToken}`,
      },
    },
  );

  expect(response.status).toBe(200);
  return (await response.json()) as PublishPayload;
}

function trimTrailingSlash(value: string | undefined): string | null {
  if (value === undefined || value.length === 0) {
    return null;
  }

  return value.endsWith("/") ? value.slice(0, -1) : value;
}

function trimSlashes(value: string | undefined): string | null {
  if (value === undefined || value.length === 0) {
    return null;
  }

  return value.replace(/^\/+|\/+$/g, "");
}

function packageIndexKey(packageName: string): string {
  const normalized = packageName.toLowerCase();

  if (normalized.length === 1) {
    return `${indexBasePath}/1/${normalized}.json`;
  }

  if (normalized.length === 2) {
    return `${indexBasePath}/2/${normalized}.json`;
  }

  if (normalized.length === 3) {
    return `${indexBasePath}/3/${normalized[0]}/${normalized}.json`;
  }

  return `${indexBasePath}/${normalized.slice(0, 2)}/${normalized.slice(2, 4)}/${normalized}.json`;
}

async function pollJson<T>(
  url: string,
  accept: (value: T | null) => boolean,
  timeoutMs = 15000,
  intervalMs = 500,
): Promise<T> {
  const deadline = Date.now() + timeoutMs;
  let lastStatus = 0;

  while (Date.now() <= deadline) {
    const response = await fetch(url);
    lastStatus = response.status;

    if (response.status === 200) {
      const payload = (await response.json()) as T;
      if (accept(payload)) {
        return payload;
      }
    } else if (response.status !== 404) {
      throw new Error(`Unexpected status ${response.status} while polling ${url}.`);
    }

    await Bun.sleep(intervalMs);
  }

  throw new Error(`Timed out waiting for indexed package at ${url}. Last status was ${lastStatus}.`);
}

interface ResolvePayload {
  package: string;
  source_url: string;
  package_subdir: string;
  selector: string;
  resolved_sha: string;
  manifest: {
    url: string;
    cdn_url: string;
  };
  source_archive: {
    url: string;
    cdn_url: string;
  };
}

interface PublishPayload extends ResolvePayload {
  package_name: string;
  package_version: string;
  claim: {
    key: string;
    created: boolean;
  };
  release: {
    key: string;
    created: boolean;
  };
  materialization: {
    manifest: boolean;
    source: boolean;
  };
}

interface IndexConfigPayload {
  schema_version: number;
  kind: string;
  package_path_strategy: string;
  index_base_url: string;
  artifact_base_url: string;
}

interface PackageIndexDocumentPayload {
  schema_version: number;
  name: string;
  latest: string;
  updated_at: string;
  releases: PackageIndexReleasePayload[];
}

interface PackageIndexReleasePayload {
  version: string;
  published_at: string;
  canonical_locator: string;
  repo_url: string;
  subdir: string;
  sha: string;
  manifest_key: string;
  source_key: string;
  dependencies: unknown[];
}

interface SearchResultPayload {
  package_name: string;
  latest_version: string;
  canonical_locator: string;
  repo_owner: string;
  repo_name: string;
}

interface SearchResponsePayload {
  query: string;
  count: number;
  results: SearchResultPayload[];
}

function authenticatedHeaders(): Record<string, string> {
  return {
    accept: "application/json",
    cookie: sessionCookie ?? "",
  };
}
