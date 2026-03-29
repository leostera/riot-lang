import { describe, expect, test } from "bun:test";

const pkgsBaseUrl = trimTrailingSlash(process.env.PKGS_E2E_BASE_URL);
const registryBaseUrl =
  trimTrailingSlash(process.env.REGISTRY_E2E_BASE_URL) ??
  trimTrailingSlash(process.env.PUBLIC_REGISTRY_BASE_URL) ??
  "https://registry.pkgs.ml";
const publishPackageLocator =
  process.env.PKGS_E2E_PUBLISH_PACKAGE_LOCATOR ??
  process.env.REGISTRY_E2E_PUBLISH_PACKAGE_LOCATOR ??
  "github.com/leostera/riot-new/packages/kernel";
const selector = process.env.REGISTRY_E2E_SELECTOR ?? "main";
const searchApiBaseUrl =
  trimTrailingSlash(process.env.PKGS_E2E_SEARCH_API_BASE_URL) ?? `${registryBaseUrl}/api/v1/search`;
const cdnBaseUrl = trimTrailingSlash(process.env.PUBLIC_CDN_BASE_URL) ?? "https://cdn.pkgs.ml";
const indexBasePath = trimSlashes(process.env.PUBLIC_INDEX_BASE_PATH) ?? "index/v1";
const rootAuthToken = process.env.REGISTRY_E2E_ROOT_AUTH_TOKEN ?? null;
const sessionCookie = process.env.PKGS_E2E_SESSION_COOKIE ?? process.env.REGISTRY_E2E_SESSION_COOKIE ?? null;
const githubLogin = process.env.PKGS_E2E_GITHUB_LOGIN ?? process.env.REGISTRY_E2E_GITHUB_LOGIN ?? null;

const liveTest = pkgsBaseUrl === null ? test.skip : test;
const livePublishTest = pkgsBaseUrl === null || rootAuthToken === null ? test.skip : test;
const liveAuthenticatedTest =
  pkgsBaseUrl === null || sessionCookie === null || githubLogin === null ? test.skip : test;

describe("pkgs.ml live e2e", () => {
  liveTest("landing page renders the search-first registry UI", async () => {
    const response = await fetch(`${pkgsBaseUrl}/`);
    expect(response.status).toBe(200);

    const html = await response.text();
    expect(html).toContain("pkgs.ml");
    expect(html).toContain("community package registry");
    expect(html).toContain('name="q"');
    expect(html).toContain("Login with GitHub");
    expect(html).toContain(
      `${registryBaseUrl}/auth/github/start?return_to=${encodeURIComponent(`${pkgsBaseUrl}/`)}`,
    );
  });

  liveTest("login page links to the registry github auth start route", async () => {
    const returnTo = `${pkgsBaseUrl}/u/leostera/tokens`;
    const response = await fetch(
      `${pkgsBaseUrl}/login?return_to=${encodeURIComponent(returnTo)}`,
    );
    expect(response.status).toBe(200);

    const html = await response.text();
    expect(html).toContain("Continue with GitHub");
    expect(html).toContain(
      `${registryBaseUrl}/auth/github/start?return_to=${encodeURIComponent(returnTo)}`,
    );
  });

  liveTest("unauthenticated token page redirects to login", async () => {
    const response = await fetch(`${pkgsBaseUrl}/u/leostera/tokens`, {
      redirect: "manual",
    });

    expect(response.status).toBe(302);
    const location = response.headers.get("location");
    expect(location).not.toBeNull();
    expect(location).toContain("/login?return_to=");
  });

  liveTest("search miss shows the empty-state copy", async () => {
    const query = `definitely-not-a-package-${Date.now()}`;
    const response = await fetch(`${pkgsBaseUrl}/?q=${encodeURIComponent(query)}`);

    expect(response.status).toBe(200);
    const html = await response.text();
    expect(html).toContain("No packages matched");
    expect(html).toContain(query);
  });

  liveTest("search results page lists all packages returned by the search api", async () => {
    const query = "leostera";
    const searchApi = await fetchSearchApi(query);
    expect(searchApi.results.length).toBeGreaterThan(1);

    const html = await pollText(
      `${pkgsBaseUrl}/?q=${encodeURIComponent(query)}`,
      (page) =>
        page.includes("Search OCaml packages") &&
        searchApi.results.every((result) => page.includes(result.package_name)),
    );

    expect(html).toContain(`${searchApi.count} results`);
    for (const result of searchApi.results) {
      expect(html).toContain(result.package_name);
      expect(html).toContain(`v${result.latest_version}`);
    }
  });

  livePublishTest("published package appears across search, package, and owner pages", async () => {
    const publication = await publishPackage();
    const owner = ownerFromLocator(publication.package);

    const searchHtml = await pollText(
      `${pkgsBaseUrl}/?q=${encodeURIComponent(publication.package_name)}`,
      (html) =>
        html.includes("Search OCaml packages") &&
        html.includes(publication.package_name) &&
        html.includes(`v${publication.package_version}`),
    );
    expect(searchHtml).toContain(publication.package_name);
    expect(searchHtml).toContain(`v${publication.package_version}`);

    const packageHtml = await pollText(
      `${pkgsBaseUrl}/p/${encodeURIComponent(publication.package_name)}`,
      (html) =>
        html.includes(publication.package_name) &&
        html.includes(`v${publication.package_version}`) &&
        html.includes(`tusk add ${publication.package_name}`),
    );
    expect(packageHtml).toContain("All packages");
    expect(packageHtml).toContain(owner);
    expect(packageHtml).toContain(`tusk add ${publication.package_name}`);

    const versionHtml = await pollText(
      `${pkgsBaseUrl}/p/${encodeURIComponent(publication.package_name)}/${encodeURIComponent(publication.package_version)}`,
      (html) =>
        html.includes(publication.package_name) && html.includes(`v${publication.package_version}`),
    );
    expect(versionHtml).toContain(publication.package_name);
    expect(versionHtml).toContain(`v${publication.package_version}`);

    const ownerHtml = await pollText(
      `${pkgsBaseUrl}/u/${encodeURIComponent(owner)}`,
      (html) =>
        html.includes(`@${owner}`) &&
        html.includes("Recently published packages") &&
        html.includes(publication.package_name),
    );
    expect(ownerHtml).toContain(publication.package_name);
  });

  livePublishTest("package page shows every indexed version for the published package", async () => {
    const publication = await publishPackage();
    const document = await fetchPackageIndexDocument(publication.package_name);

    const packageHtml = await pollText(
      `${pkgsBaseUrl}/p/${encodeURIComponent(publication.package_name)}`,
      (html) =>
        html.includes("Versions") &&
        document.releases.every((release) => html.includes(`v${release.version}`)),
    );

    expect(packageHtml).toContain("Versions");
    for (const release of document.releases) {
      expect(packageHtml).toContain(`v${release.version}`);
      expect(packageHtml).toContain(
        `/p/${encodeURIComponent(publication.package_name)}/${encodeURIComponent(release.version)}`,
      );
    }
  });

  livePublishTest("every indexed package version has an accessible version page", async () => {
    const publication = await publishPackage();
    const document = await fetchPackageIndexDocument(publication.package_name);

    for (const release of document.releases) {
      const html = await pollText(
        `${pkgsBaseUrl}/p/${encodeURIComponent(publication.package_name)}/${encodeURIComponent(release.version)}`,
        (page) => page.includes(`v${release.version}`) && page.includes(publication.package_name),
      );

      expect(html).toContain(publication.package_name);
      expect(html).toContain(`v${release.version}`);
    }
  });

  liveTest("owner page lists recently published packages for the owner", async () => {
    const owner = "leostera";
    const packages = await fetchSearchApi(owner);
    const ownerPackages = packages.results.filter(
      (result) => result.repo_owner.toLowerCase() === owner,
    );

    expect(ownerPackages.length).toBeGreaterThan(0);

    const html = await pollText(
      `${pkgsBaseUrl}/u/${encodeURIComponent(owner)}`,
      (page) =>
        page.includes("Recently published packages") &&
        ownerPackages.every((result) => page.includes(result.package_name)),
    );

    expect(html).toContain(`@${owner}`);
    expect(html).toContain("Recently published packages");
    for (const result of ownerPackages) {
      expect(html).toContain(result.package_name);
    }
  });

  liveAuthenticatedTest("authenticated owner page shows the api tokens section", async () => {
    const response = await fetch(`${pkgsBaseUrl}/u/${encodeURIComponent(githubLogin ?? "")}`, {
      headers: {
        cookie: sessionCookie ?? "",
      },
    });

    expect(response.status).toBe(200);
    const html = await response.text();
    expect(html).toContain("API Tokens");
    expect(html).toContain("Manage API tokens");
  });

  liveAuthenticatedTest("token page shows a new publish token only once", async () => {
    const tokenName = `pkgs-e2e-${Date.now()}`;
    const createResponse = await fetch(
      `${pkgsBaseUrl}/u/${encodeURIComponent(githubLogin ?? "")}/tokens`,
      {
        method: "POST",
        headers: {
          "content-type": "application/x-www-form-urlencoded",
          cookie: sessionCookie ?? "",
        },
        body: new URLSearchParams({
          action: "create",
          name: tokenName,
        }).toString(),
      },
    );

    expect(createResponse.status).toBe(200);
    const createHtml = await createResponse.text();
    expect(createHtml).toContain("New publish token created");
    expect(createHtml).toContain(tokenName);

    const plaintextMatch = createHtml.match(/rpk_[A-Za-z0-9_-]+/);
    expect(plaintextMatch).not.toBeNull();
    const plaintextToken = plaintextMatch?.[0] ?? "";
    expect(plaintextToken.length).toBeGreaterThan(0);

    const tokenId = extractTokenIdForName(createHtml, tokenName);
    expect(tokenId).not.toBeNull();

    const revisitResponse = await fetch(
      `${pkgsBaseUrl}/u/${encodeURIComponent(githubLogin ?? "")}/tokens`,
      {
        headers: {
          cookie: sessionCookie ?? "",
        },
      },
    );
    expect(revisitResponse.status).toBe(200);
    const revisitHtml = await revisitResponse.text();
    expect(revisitHtml).toContain("Publish credentials");
    expect(revisitHtml).not.toContain("New publish token created");
    expect(revisitHtml).not.toContain(plaintextToken);

    const revokeResponse = await fetch(
      `${registryBaseUrl}/api/v1/me/tokens/${encodeURIComponent(tokenId ?? "")}`,
      {
        method: "DELETE",
        headers: {
          accept: "application/json",
          cookie: sessionCookie ?? "",
        },
      },
    );
    expect(revokeResponse.status).toBe(200);
  });
});

async function publishPackage(): Promise<PublishPayload> {
  const response = await fetch(
    `${registryBaseUrl}/package/${publishPackageLocator}/-/publish?ref=${encodeURIComponent(selector)}`,
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

async function pollText(
  url: string,
  accept: (html: string) => boolean,
  timeoutMs = 15_000,
  intervalMs = 500,
): Promise<string> {
  const deadline = Date.now() + timeoutMs;
  let lastStatus = 0;

  while (Date.now() <= deadline) {
    const response = await fetch(url, {
      headers: sessionCookie === null ? undefined : { cookie: sessionCookie },
    });
    lastStatus = response.status;

    if (response.status === 200) {
      const html = await response.text();
      if (accept(html)) {
        return html;
      }
    } else if (response.status !== 404) {
      throw new Error(`Unexpected status ${response.status} while polling ${url}.`);
    }

    await Bun.sleep(intervalMs);
  }

  throw new Error(`Timed out waiting for HTML at ${url}. Last status was ${lastStatus}.`);
}

function trimTrailingSlash(value: string | undefined): string | null {
  if (value === undefined || value.length === 0) {
    return null;
  }

  return value.endsWith("/") ? value.slice(0, -1) : value;
}

function ownerFromLocator(locator: string): string {
  return locator.split("/")[1] ?? "unknown";
}

function extractTokenIdForName(html: string, tokenName: string): string | null {
  const escapedName = escapeRegex(tokenName);
  const match = html.match(
    new RegExp(`${escapedName}[\\s\\S]*?name="token_id" value="([^"]+)"`, "i"),
  );

  return match?.[1] ?? null;
}

function escapeRegex(value: string): string {
  return value.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
}

interface PublishPayload {
  package: string;
  package_name: string;
  package_version: string;
}

interface SearchApiResponse {
  query: string;
  count: number;
  results: Array<{
    package_name: string;
    latest_version: string;
    repo_owner: string;
  }>;
}

interface PackageIndexDocument {
  name: string;
  releases: Array<{
    version: string;
  }>;
}

async function fetchSearchApi(query: string): Promise<SearchApiResponse> {
  const response = await fetch(`${searchApiBaseUrl}?q=${encodeURIComponent(query)}&limit=100`, {
    headers: {
      accept: "application/json",
    },
  });

  expect(response.status).toBe(200);
  return (await response.json()) as SearchApiResponse;
}

async function fetchPackageIndexDocument(packageName: string): Promise<PackageIndexDocument> {
  const response = await fetch(`${cdnBaseUrl}/${packageIndexKey(packageName)}`, {
    headers: {
      accept: "application/json",
    },
  });

  expect(response.status).toBe(200);
  return (await response.json()) as PackageIndexDocument;
}

function packageIndexKey(packageName: string): string {
  const normalized = packageName.trim().toLowerCase();

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

function trimSlashes(value: string | undefined): string | null {
  if (value === undefined || value.length === 0) {
    return null;
  }

  return value.replace(/^\/+|\/+$/g, "");
}
