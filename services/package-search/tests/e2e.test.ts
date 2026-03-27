import { describe, expect, test } from "bun:test";

const searchBaseUrl = trimTrailingSlash(process.env.SEARCH_E2E_BASE_URL);
const registryBaseUrl = trimTrailingSlash(process.env.REGISTRY_E2E_BASE_URL);
const publishPackageLocator =
  process.env.SEARCH_E2E_PUBLISH_PACKAGE_LOCATOR ??
  process.env.REGISTRY_E2E_PUBLISH_PACKAGE_LOCATOR ??
  null;
const selector = process.env.REGISTRY_E2E_SELECTOR ?? "main";
const rootAuthToken = process.env.REGISTRY_E2E_ROOT_AUTH_TOKEN ?? null;

const liveSearchTest = searchBaseUrl === null ? test.skip : test;
const livePublishSearchTest =
  searchBaseUrl === null ||
  registryBaseUrl === null ||
  rootAuthToken === null ||
  publishPackageLocator === null
    ? test.skip
    : test;

describe("riot package search live e2e", () => {
  liveSearchTest("root route returns service metadata", async () => {
    const response = await fetch(`${searchBaseUrl}/`);
    expect(response.status).toBe(200);

    const payload = (await response.json()) as Record<string, unknown>;
    expect(payload).toEqual({
      service: "riot-package-search",
      route: "/?q=<query>",
      source: {
        package_index_base_url: "https://cdn.pkgs.ml/index/v1",
        queue_consumer: "package.indexed",
      },
    });
  });

  livePublishSearchTest(
    "published package becomes visible through search",
    async () => {
      const publication = await publishPackage();

      const response = await pollJson<SearchResponsePayload>(
        `${searchBaseUrl}/?q=${encodeURIComponent(publication.package_name)}`,
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

      expect(response.query).toBe(publication.package_name);
      expect(response.count).toBeGreaterThanOrEqual(1);
      expect(response.results[0]?.package_name).toBe(publication.package_name);
      expect(response.results[0]?.latest_version).toBe(publication.package_version);
      expect(response.results[0]?.canonical_locator).toBe(publication.package);
    },
    { timeout: 30_000 },
  );
});

async function publishPackage(): Promise<PublishPayload> {
  if (registryBaseUrl === null || rootAuthToken === null) {
    throw new Error(
      "REGISTRY_E2E_BASE_URL and REGISTRY_E2E_ROOT_AUTH_TOKEN must be set to publish live packages.",
    );
  }

  const response = await fetch(
    `${registryBaseUrl}/package/${publishPackageLocator}/-/publish?ref=${encodeURIComponent(selector)}`,
    {
      method: "POST",
      headers: {
        authorization: `Bearer ${rootAuthToken}`,
      },
    },
  );

  if (response.status !== 200) {
    throw new Error(
      `Expected publish to succeed for ${publishPackageLocator}, got ${response.status}: ${await response.text()}`,
    );
  }

  return (await response.json()) as PublishPayload;
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

  throw new Error(`Timed out waiting for searchable package at ${url}. Last status was ${lastStatus}.`);
}

function trimTrailingSlash(value: string | undefined): string | null {
  if (value === undefined || value.length === 0) {
    return null;
  }

  return value.endsWith("/") ? value.slice(0, -1) : value;
}

interface PublishPayload {
  package: string;
  package_name: string;
  package_version: string;
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
