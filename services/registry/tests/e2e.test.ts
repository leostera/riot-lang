import { describe, expect, test } from "bun:test";

const baseUrl = trimTrailingSlash(process.env.REGISTRY_E2E_BASE_URL);
const packageLocator =
  process.env.REGISTRY_E2E_PACKAGE_LOCATOR ?? "github.com/leostera/riot-new/packages/kernel";
const selector = process.env.REGISTRY_E2E_SELECTOR ?? "main";
const publishPackageLocator = process.env.REGISTRY_E2E_PUBLISH_PACKAGE_LOCATOR ?? packageLocator;
const liveTest = baseUrl === null ? test.skip : test;
const rootAuthToken = process.env.REGISTRY_E2E_ROOT_AUTH_TOKEN ?? null;
const livePublishTest = baseUrl === null || rootAuthToken === null ? test.skip : test;

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
