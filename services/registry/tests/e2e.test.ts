import { describe, expect, test } from "bun:test";

const baseUrl = trimTrailingSlash(process.env.REGISTRY_E2E_BASE_URL);
const packageLocator =
  process.env.REGISTRY_E2E_PACKAGE_LOCATOR ?? "github.com/leostera/riot-new/packages/kernel";
const selector = process.env.REGISTRY_E2E_SELECTOR ?? "main";
const liveTest = baseUrl === null ? test.skip : test;

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
    });
  });

  liveTest("resolve returns a concrete publication", async () => {
    const publication = await resolvePublication();

    expect(publication.package).toBe(packageLocator);
    expect(publication.selector).toBe(selector);
    expect(publication.resolved_sha).toMatch(/^[0-9a-f]{40}$/);
    expect(publication.manifest.url).toContain(`/package/${packageLocator}/-/manifest/`);
    expect(publication.source_archive.url).toContain(`/package/${packageLocator}/-/source/`);
  });

  liveTest("manifest route returns immutable publication metadata", async () => {
    const publication = await resolvePublication();

    const response = await fetch(publication.manifest.url);
    expect(response.status).toBe(200);
    expect(response.headers.get("content-type")).toContain("application/json");

    const manifest = (await response.json()) as Record<string, unknown>;
    expect(manifest.package_locator).toBe(packageLocator);
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

function trimTrailingSlash(value: string | undefined): string | null {
  if (value === undefined || value.length === 0) {
    return null;
  }

  return value.endsWith("/") ? value.slice(0, -1) : value;
}

interface ResolvePayload {
  package: string;
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
