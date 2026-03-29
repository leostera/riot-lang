import { describe, expect, test } from "bun:test";

import { indexPublishedRelease } from "../src/indexing.ts";
import { handleRequest } from "../src/routes.ts";
import type { PackagePublicationManifest, PublishedReleaseRecord } from "../src/types.ts";
import { FakeExecutionContext, makeEnv } from "./helpers.ts";

describe("registry search api", () => {
  test("search route returns metadata when q is absent", async () => {
    const { env } = makeEnv();
    const response = await handleRequest(
      new Request("https://registry.test/api/v1/search"),
      env,
      new FakeExecutionContext(),
    );

    expect(response.status).toBe(200);
    const payload = (await response.json()) as Record<string, unknown>;
    expect(payload).toEqual({
      service: "riot-package-registry",
      route: "/api/v1/search?q=<query>",
      source: {
        package_index_base_url: "https://cdn.pkgs.ml/index/v1",
        updated_during_publish: true,
      },
    });
  });

  test("indexed packages are searchable by exact name and provenance", async () => {
    const { env } = makeEnv();
    await indexPublishedRelease(env, makeReleaseRecord(), makeManifest());

    const exact = await handleRequest(
      new Request("https://registry.test/api/v1/search?q=kernel"),
      env,
      new FakeExecutionContext(),
    );
    expect(exact.status).toBe(200);
    expect(await exact.json()).toMatchObject({
      query: "kernel",
      count: 1,
      results: [
        {
          package_name: "kernel",
          latest_version: "0.0.1",
          description: "Actor runtime kernel primitives for Riot",
          repo_owner: "leostera",
          repo_name: "riot-new",
          subdir: "packages/kernel",
          release_count: 1,
        },
      ],
    });

    const provenance = await handleRequest(
      new Request("https://registry.test/api/v1/search?q=leostera"),
      env,
      new FakeExecutionContext(),
    );
    expect(provenance.status).toBe(200);
    expect(await provenance.json()).toMatchObject({
      count: 1,
      results: [{ package_name: "kernel" }],
    });
  });

  test("search miss returns an empty result set instead of throwing", async () => {
    const { env } = makeEnv();
    await indexPublishedRelease(env, makeReleaseRecord(), makeManifest());

    const response = await handleRequest(
      new Request("https://registry.test/api/v1/search?q=definitely-not-a-package-12345"),
      env,
      new FakeExecutionContext(),
    );

    expect(response.status).toBe(200);
    const payload = (await response.json()) as Record<string, unknown>;
    expect(payload).toEqual({
      query: "definitely-not-a-package-12345",
      count: 0,
      results: [],
    });
  });
});

function makeReleaseRecord(
  overrides: Partial<PublishedReleaseRecord> = {},
): PublishedReleaseRecord {
  return {
    package_name: "kernel",
    package_version: "0.0.1",
    package_locator: "github.com/leostera/riot-new/packages/kernel",
    source_url: "https://github.com/leostera/riot-new",
    package_subdir: "packages/kernel",
    selector: "main",
    resolved_sha: "2aef0372bf5b6687db05bda80cde55f960cbfd9d",
    package_description: "Actor runtime kernel primitives for Riot",
    package_license: "Apache-2.0",
    package_homepage: "https://riot.ml",
    package_repository: "https://github.com/leostera/riot-new",
    package_root_module: "Kernel",
    dependencies: [{ name: "std", path: "../std" }],
    source_archive_key: "sources/github.com/leostera/riot-new/2aef0372bf5b6687db05bda80cde55f960cbfd9d.tar.gz",
    manifest_key:
      "packages/github.com/leostera/riot-new/packages/kernel/2aef0372bf5b6687db05bda80cde55f960cbfd9d.manifest.json",
    published_at: "2026-03-27T15:27:35Z",
    ...overrides,
  };
}

function makeManifest(
  overrides: Partial<PackagePublicationManifest> = {},
): PackagePublicationManifest {
  const release = makeReleaseRecord();

  return {
    package_locator: release.package_locator,
    source_url: release.source_url,
    package_subdir: release.package_subdir,
    selector: release.selector,
    resolved_sha: release.resolved_sha,
    package_name: release.package_name,
    package_version: release.package_version,
    package_public: true,
    package_description: release.package_description,
    package_license: release.package_license,
    package_homepage: release.package_homepage,
    package_repository: release.package_repository,
    package_root_module: release.package_root_module,
    dependencies: release.dependencies,
    source_archive_key: release.source_archive_key,
    manifest_key: release.manifest_key,
    materialized_at: "2026-03-27T15:20:00Z",
    ...overrides,
  };
}
