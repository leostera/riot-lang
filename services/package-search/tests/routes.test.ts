import { describe, expect, test } from "bun:test";

import { handleRequest } from "../src/routes.ts";
import { packageIndexKey } from "../src/storage.ts";
import type { PackageIndexedEvent, PackageIndexDocument } from "../src/types.ts";
import { consumeIndexed, makeEnv, putPackageIndexDocument } from "./helpers.ts";

describe("riot package search routes", () => {
  test("root route returns service metadata when q is absent", async () => {
    const { env } = makeEnv();

    const response = await handleRequest(new Request("https://search.test/"), env);
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

  test("search returns one package result for exact and fuzzy-ish matches", async () => {
    const { env, bucket } = makeEnv();
    const document = makePackageIndexDocument();
    await putPackageIndexDocument(bucket, packageIndexKey({ cdnBaseUrl: "https://cdn.pkgs.ml", indexBasePath: "index/v1" }, "kernel"), document);
    await consumeIndexed(env, makeIndexedEvent());

    const exact = await handleRequest(new Request("https://search.test/?q=kernel"), env);
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

    const provenance = await handleRequest(new Request("https://search.test/?q=leostera"), env);
    expect(provenance.status).toBe(200);
    expect(await provenance.json()).toMatchObject({
      count: 1,
      results: [{ package_name: "kernel" }],
    });
  });
});

function makeIndexedEvent(): PackageIndexedEvent {
  return {
    type: "package.indexed",
    package_name: "kernel",
    package_version: "0.0.1",
    package_locator: "github.com/leostera/riot-new/packages/kernel",
    resolved_sha: "2aef0372bf5b6687db05bda80cde55f960cbfd9d",
    package_index_key: "index/v1/ke/rn/kernel.json",
    package_index_url: "https://cdn.pkgs.ml/index/v1/ke/rn/kernel.json",
    latest: "0.0.1",
    indexed_at: "2026-03-27T17:40:00Z",
  };
}

function makePackageIndexDocument(): PackageIndexDocument {
  return {
    schema_version: 1,
    name: "kernel",
    latest: "0.0.1",
    updated_at: "2026-03-27T17:40:00Z",
    releases: [
      {
        version: "0.0.1",
        published_at: "2026-03-27T17:40:00Z",
        canonical_locator: "github.com/leostera/riot-new/packages/kernel",
        repo_url: "https://github.com/leostera/riot-new",
        subdir: "packages/kernel",
        sha: "2aef0372bf5b6687db05bda80cde55f960cbfd9d",
        description: "Actor runtime kernel primitives for Riot",
        license: "Apache-2.0",
        homepage: "https://riot.ml",
        repository: "https://github.com/leostera/riot-new",
        root_module: "Kernel",
        manifest_key:
          "packages/github.com/leostera/riot-new/packages/kernel/2aef0372bf5b6687db05bda80cde55f960cbfd9d.manifest.json",
        source_key:
          "sources/github.com/leostera/riot-new/2aef0372bf5b6687db05bda80cde55f960cbfd9d.tar.gz",
        dependencies: [{ name: "std", path: "../std" }],
      },
    ],
  };
}
