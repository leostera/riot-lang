import { describe, expect, test } from "bun:test";

import { searchPackages } from "../src/db.ts";
import { packageIndexKey } from "../src/storage.ts";
import type { PackageIndexedEvent, PackageIndexDocument } from "../src/types.ts";
import { consumeIndexed, makeEnv, putPackageIndexDocument } from "./helpers.ts";

describe("riot package search consumer", () => {
  test("consuming package.indexed upserts a searchable package row", async () => {
    const { env, bucket, db } = makeEnv();
    await putPackageIndexDocument(
      bucket,
      packageIndexKey({ cdnBaseUrl: "https://cdn.pkgs.ml", indexBasePath: "index/v1" }, "kernel"),
      makePackageIndexDocument(),
    );

    await consumeIndexed(env, makeIndexedEvent());

    const results = await searchPackages(db as unknown as D1Database, "kernel", 10, 0);
    expect(results).toHaveLength(1);
    expect(results[0]).toMatchObject({
      package_name: "kernel",
      latest_version: "0.0.1",
      description: "Actor runtime kernel primitives for Riot",
      canonical_locator: "github.com/leostera/riot-new/packages/kernel",
      repo_owner: "leostera",
      repo_name: "riot-new",
      release_count: 1,
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
