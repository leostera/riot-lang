import { describe, expect, test } from "bun:test";

import { getConfig } from "../src/config.ts";
import { indexPublishedRelease } from "../src/indexing.ts";
import { indexConfigKey, packageIndexKey } from "../src/storage.ts";
import type {
  PackageIndexDocument,
  PackagePublicationManifest,
  PublishedReleaseRecord,
} from "../src/types.ts";
import { makeEnv } from "./helpers.ts";

describe("registry indexing", () => {
  test("indexing a published release writes config, package doc, and package.indexed", async () => {
    const { env, bucket, indexedQueue } = makeEnv();
    const release = makeReleaseRecord();
    const manifest = makeManifest(release);

    const result = await indexPublishedRelease(env, release, manifest);

    expect(result).toEqual({
      changed: true,
      latest: "0.0.1",
      indexedAt: "2026-03-27T15:27:35Z",
      packageIndexKey: "index/v1/ke/rn/kernel.json",
      packageIndexUrl: "https://cdn.pkgs.ml/index/v1/ke/rn/kernel.json",
    });

    const config = getConfig(env);
    expect(await bucket.text(indexConfigKey(config))).not.toBeNull();

    const document = JSON.parse(
      (await bucket.text(packageIndexKey(config, "kernel"))) ?? "null",
    ) as PackageIndexDocument;

    expect(document).toEqual({
      schema_version: 1,
      name: "kernel",
      latest: "0.0.1",
      updated_at: "2026-03-27T15:27:35Z",
      releases: [
        {
          version: "0.0.1",
          published_at: "2026-03-27T15:27:35Z",
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
    });

    expect(indexedQueue.messages).toEqual([
      {
        type: "package.indexed",
        package_name: "kernel",
        package_version: "0.0.1",
        package_locator: "github.com/leostera/riot-new/packages/kernel",
        resolved_sha: "2aef0372bf5b6687db05bda80cde55f960cbfd9d",
        package_index_key: "index/v1/ke/rn/kernel.json",
        package_index_url: "https://cdn.pkgs.ml/index/v1/ke/rn/kernel.json",
        latest: "0.0.1",
        indexed_at: "2026-03-27T15:27:35Z",
      },
    ]);
  });

  test("higher versions become latest and releases stay semver-sorted", async () => {
    const { env, bucket, indexedQueue } = makeEnv();
    const first = makeReleaseRecord();
    const second = makeReleaseRecord({
      package_version: "0.2.0",
      resolved_sha: "eeee0372bf5b6687db05bda80cde55f960cbfd9d",
      source_archive_key:
        "sources/github.com/leostera/riot-new/eeee0372bf5b6687db05bda80cde55f960cbfd9d.tar.gz",
      manifest_key:
        "packages/github.com/leostera/riot-new/packages/kernel/eeee0372bf5b6687db05bda80cde55f960cbfd9d.manifest.json",
      published_at: "2026-03-27T16:00:00Z",
    });

    await indexPublishedRelease(env, first, makeManifest(first));
    await indexPublishedRelease(env, second, makeManifest(second));

    const document = JSON.parse(
      (await bucket.text(packageIndexKey(getConfig(env), "kernel"))) ?? "null",
    ) as PackageIndexDocument;

    expect(document.latest).toBe("0.2.0");
    expect(document.updated_at).toBe("2026-03-27T16:00:00Z");
    expect(document.releases.map((release) => release.version)).toEqual(["0.2.0", "0.0.1"]);
    expect(indexedQueue.messages).toHaveLength(2);
  });

  test("reprocessing the same release is idempotent and still emits package.indexed for downstream consumers", async () => {
    const { env, bucket, indexedQueue } = makeEnv();
    const release = makeReleaseRecord();
    const manifest = makeManifest(release);

    const first = await indexPublishedRelease(env, release, manifest);
    const second = await indexPublishedRelease(env, release, manifest);

    const document = JSON.parse(
      (await bucket.text(packageIndexKey(getConfig(env), "kernel"))) ?? "null",
    ) as PackageIndexDocument;

    expect(first.changed).toBe(true);
    expect(second.changed).toBe(false);
    expect(document.releases).toHaveLength(1);
    expect(indexedQueue.messages).toHaveLength(2);
    expect(indexedQueue.messages[1]).toMatchObject({
      type: "package.indexed",
      package_name: "kernel",
      package_version: "0.0.1",
      package_locator: "github.com/leostera/riot-new/packages/kernel",
      resolved_sha: "2aef0372bf5b6687db05bda80cde55f960cbfd9d",
      package_index_key: "index/v1/ke/rn/kernel.json",
      package_index_url: "https://cdn.pkgs.ml/index/v1/ke/rn/kernel.json",
      latest: "0.0.1",
      indexed_at: "2026-03-27T15:27:35Z",
    });
  });

  test("reindexing the same version replaces stale release metadata", async () => {
    const { env, bucket } = makeEnv();
    const release = makeReleaseRecord();
    const manifest = makeManifest(release);

    await indexPublishedRelease(env, release, manifest);

    await bucket.put(
      packageIndexKey(getConfig(env), "kernel"),
      JSON.stringify({
        schema_version: 1,
        name: "kernel",
        latest: "0.0.1",
        updated_at: "2026-03-27T15:27:35Z",
        releases: [
          {
            version: "0.0.1",
            published_at: "2026-03-27T15:27:35Z",
            canonical_locator: "github.com/leostera/riot-new/packages/kernel",
            repo_url: "https://github.com/leostera/riot-new",
            subdir: "packages/kernel",
            sha: "different",
            description: "Old description",
            license: "MIT",
            manifest_key: manifest.manifest_key,
            source_key: release.source_archive_key,
            dependencies: [{ name: "std", path: "../std" }],
          },
        ],
      }),
      {
        httpMetadata: {
          contentType: "application/json; charset=utf-8",
        },
      },
    );

    const result = await indexPublishedRelease(env, release, manifest);

    expect(result.changed).toBe(true);

    const document = JSON.parse(
      (await bucket.text(packageIndexKey(getConfig(env), "kernel"))) ?? "null",
    ) as PackageIndexDocument;

    expect(document.releases).toHaveLength(1);
    expect(document.releases[0]).toMatchObject({
      version: "0.0.1",
      sha: "2aef0372bf5b6687db05bda80cde55f960cbfd9d",
      description: "Actor runtime kernel primitives for Riot",
      license: "Apache-2.0",
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
  release: PublishedReleaseRecord,
  overrides: Partial<PackagePublicationManifest> = {},
): PackagePublicationManifest {
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
