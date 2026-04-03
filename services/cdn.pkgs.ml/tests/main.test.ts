import { describe, expect, test } from "bun:test";

import worker from "../src/main.ts";
import {
  FakeD1Database,
  FakeExecutionContext,
  FakeR2Bucket,
} from "../../api.pkgs.ml/tests/helpers.ts";

interface Env {
  ML_PKGS_CDN: R2Bucket;
  SEARCH_DB: D1Database;
}

describe("riot cdn worker", () => {
  test("serves a public sparse index bootstrap document from the request host", async () => {
    const bucket = new FakeR2Bucket();
    const db = new FakeD1Database();
    const ctx = new FakeExecutionContext();
    const env: Env = {
      ML_PKGS_CDN: bucket as unknown as R2Bucket,
      SEARCH_DB: db as unknown as D1Database,
    };

    const response = await worker.fetch(
      new Request("https://cdn.pkgs.ml/index/v1/config.json"),
      env,
      ctx,
    );
    await ctx.drain();

    expect(response.status).toBe(200);
    expect(response.headers.get("cache-control")).toBe("no-store");
    const payload = await response.json() as {
      schema_version: number;
      kind: string;
      package_path_strategy: string;
      index_base_url: string;
      artifact_base_url: string;
    };

    expect(payload).toEqual({
      schema_version: 1,
      kind: "sparse",
      package_path_strategy: "cargo-lowercase-v1",
      index_base_url: "https://cdn.pkgs.ml/index/v1",
      artifact_base_url: "https://cdn.pkgs.ml",
    });

    const reads = await db
      .prepare("SELECT document_key, package_name FROM index_reads")
      .all<{ document_key: string; package_name: string | null }>();

    expect(reads.results).toEqual([
      {
        document_key: "index/v1/config.json",
        package_name: null,
      },
    ]);
  });

  test("proxies package source archives and records package downloads", async () => {
    const bucket = new FakeR2Bucket();
    const db = new FakeD1Database();
    const ctx = new FakeExecutionContext();
    const env: Env = {
      ML_PKGS_CDN: bucket as unknown as R2Bucket,
      SEARCH_DB: db as unknown as D1Database,
    };

    await bucket.put("sources/kernel/0.0.1/deadbeef.tar.gz", "archive", {
      httpMetadata: {
        contentType: "application/gzip",
      },
    });

    const response = await worker.fetch(
      new Request("https://cdn.pkgs.ml/sources/kernel/0.0.1/deadbeef.tar.gz"),
      env,
      ctx,
    );
    await ctx.drain();

    expect(response.status).toBe(200);
    expect(response.headers.get("cache-control")).toBe("public, max-age=31536000, immutable");
    expect(await response.text()).toBe("archive");

    const downloads = await db
      .prepare(
        "SELECT package_name, package_version, artifact_sha256, source_archive_key FROM package_downloads",
      )
      .all<{
        package_name: string;
        package_version: string;
        artifact_sha256: string;
        source_archive_key: string;
      }>();

    expect(downloads.results).toEqual([
      {
        package_name: "kernel",
        package_version: "0.0.1",
        artifact_sha256: "deadbeef",
        source_archive_key: "sources/kernel/0.0.1/deadbeef.tar.gz",
      },
    ]);
  });

  test("serves sanitized sparse index documents and records index reads", async () => {
    const bucket = new FakeR2Bucket();
    const db = new FakeD1Database();
    const ctx = new FakeExecutionContext();
    const env: Env = {
      ML_PKGS_CDN: bucket as unknown as R2Bucket,
      SEARCH_DB: db as unknown as D1Database,
    };

    await bucket.put(
      "index/v1/ke/rn/kernel.json",
      JSON.stringify({
        schema_version: 1,
        name: "kernel",
        latest: "0.2.0",
        updated_at: "2026-04-02T10:00:00.000Z",
        releases: [
          {
            version: "0.2.0",
            published_at: "2026-04-02T10:00:00.000Z",
            canonical_locator: "",
            repo_url: "",
            subdir: ".",
            artifact_sha256: "bbbb",
            manifest_key: "packages/github.com/example/kernel/bbbb.manifest.json",
            source_key: "sources/github.com/example/kernel/bbbb.tar.gz",
            dependencies: [],
          },
          {
            version: "0.1.0",
            published_at: "2026-04-01T10:00:00.000Z",
            canonical_locator: "",
            repo_url: "",
            subdir: ".",
            artifact_sha256: "aaaa",
            manifest_key: "packages/kernel/0.1.0/aaaa.manifest.json",
            source_key: "sources/kernel/0.1.0/aaaa.tar.gz",
            dependencies: [],
          },
        ],
      }),
      {
        httpMetadata: {
          contentType: "application/json; charset=utf-8",
        },
      },
    );

    const response = await worker.fetch(
      new Request("https://cdn.pkgs.ml/index/v1/ke/rn/kernel.json"),
      env,
      ctx,
    );
    await ctx.drain();

    expect(response.status).toBe(200);
    expect(response.headers.get("cache-control")).toBe("no-store");
    const payload = await response.json() as {
      schema_version: number;
      name: string;
      latest: string;
      updated_at: string;
      releases: Array<{
        version: string;
        published_at: string;
        canonical_locator: string;
        repo_url: string;
        subdir: string;
        artifact_sha256: string;
        manifest_key: string;
        source_key: string;
        dependencies: unknown[];
      }>;
    };

    expect(payload).toEqual({
      schema_version: 1,
      name: "kernel",
      latest: "0.1.0",
      updated_at: "2026-04-01T10:00:00.000Z",
      releases: [
        {
          version: "0.1.0",
          published_at: "2026-04-01T10:00:00.000Z",
          canonical_locator: "",
          repo_url: "",
          subdir: ".",
          artifact_sha256: "aaaa",
          manifest_key: "packages/kernel/0.1.0/aaaa.manifest.json",
          source_key: "sources/kernel/0.1.0/aaaa.tar.gz",
          dependencies: [],
        },
      ],
    });

    const reads = await db
      .prepare("SELECT document_key, package_name FROM index_reads")
      .all<{ document_key: string; package_name: string | null }>();

    expect(reads.results).toEqual([
      {
        document_key: "index/v1/ke/rn/kernel.json",
        package_name: "kernel",
      },
    ]);
  });
});
