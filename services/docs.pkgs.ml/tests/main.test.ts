import { describe, expect, test } from "bun:test";

import { FakeD1Database, FakeExecutionContext, FakeR2Bucket } from "../../api.pkgs.ml/tests/helpers.ts";
import type { PackagePublishedEvent } from "../../api.pkgs.ml/src/types.ts";
import worker from "../src/main.ts";

interface TestEnv {
  ASSETS: { fetch(request: Request): Promise<Response> | Response };
  ML_PKGS_CDN: R2Bucket;
  SEARCH_DB: D1Database;
}

describe("docs.pkgs worker", () => {
  test("root redirects to pkgs.ml", async () => {
    const env = makeEnv();
    const response = await worker.fetch(new Request("https://docs.pkgs.ml/"), env);

    expect(response.status).toBe(302);
    expect(response.headers.get("location")).toBe("https://pkgs.ml/");
  });

  test("package docs route redirects to trailing slash", async () => {
    const env = makeEnv();
    const response = await worker.fetch(new Request("https://docs.pkgs.ml/p/std/0.1.0"), env);

    expect(response.status).toBe(308);
    expect(response.headers.get("location")).toBe("https://docs.pkgs.ml/p/std/0.1.0/");
  });

  test("package docs route serves generated docs from R2", async () => {
    const env = makeEnv();
    const bucket = env.ML_PKGS_CDN as unknown as FakeR2Bucket;
    await bucket.put("docs/std/0.1.0/index.html", "<h1>std docs</h1>", {
      httpMetadata: { contentType: "text/html; charset=utf-8" },
    });

    const response = await worker.fetch(new Request("https://docs.pkgs.ml/p/std/0.1.0/"), env);

    expect(response.status).toBe(200);
    expect(response.headers.get("content-type")).toContain("text/html");
    expect(await response.text()).toContain("std docs");
  });

  test("queue consumer records published releases for timer-driven processing", async () => {
    const env = makeEnv();
    const db = env.SEARCH_DB as unknown as FakeD1Database;
    const ctx = new FakeExecutionContext();
    let acked = false;

    const event: PackagePublishedEvent = {
      type: "package.published",
      package_name: "std",
      package_version: "0.1.0",
      package_locator: "github.com/leostera/riot/packages/std",
      source_url: "https://github.com/leostera/riot",
      package_subdir: "packages/std",
      artifact_sha256: "deadbeef",
      package_description: "Std library",
      package_license: "Apache-2.0",
      package_homepage: "https://riot.ml",
      package_repository: "https://github.com/leostera/riot",
      package_root_module: "Std",
      package_categories: [],
      package_keywords: [],
      dependencies: [],
      source_archive_key: "sources/std/0.1.0/deadbeef.tar.gz",
      manifest_key: "manifests/std/0.1.0/deadbeef.json",
      published_at: "2026-04-04T05:00:00.000Z",
    };

    await worker.queue?.(
      {
        queue: "riot-package-published",
        messages: [
          {
            id: "msg-1",
            timestamp: new Date(),
            attempts: 1,
            body: event,
            ack: () => {
              acked = true;
            },
            retry: () => {
              throw new Error("retry should not be called");
            },
          },
        ],
      } as unknown as MessageBatch<PackagePublishedEvent>,
      env,
      ctx,
    );

    expect(acked).toBe(true);

    const rows = await db
      .prepare(
        "SELECT status, attempt_count, status_message FROM package_releases_to_process WHERE package_name = ? AND package_version = ?",
      )
      .bind("std", "0.1.0")
      .all<{
        status: string;
        attempt_count: number;
        status_message: string;
      }>();

    expect(rows.results).toEqual([
      {
        status: "pending",
        attempt_count: 0,
        status_message: "Queued for package post-publish processing.",
      },
    ]);

    const events = await db
      .prepare(
        "SELECT event_type FROM registry_events WHERE package_name = ? AND package_version = ? ORDER BY event_id",
      )
      .bind("std", "0.1.0")
      .all<{ event_type: string }>();

    expect(events.results.map((event) => event.event_type)).toEqual([
      "package.processing.queued",
    ]);
  });

  test("scheduled processing stages docs and build runs for queued releases", async () => {
    const env = makeEnv();
    const bucket = env.ML_PKGS_CDN as unknown as FakeR2Bucket;
    const db = env.SEARCH_DB as unknown as FakeD1Database;
    const ctx = new FakeExecutionContext();

    const buildRequestPayload = await bucket.text("pipelines/builds/std/0.1.0/deadbeef/request.json");
    const event: PackagePublishedEvent = {
      type: "package.published",
      package_name: "std",
      package_version: "0.1.0",
      package_locator: "github.com/leostera/riot/packages/std",
      source_url: "https://github.com/leostera/riot",
      package_subdir: "packages/std",
      artifact_sha256: "deadbeef",
      package_description: "Std library",
      package_license: "Apache-2.0",
      package_homepage: "https://riot.ml",
      package_repository: "https://github.com/leostera/riot",
      package_root_module: "Std",
      package_categories: [],
      package_keywords: [],
      dependencies: [],
      source_archive_key: "sources/std/0.1.0/deadbeef.tar.gz",
      manifest_key: "manifests/std/0.1.0/deadbeef.json",
      published_at: "2026-04-04T05:00:00.000Z",
    };

    await worker.queue?.(
      {
        queue: "riot-package-published",
        messages: [
          {
            id: "msg-1",
            timestamp: new Date(),
            attempts: 1,
            body: event,
            ack: () => {},
            retry: () => {
              throw new Error("retry should not be called");
            },
          },
        ],
      } as unknown as MessageBatch<PackagePublishedEvent>,
      env,
      ctx,
    );

    await worker.scheduled?.(
      {
        cron: "* * * * *",
        scheduledTime: Date.now(),
      } as ScheduledController,
      env,
      ctx,
    );
    await ctx.drain();

    const rows = await db
      .prepare(
        "SELECT run_kind, status, output_prefix, request_key, status_message FROM package_pipeline_runs WHERE package_name = ? AND package_version = ? ORDER BY run_kind",
      )
      .bind("std", "0.1.0")
      .all<{
        run_kind: string;
        status: string;
        output_prefix: string;
        request_key: string;
        status_message: string;
      }>();

    expect(rows.results).toHaveLength(2);
    expect(rows.results[0]).toMatchObject({
      run_kind: "build",
      status: "staged",
      output_prefix: "pipelines/builds/std/0.1.0/deadbeef/",
      request_key: "pipelines/builds/std/0.1.0/deadbeef/request.json",
    });
    expect(rows.results[1]).toMatchObject({
      run_kind: "docs",
      status: "staged",
      output_prefix: "docs/std/0.1.0/",
      request_key: "docs/std/0.1.0/_pipeline/request.json",
    });

    const releaseRows = await db
      .prepare(
        "SELECT status, attempt_count, finished_at, status_message FROM package_releases_to_process WHERE package_name = ? AND package_version = ?",
      )
      .bind("std", "0.1.0")
      .all<{
        status: string;
        attempt_count: number;
        finished_at: string | null;
        status_message: string;
      }>();

    expect(releaseRows.results).toHaveLength(1);
    expect(releaseRows.results[0]).toMatchObject({
      status: "finished",
      attempt_count: 1,
      status_message: "Docs and build-verification requests staged for a future container-backed runner.",
    });
    expect(releaseRows.results[0]?.finished_at).not.toBeNull();

    expect(buildRequestPayload).toBeNull();

    const stagedBuildRequestPayload = await bucket.text("pipelines/builds/std/0.1.0/deadbeef/request.json");
    expect(stagedBuildRequestPayload).not.toBeNull();
    expect(stagedBuildRequestPayload).toContain('"run_kind": "build"');
    expect(stagedBuildRequestPayload).toContain('"command": [');
    expect(stagedBuildRequestPayload).toContain('"riot"');
    expect(stagedBuildRequestPayload).toContain('"build"');
    expect(stagedBuildRequestPayload).toContain('"result_key": "pipelines/builds/std/0.1.0/deadbeef/result.json"');
    expect(stagedBuildRequestPayload).toContain('"logs_key": "pipelines/builds/std/0.1.0/deadbeef/build.log"');

    const docsRequestPayload = await bucket.text("docs/std/0.1.0/_pipeline/request.json");
    expect(docsRequestPayload).not.toBeNull();
    expect(docsRequestPayload).toContain('"run_kind": "docs"');
    expect(docsRequestPayload).toContain('"riot_install_url": "https://get.riot.ml"');
    expect(docsRequestPayload).toContain('"riot_release_metadata_url": "https://cdn.pkgs.ml/riot/latest.json"');
    expect(docsRequestPayload).toContain('"doc"');

    const events = await db
      .prepare(
        "SELECT event_type FROM registry_events WHERE package_name = ? AND package_version = ? ORDER BY event_id",
      )
      .bind("std", "0.1.0")
      .all<{ event_type: string }>();

    expect(events.results.map((event) => event.event_type)).toEqual([
      "package.processing.queued",
      "package.processing.started",
      "package.docs.staged",
      "package.build.staged",
      "package.processing.finished",
    ]);
  });

  test("missing docs route surfaces staged pipeline status", async () => {
    const env = makeEnv();
    const ctx = new FakeExecutionContext();

    const event: PackagePublishedEvent = {
      type: "package.published",
      package_name: "std",
      package_version: "0.1.0",
      package_locator: "github.com/leostera/riot/packages/std",
      source_url: "https://github.com/leostera/riot",
      package_subdir: "packages/std",
      artifact_sha256: "deadbeef",
      package_description: "Std library",
      package_license: "Apache-2.0",
      package_homepage: "https://riot.ml",
      package_repository: "https://github.com/leostera/riot",
      package_root_module: "Std",
      package_categories: [],
      package_keywords: [],
      dependencies: [],
      source_archive_key: "sources/std/0.1.0/deadbeef.tar.gz",
      manifest_key: "manifests/std/0.1.0/deadbeef.json",
      published_at: "2026-04-04T05:00:00.000Z",
    };

    await worker.queue?.(
      {
        queue: "riot-package-published",
        messages: [
          {
            id: "msg-1",
            timestamp: new Date(),
            attempts: 1,
            body: event,
            ack: () => {},
            retry: () => {
              throw new Error("retry should not be called");
            },
          },
        ],
      } as unknown as MessageBatch<PackagePublishedEvent>,
      env,
      ctx,
    );

    const response = await worker.fetch(new Request("https://docs.pkgs.ml/p/std/0.1.0/"), env);

    expect(response.status).toBe(404);
    expect(await response.text()).toContain("Current release-processing status: pending.");
  });
});

function makeEnv(): TestEnv {
  return {
    ASSETS: {
      fetch: async () =>
        new Response("<html><body>landing</body></html>", {
          headers: {
            "content-type": "text/html; charset=utf-8",
          },
        }),
    },
    ML_PKGS_CDN: new FakeR2Bucket() as unknown as R2Bucket,
    SEARCH_DB: new FakeD1Database() as unknown as D1Database,
  };
}
