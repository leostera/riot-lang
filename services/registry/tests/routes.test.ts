import { describe, expect, test } from "bun:test";

import { handleRequest } from "../src/routes.ts";
import { manifestKey, requestLogKey, sourceArchiveKey } from "../src/storage.ts";
import { makeEnv, FakeExecutionContext } from "./helpers.ts";

const SHA = "0123456789abcdef0123456789abcdef01234567";

describe("riot package registry routes", () => {
  test("root route returns service metadata and logs the request", async () => {
    const { env, bucket } = makeEnv();
    const ctx = new FakeExecutionContext();

    const response = await handleRequest(new Request("https://registry.test/"), env, ctx);
    await ctx.drain();

    expect(response.status).toBe(200);
    expect(await readJson(response)).toEqual({
      service: "riot-package-registry",
      routes: {
        resolve: "/package/<locator>/-/resolve?ref=<selector>",
        manifest: "/package/<locator>/-/manifest/<sha>.json",
        source: "/package/<locator>/-/source/<sha>.tar.gz",
      },
      cdn_base_url: "https://cdn.pkgs.ml",
    });

    const logKey = onlyRequestLogKey(bucket.keys());
    const logEntry = JSON.parse((await bucket.text(logKey)) ?? "null");
    expect(logEntry.route).toBe("root");
    expect(logEntry.success).toBe(true);
    expect(logEntry.status).toBe(200);
  });

  test("resolve returns cached SHA publication metadata for GitHub shorthand locators", async () => {
    const { env, bucket } = makeEnv();
    const ctx = new FakeExecutionContext();

    await bucket.put(
      manifestKey(
        {
          raw: "leostera/minttea",
          normalized: "github.com/leostera/minttea",
          provider: "github.com",
          owner: "leostera",
          repo: "minttea",
          subpath: null,
        },
        SHA,
      ),
      JSON.stringify({ ok: true }),
      { httpMetadata: { contentType: "application/json; charset=utf-8" } },
    );

    await bucket.put(
      sourceArchiveKey(
        {
          raw: "leostera/minttea",
          normalized: "github.com/leostera/minttea",
          provider: "github.com",
          owner: "leostera",
          repo: "minttea",
          subpath: null,
        },
        SHA,
      ),
      "tarball",
      { httpMetadata: { contentType: "application/gzip" } },
    );

    const response = await handleRequest(
      new Request(`https://registry.test/package/leostera/minttea/-/resolve?ref=${SHA}`),
      env,
      ctx,
    );
    await ctx.drain();

    expect(response.status).toBe(200);
    expect(await readJson(response)).toEqual({
      package: "github.com/leostera/minttea",
      source_url: "https://github.com/leostera/minttea",
      package_subdir: ".",
      selector: SHA,
      resolved_sha: SHA,
      manifest: {
        key: `packages/github.com/leostera/minttea/${SHA}.manifest.json`,
        url: `https://registry.test/package/github.com/leostera/minttea/-/manifest/${SHA}.json`,
        cdn_url: `https://cdn.pkgs.ml/packages/leostera/minttea/-/${SHA}.manifest.json`,
      },
      source_archive: {
        key: `sources/github.com/leostera/minttea/${SHA}.tar.gz`,
        url: `https://registry.test/package/github.com/leostera/minttea/-/source/${SHA}.tar.gz`,
        cdn_url: `https://cdn.pkgs.ml/packages/leostera/minttea/-/${SHA}.tar.gz`,
      },
      cache: {
        manifest: true,
        source: true,
      },
    });

    const logKey = onlyRequestLogKey(bucket.keys());
    const logEntry = JSON.parse((await bucket.text(logKey)) ?? "null");
    expect(logEntry.route).toBe("resolve");
    expect(logEntry.package_locator).toBe("github.com/leostera/minttea");
    expect(logEntry.resolved_sha).toBe(SHA);
    expect(logEntry.success).toBe(true);
  });

  test("resolve returns not implemented for non-SHA selectors and still logs the request", async () => {
    const { env, bucket } = makeEnv();
    const ctx = new FakeExecutionContext();

    const response = await handleRequest(
      new Request("https://registry.test/package/leostera/minttea/-/resolve?ref=main"),
      env,
      ctx,
    );
    await ctx.drain();

    expect(response.status).toBe(501);
    expect(await readJson(response)).toEqual({
      error: "selector_resolution_not_implemented",
      message:
        "This scaffold only resolves already-published SHA selectors. GitHub ref resolution and first publication are the next step.",
      package: "github.com/leostera/minttea",
      selector: "main",
      source_url: "https://github.com/leostera/minttea",
    });

    const logKey = onlyRequestLogKey(bucket.keys());
    const logEntry = JSON.parse((await bucket.text(logKey)) ?? "null");
    expect(logEntry.route).toBe("resolve");
    expect(logEntry.selector).toBe("main");
    expect(logEntry.success).toBe(false);
    expect(logEntry.status).toBe(501);
  });

  test("manifest route returns immutable JSON from R2", async () => {
    const { env, bucket } = makeEnv();
    const ctx = new FakeExecutionContext();
    const manifest = { package_name: "core" };

    await bucket.put(
      `packages/github.com/leostera/minttea/widgets/core/${SHA}.manifest.json`,
      JSON.stringify(manifest),
      { httpMetadata: { contentType: "application/json; charset=utf-8" } },
    );

    const response = await handleRequest(
      new Request(
        `https://registry.test/package/github.com/leostera/minttea/widgets/core/-/manifest/${SHA}.json`,
      ),
      env,
      ctx,
    );
    await ctx.drain();

    expect(response.status).toBe(200);
    expect(response.headers.get("cache-control")).toBe("public, max-age=31536000, immutable");
    expect(response.headers.get("content-type")).toBe("application/json; charset=utf-8");
    expect(await readJson(response)).toEqual(manifest);
  });

  test("source route returns immutable bytes from R2", async () => {
    const { env, bucket } = makeEnv();
    const ctx = new FakeExecutionContext();

    await bucket.put(`sources/github.com/leostera/minttea/${SHA}.tar.gz`, "archive-bytes", {
      httpMetadata: { contentType: "application/gzip" },
    });

    const response = await handleRequest(
      new Request(`https://registry.test/package/leostera/minttea/-/source/${SHA}.tar.gz`),
      env,
      ctx,
    );
    await ctx.drain();

    expect(response.status).toBe(200);
    expect(response.headers.get("cache-control")).toBe("public, max-age=31536000, immutable");
    expect(response.headers.get("content-type")).toBe("application/gzip");
    expect(await response.text()).toBe("archive-bytes");
  });

  test("invalid locators return 400 and failures are still logged", async () => {
    const { env, bucket } = makeEnv();
    const ctx = new FakeExecutionContext();

    const response = await handleRequest(
      new Request("https://registry.test/package/leostera/-/resolve?ref=main"),
      env,
      ctx,
    );
    await ctx.drain();

    expect(response.status).toBe(400);
    expect(await readJson(response)).toMatchObject({
      error: "invalid_locator",
    });

    const logKey = onlyRequestLogKey(bucket.keys());
    const logEntry = JSON.parse((await bucket.text(logKey)) ?? "null");
    expect(logEntry.success).toBe(false);
    expect(logEntry.status).toBe(400);
    expect(logEntry.error_category).toBe("invalid_locator");
  });
});

function onlyRequestLogKey(keys: string[]): string {
  const requestKeys = keys.filter((key) => key.startsWith("requests/"));
  expect(requestKeys).toHaveLength(1);
  return requestKeys[0]!;
}

async function readJson(response: Response): Promise<unknown> {
  return JSON.parse(await response.text());
}
