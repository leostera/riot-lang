import { describe, expect, test } from "bun:test";

import { handleRequest } from "../src/routes.ts";
import {
  manifestKey,
  selectorResolutionKey,
  sourceArchiveKey,
} from "../src/storage.ts";
import {
  FakeExecutionContext,
  makeEnv,
  makeTarGz,
  withMockedFetch,
} from "./helpers.ts";

const SHA = "0123456789abcdef0123456789abcdef01234567";
const NEXT_SHA = "89abcdef012345670123456789abcdef01234567";

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
        cdn_url: `https://cdn.pkgs.ml/packages/github.com/leostera/minttea/${SHA}.manifest.json`,
      },
      source_archive: {
        key: `sources/github.com/leostera/minttea/${SHA}.tar.gz`,
        url: `https://registry.test/package/github.com/leostera/minttea/-/source/${SHA}.tar.gz`,
        cdn_url: `https://cdn.pkgs.ml/sources/github.com/leostera/minttea/${SHA}.tar.gz`,
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

  test("resolve publishes an uncached package from GitHub and emits a queue event", async () => {
    const { env, bucket, queue } = makeEnv();
    const ctx = new FakeExecutionContext();
    const archive = await makeTarGz({
      "tusk.toml": [
        "[package]",
        'name = "minttea"',
        'version = "0.4.2"',
        "",
        "[dependencies]",
        'std = { path = "../std" }',
      ].join("\n"),
    });

    await withMockedFetch(async (input) => {
      const url = new URL(typeof input === "string" ? input : input.toString());
      if (url.pathname === "/repos/leostera/minttea") {
        return Response.json({ private: false });
      }

      if (url.pathname === "/repos/leostera/minttea/commits/main") {
        return Response.json({ sha: SHA });
      }

      if (url.pathname === `/repos/leostera/minttea/tarball/${SHA}`) {
        return new Response(archive, { status: 200 });
      }

      throw new Error(`Unexpected fetch to ${url.toString()}`);
    }, async () => {
      const response = await handleRequest(
        new Request("https://registry.test/package/leostera/minttea/-/resolve?ref=main"),
        env,
        ctx,
      );
      await ctx.drain();

      expect(response.status).toBe(200);
      expect(await readJson(response)).toEqual({
        package: "github.com/leostera/minttea",
        source_url: "https://github.com/leostera/minttea",
        package_subdir: ".",
        selector: "main",
        resolved_sha: SHA,
        manifest: {
          key: `packages/github.com/leostera/minttea/${SHA}.manifest.json`,
          url: `https://registry.test/package/github.com/leostera/minttea/-/manifest/${SHA}.json`,
          cdn_url: `https://cdn.pkgs.ml/packages/github.com/leostera/minttea/${SHA}.manifest.json`,
        },
        source_archive: {
          key: `sources/github.com/leostera/minttea/${SHA}.tar.gz`,
          url: `https://registry.test/package/github.com/leostera/minttea/-/source/${SHA}.tar.gz`,
          cdn_url: `https://cdn.pkgs.ml/sources/github.com/leostera/minttea/${SHA}.tar.gz`,
        },
        cache: {
          manifest: false,
          source: false,
        },
      });
    });

    const manifest = JSON.parse(
      (await bucket.text(`packages/github.com/leostera/minttea/${SHA}.manifest.json`)) ?? "null",
    );
    expect(manifest.package_name).toBe("minttea");
    expect(manifest.package_version).toBe("0.4.2");
    expect(manifest.dependencies).toEqual([{ name: "std", path: "../std" }]);

    expect(await bucket.text(`sources/github.com/leostera/minttea/${SHA}.tar.gz`)).not.toBeNull();
    expect(queue.messages).toHaveLength(1);
    expect(queue.messages[0]).toMatchObject({
      type: "package.published",
      package_locator: "github.com/leostera/minttea",
      selector: "main",
      resolved_sha: SHA,
      package_name: "minttea",
    });

    const logKey = onlyRequestLogKey(bucket.keys());
    const logEntry = JSON.parse((await bucket.text(logKey)) ?? "null");
    expect(logEntry.route).toBe("resolve");
    expect(logEntry.selector).toBe("main");
    expect(logEntry.resolved_sha).toBe(SHA);
    expect(logEntry.success).toBe(true);
    expect(logEntry.status).toBe(200);
  });

  test("semver-like tags freeze after first publication", async () => {
    const { env, bucket, queue } = makeEnv();
    const firstArchive = await makeTarGz({
      "tusk.toml": ['[package]', 'name = "minttea"', 'version = "0.4.2"'].join("\n"),
    });
    const secondArchive = await makeTarGz({
      "tusk.toml": ['[package]', 'name = "minttea"', 'version = "9.9.9"'].join("\n"),
    });
    let commitLookupCount = 0;

    await withMockedFetch(async (input) => {
      const url = new URL(typeof input === "string" ? input : input.toString());
      if (url.pathname === "/repos/leostera/minttea") {
        return Response.json({ private: false });
      }

      if (url.pathname === "/repos/leostera/minttea/commits/0.4.2") {
        commitLookupCount += 1;
        return Response.json({ sha: commitLookupCount === 1 ? SHA : NEXT_SHA });
      }

      if (url.pathname === `/repos/leostera/minttea/tarball/${SHA}`) {
        return new Response(firstArchive, { status: 200 });
      }

      if (url.pathname === `/repos/leostera/minttea/tarball/${NEXT_SHA}`) {
        return new Response(secondArchive, { status: 200 });
      }

      throw new Error(`Unexpected fetch to ${url.toString()}`);
    }, async () => {
      const firstCtx = new FakeExecutionContext();
      const firstResponse = await handleRequest(
        new Request("https://registry.test/package/leostera/minttea/-/resolve?ref=0.4.2"),
        env,
        firstCtx,
      );
      await firstCtx.drain();

      const secondCtx = new FakeExecutionContext();
      const secondResponse = await handleRequest(
        new Request("https://registry.test/package/leostera/minttea/-/resolve?ref=0.4.2"),
        env,
        secondCtx,
      );
      await secondCtx.drain();

      expect((await readJson(firstResponse)) as Record<string, unknown>).toMatchObject({
        resolved_sha: SHA,
      });
      expect((await readJson(secondResponse)) as Record<string, unknown>).toMatchObject({
        resolved_sha: SHA,
      });
    });

    expect(commitLookupCount).toBe(1);
    expect(await bucket.text(selectorResolutionKey(locator("leostera/minttea"), "0.4.2"))).not.toBeNull();
    expect(queue.messages).toHaveLength(1);
  });

  test("missing package manifests do not cache source archives or publish packages", async () => {
    const { env, bucket, queue } = makeEnv();
    const ctx = new FakeExecutionContext();
    const archive = await makeTarGz({
      "README.md": "# no package here",
    });

    await withMockedFetch(async (input) => {
      const url = new URL(typeof input === "string" ? input : input.toString());
      if (url.pathname === "/repos/leostera/minttea") {
        return Response.json({ private: false });
      }

      if (url.pathname === "/repos/leostera/minttea/commits/main") {
        return Response.json({ sha: SHA });
      }

      if (url.pathname === `/repos/leostera/minttea/tarball/${SHA}`) {
        return new Response(archive, { status: 200 });
      }

      throw new Error(`Unexpected fetch to ${url.toString()}`);
    }, async () => {
      const response = await handleRequest(
        new Request("https://registry.test/package/leostera/minttea/widgets/core/-/resolve?ref=main"),
        env,
        ctx,
      );
      await ctx.drain();

      expect(response.status).toBe(404);
      expect(await readJson(response)).toMatchObject({
        error: "package_not_found",
      });
    });

    expect(await bucket.text(`sources/github.com/leostera/minttea/${SHA}.tar.gz`)).toBeNull();
    expect(await bucket.text(`packages/github.com/leostera/minttea/widgets/core/${SHA}.manifest.json`)).toBeNull();
    expect(queue.messages).toHaveLength(0);
  });

  test("resolve publishes a package from a repository subdirectory", async () => {
    const { env, bucket, queue } = makeEnv();
    const ctx = new FakeExecutionContext();
    const archive = await makeTarGz({
      "widgets/core/tusk.toml": [
        "[package]",
        'name = "minttea-core"',
        'version = "1.2.3"',
        "",
        "[dependencies]",
        'std = { path = "../../../std" }',
      ].join("\n"),
    });

    await withMockedFetch(async (input) => {
      const url = new URL(typeof input === "string" ? input : input.toString());
      if (url.pathname === "/repos/leostera/minttea") {
        return Response.json({ private: false });
      }

      if (url.pathname === "/repos/leostera/minttea/commits/main") {
        return Response.json({ sha: SHA });
      }

      if (url.pathname === `/repos/leostera/minttea/tarball/${SHA}`) {
        return new Response(archive, { status: 200 });
      }

      throw new Error(`Unexpected fetch to ${url.toString()}`);
    }, async () => {
      const response = await handleRequest(
        new Request("https://registry.test/package/leostera/minttea/widgets/core/-/resolve?ref=main"),
        env,
        ctx,
      );
      await ctx.drain();

      expect(response.status).toBe(200);
      expect((await readJson(response)) as Record<string, unknown>).toMatchObject({
        package: "github.com/leostera/minttea/widgets/core",
        package_subdir: "widgets/core",
        resolved_sha: SHA,
      });
    });

    const manifest = JSON.parse(
      (await bucket.text(`packages/github.com/leostera/minttea/widgets/core/${SHA}.manifest.json`)) ??
        "null",
    );
    expect(manifest.package_name).toBe("minttea-core");
    expect(manifest.package_subdir).toBe("widgets/core");
    expect(queue.messages[0]).toMatchObject({
      package_locator: "github.com/leostera/minttea/widgets/core",
      package_name: "minttea-core",
    });
  });

  test("github selector misses surface as 404 without publishing", async () => {
    const { env, bucket, queue } = makeEnv();
    const ctx = new FakeExecutionContext();

    await withMockedFetch(async (input) => {
      const url = new URL(typeof input === "string" ? input : input.toString());
      if (url.pathname === "/repos/leostera/minttea") {
        return Response.json({ private: false });
      }

      if (url.pathname === "/repos/leostera/minttea/commits/missing-tag") {
        return new Response("not found", { status: 404 });
      }

      throw new Error(`Unexpected fetch to ${url.toString()}`);
    }, async () => {
      const response = await handleRequest(
        new Request("https://registry.test/package/leostera/minttea/-/resolve?ref=missing-tag"),
        env,
        ctx,
      );
      await ctx.drain();

      expect(response.status).toBe(404);
      expect(await readJson(response)).toMatchObject({
        error: "github_ref_not_found",
      });
    });

    expect(queue.messages).toHaveLength(0);
    expect(bucket.keys().filter((key) => key.startsWith("packages/"))).toHaveLength(0);
    expect(bucket.keys().filter((key) => key.startsWith("sources/"))).toHaveLength(0);
  });

  test("github upstream failures surface as 502 without publishing", async () => {
    const { env, bucket, queue } = makeEnv();
    const ctx = new FakeExecutionContext();

    await withMockedFetch(async (input) => {
      const url = new URL(typeof input === "string" ? input : input.toString());
      if (url.pathname === "/repos/leostera/minttea") {
        return new Response("boom", { status: 500 });
      }

      throw new Error(`Unexpected fetch to ${url.toString()}`);
    }, async () => {
      const response = await handleRequest(
        new Request("https://registry.test/package/leostera/minttea/-/resolve?ref=main"),
        env,
        ctx,
      );
      await ctx.drain();

      expect(response.status).toBe(502);
      expect(await readJson(response)).toMatchObject({
        error: "github_unavailable",
      });
    });

    expect(queue.messages).toHaveLength(0);
    expect(bucket.keys().filter((key) => key.startsWith("packages/"))).toHaveLength(0);
    expect(bucket.keys().filter((key) => key.startsWith("sources/"))).toHaveLength(0);
  });

  test("private repositories require a github token", async () => {
    const { env, bucket, queue } = makeEnv({
      GITHUB_TOKEN: "",
    });
    const ctx = new FakeExecutionContext();

    await withMockedFetch(async (input) => {
      const url = new URL(typeof input === "string" ? input : input.toString());
      if (url.pathname === "/repos/leostera/minttea") {
        return Response.json({ private: true });
      }

      throw new Error(`Unexpected fetch to ${url.toString()}`);
    }, async () => {
      const response = await handleRequest(
        new Request("https://registry.test/package/leostera/minttea/-/resolve?ref=main"),
        env,
        ctx,
      );
      await ctx.drain();

      expect(response.status).toBe(403);
      expect(await readJson(response)).toMatchObject({
        error: "private_upstream_requires_token",
      });
    });

    expect(queue.messages).toHaveLength(0);
    expect(bucket.keys().filter((key) => key.startsWith("packages/"))).toHaveLength(0);
    expect(bucket.keys().filter((key) => key.startsWith("sources/"))).toHaveLength(0);
  });

  test("private repositories publish when github token is configured", async () => {
    const { env, bucket, queue } = makeEnv({
      GITHUB_TOKEN: "secret-token",
    });
    const ctx = new FakeExecutionContext();
    const archive = await makeTarGz({
      "tusk.toml": ['[package]', 'name = "minttea"', 'version = "0.4.2"'].join("\n"),
    });

    await withMockedFetch(async (input, init) => {
      const url = new URL(typeof input === "string" ? input : input.toString());
      const headers = new Headers(init?.headers);
      expect(headers.get("authorization")).toBe("Bearer secret-token");

      if (url.pathname === "/repos/leostera/minttea") {
        return Response.json({ private: true });
      }

      if (url.pathname === "/repos/leostera/minttea/commits/main") {
        return Response.json({ sha: SHA });
      }

      if (url.pathname === `/repos/leostera/minttea/tarball/${SHA}`) {
        return new Response(archive, { status: 200 });
      }

      throw new Error(`Unexpected fetch to ${url.toString()}`);
    }, async () => {
      const response = await handleRequest(
        new Request("https://registry.test/package/leostera/minttea/-/resolve?ref=main"),
        env,
        ctx,
      );
      await ctx.drain();

      expect(response.status).toBe(200);
      expect((await readJson(response)) as Record<string, unknown>).toMatchObject({
        resolved_sha: SHA,
      });
    });

    expect(await bucket.text(`packages/github.com/leostera/minttea/${SHA}.manifest.json`)).not.toBeNull();
    expect(queue.messages).toHaveLength(1);
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

  test("source route redirects to the CDN-backed immutable archive URL", async () => {
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

    expect(response.status).toBe(307);
    expect(response.headers.get("location")).toBe(
      `https://cdn.pkgs.ml/sources/github.com/leostera/minttea/${SHA}.tar.gz`,
    );
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

function locator(raw: string) {
  return {
    raw,
    normalized: `github.com/${raw}`,
    provider: "github.com",
    owner: raw.split("/")[0]!,
    repo: raw.split("/")[1]!,
    subpath: null,
  };
}
