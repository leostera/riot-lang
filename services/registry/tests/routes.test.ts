import { describe, expect, test } from "bun:test";

import { handleRequest } from "../src/routes.ts";
import {
  applyMetadataMigrations,
  readPackageClaim,
  readPublishedRelease,
  readSelectorResolution,
} from "../src/metadata-db.ts";
import {
  indexConfigKey,
  manifestKey,
  packageIndexKey,
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
const INDEX_CONFIG = {
  cdnBaseUrl: "https://cdn.pkgs.ml",
  indexBasePath: "index/v1",
  viewsBasePath: "views/v1",
  authCookieDomain: "pkgs.ml",
  pkgsWebBaseUrl: "https://pkgs.ml",
};

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
        resolve: "/v1/packages/<locator>/resolve?ref=<selector>",
        manifest: "/v1/packages/<locator>/manifest/<sha>.json",
        source: "/v1/packages/<locator>/source/<sha>.tar.gz",
        publish: "/v1/packages/<locator>/publish?ref=<selector>",
        views_package_overview: "/v1/views/packages/<package-name>/overview",
        views_package_relations: "/v1/views/packages/<package-name>/relations",
        views_recent_packages: "/v1/views/recent/packages",
        views_popular_packages: "/v1/views/popular/packages",
        views_categories: "/v1/views/categories",
        views_owner_packages: "/v1/views/owners/<github-login>/packages",
        auth_github_start: "/v1/auth/github/start?return_to=<url>",
        auth_github_callback: "/v1/auth/github/callback?code=<code>&state=<state>",
        auth_logout: "/v1/auth/logout",
        me: "/v1/me",
        tokens: "/v1/me/tokens",
        search: "/v1/search?q=<query>",
      },
      legacy_routes: {
        resolve: "/package/<locator>/-/resolve?ref=<selector>",
        manifest: "/package/<locator>/-/manifest/<sha>.json",
        source: "/package/<locator>/-/source/<sha>.tar.gz",
        publish: "/package/<locator>/-/publish?ref=<selector>",
        views_package_overview: "/api/v1/views/packages/<package-name>/overview",
        views_package_relations: "/api/v1/views/packages/<package-name>/relations",
        views_recent_packages: "/api/v1/views/recent/packages",
        views_popular_packages: "/api/v1/views/popular/packages",
        views_categories: "/api/v1/views/categories",
        views_owner_packages: "/api/v1/views/owners/<github-login>/packages",
        auth_github_start: "/auth/github/start?return_to=<url>",
        auth_github_callback: "/auth/github/callback?code=<code>&state=<state>",
        auth_logout: "/auth/logout",
        me: "/api/v1/me",
        tokens: "/api/v1/me/tokens",
        search: "/api/v1/search?q=<query>",
      },
      cdn_base_url: "https://cdn.pkgs.ml",
    });

    const logKey = onlyRequestLogKey(bucket.keys());
    const logEntry = JSON.parse((await bucket.text(logKey)) ?? "null");
    expect(logEntry.route).toBe("root");
    expect(logEntry.success).toBe(true);
    expect(logEntry.status).toBe(200);
  });

  test("resolve returns cached SHA materialization metadata for GitHub shorthand locators", async () => {
    const { env, bucket } = makeEnv();
    const ctx = new FakeExecutionContext();

    await bucket.put(manifestKey(locator("leostera/minttea"), SHA), JSON.stringify({ ok: true }), {
      httpMetadata: { contentType: "application/json; charset=utf-8" },
    });
    await bucket.put(sourceArchiveKey(locator("leostera/minttea"), SHA), "tarball", {
      httpMetadata: { contentType: "application/gzip" },
    });

    const response = await handleRequest(
      new Request(`https://registry.test/v1/packages/leostera/minttea/resolve?ref=${SHA}`),
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
        url: `https://registry.test/v1/packages/github.com/leostera/minttea/manifest/${SHA}.json`,
        cdn_url: `https://cdn.pkgs.ml/packages/github.com/leostera/minttea/${SHA}.manifest.json`,
      },
      source_archive: {
        key: `sources/github.com/leostera/minttea/${SHA}.tar.gz`,
        url: `https://registry.test/v1/packages/github.com/leostera/minttea/source/${SHA}.tar.gz`,
        cdn_url: `https://cdn.pkgs.ml/sources/github.com/leostera/minttea/${SHA}.tar.gz`,
      },
      cache: {
        manifest: true,
        source: true,
      },
    });
  });

  test("resolve materializes an uncached package from GitHub without emitting publish events", async () => {
    const { env, bucket, queue, indexedQueue } = makeEnv();
    const ctx = new FakeExecutionContext();
    const archive = await makeTarGz({
      "tusk.toml": [
        "[package]",
        'name = "minttea"',
        'version = "0.4.2"',
        "public = true",
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
      expect((await readJson(response)) as Record<string, unknown>).toMatchObject({
        package: "github.com/leostera/minttea",
        source_url: "https://github.com/leostera/minttea",
        package_subdir: ".",
        selector: "main",
        resolved_sha: SHA,
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
    expect(manifest.package_public).toBe(true);
    expect(manifest.dependencies).toEqual([{ name: "std", path: "../std" }]);
    expect(await bucket.text(`sources/github.com/leostera/minttea/${SHA}.tar.gz`)).not.toBeNull();
    expect(queue.messages).toHaveLength(0);
  });

  test("semver-like tags freeze after first materialization", async () => {
    const { env, bucket, db, queue, indexedQueue } = makeEnv();
    const firstArchive = await makeTarGz({
      "tusk.toml": ['[package]', 'name = "minttea"', 'version = "0.4.2"', "public = true"].join(
        "\n",
      ),
    });
    const secondArchive = await makeTarGz({
      "tusk.toml": ['[package]', 'name = "minttea"', 'version = "9.9.9"', "public = true"].join(
        "\n",
      ),
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
    await applyMetadataMigrations(db as unknown as D1Database);
    expect(
      await readSelectorResolution(
        db as unknown as D1Database,
        locator("leostera/minttea").normalized,
        "0.4.2",
      ),
    ).not.toBeNull();
    expect(queue.messages).toHaveLength(0);
  });

  test("resolve rejects package paths without tusk.toml and does not cache fresh archives", async () => {
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

  test("resolve materializes a package from a repository subdirectory", async () => {
    const { env, bucket, queue } = makeEnv();
    const ctx = new FakeExecutionContext();
    const archive = await makeTarGz({
      "widgets/core/tusk.toml": [
        "[package]",
        'name = "minttea-core"',
        'version = "1.2.3"',
        "public = true",
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
        source_url: "https://github.com/leostera/minttea",
        package_subdir: "widgets/core",
        resolved_sha: SHA,
      });
    });

    const manifest = JSON.parse(
      (await bucket.text(`packages/github.com/leostera/minttea/widgets/core/${SHA}.manifest.json`)) ??
        "null",
    );
    expect(manifest.source_url).toBe("https://github.com/leostera/minttea");
    expect(manifest.package_name).toBe("minttea-core");
    expect(manifest.package_subdir).toBe("widgets/core");
    expect(queue.messages).toHaveLength(0);
  });

  test("publish creates claim and release records and emits package.published", async () => {
    const { env, bucket, db, queue, indexedQueue } = makeEnv();
    const ctx = new FakeExecutionContext();
    const archive = await makeTarGz({
      "tusk.toml": [
        "[package]",
        'name = "minttea"',
        'version = "0.4.2"',
        "public = true",
        'description = "Terminal UI toolkit for Riot"',
        'license = "MIT"',
        'homepage = "https://minttea.dev"',
        'repository = "https://github.com/leostera/minttea"',
        'root_module = "Minttea"',
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
        new Request("https://registry.test/package/leostera/minttea/-/publish?ref=main", {
          method: "POST",
          headers: {
            authorization: "Bearer root-secret",
          },
        }),
        env,
        ctx,
      );
      await ctx.drain();

      expect(response.status).toBe(200);
      expect((await readJson(response)) as Record<string, unknown>).toMatchObject({
        package: "github.com/leostera/minttea",
        source_url: "https://github.com/leostera/minttea",
        package_name: "minttea",
        package_version: "0.4.2",
        resolved_sha: SHA,
        claim: {
          key: "claims/minttea.json",
          created: true,
        },
        release: {
          key: "releases/minttea/0.4.2.json",
          created: true,
        },
      });
    });

    await applyMetadataMigrations(db as unknown as D1Database);
    expect(await readPackageClaim(db as unknown as D1Database, "minttea")).not.toBeNull();
    expect(await readPublishedRelease(db as unknown as D1Database, "minttea", "0.4.2")).not.toBeNull();
    expect(await bucket.text(indexConfigKey(INDEX_CONFIG))).not.toBeNull();
    expect(
      await bucket.text(packageIndexKey(INDEX_CONFIG, "minttea")),
    ).not.toBeNull();
    expect(queue.messages).toHaveLength(1);
    expect(queue.messages[0]).toMatchObject({
      type: "package.published",
      package_name: "minttea",
      package_version: "0.4.2",
      package_locator: "github.com/leostera/minttea",
      resolved_sha: SHA,
      package_description: "Terminal UI toolkit for Riot",
      package_license: "MIT",
      package_homepage: "https://minttea.dev",
      package_repository: "https://github.com/leostera/minttea",
      package_root_module: "Minttea",
    });
    expect(indexedQueue.messages).toHaveLength(1);
    expect(indexedQueue.messages[0]).toMatchObject({
      type: "package.indexed",
      package_name: "minttea",
      package_version: "0.4.2",
      package_locator: "github.com/leostera/minttea",
      resolved_sha: SHA,
      package_index_key: "index/v1/mi/nt/minttea.json",
      package_index_url: "https://cdn.pkgs.ml/index/v1/mi/nt/minttea.json",
      latest: "0.4.2",
    });

    const publishedRelease = await readPublishedRelease(
      db as unknown as D1Database,
      "minttea",
      "0.4.2",
    );
    expect(publishedRelease).toMatchObject({
      package_description: "Terminal UI toolkit for Riot",
      package_license: "MIT",
      package_homepage: "https://minttea.dev",
      package_repository: "https://github.com/leostera/minttea",
      package_root_module: "Minttea",
    });

    const packageDocument = JSON.parse(
      (await bucket.text(packageIndexKey(INDEX_CONFIG, "minttea"))) ?? "null",
    );
    expect(packageDocument.releases[0]).toMatchObject({
      description: "Terminal UI toolkit for Riot",
      license: "MIT",
      homepage: "https://minttea.dev",
      repository: "https://github.com/leostera/minttea",
      root_module: "Minttea",
    });
  });

  test("publish is idempotent for an already-published release", async () => {
    const { env, bucket, queue, indexedQueue } = makeEnv();
    const archive = await makeTarGz({
      "tusk.toml": [
        "[package]",
        'name = "minttea"',
        'version = "0.4.2"',
        "public = true",
        'description = "Terminal UI toolkit for Riot"',
        'license = "MIT"',
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
      const firstCtx = new FakeExecutionContext();
      const first = await handleRequest(
        new Request("https://registry.test/package/leostera/minttea/-/publish?ref=main", {
          method: "POST",
          headers: { authorization: "Bearer root-secret" },
        }),
        env,
        firstCtx,
      );
      await firstCtx.drain();

      const secondCtx = new FakeExecutionContext();
      const second = await handleRequest(
        new Request("https://registry.test/package/leostera/minttea/-/publish?ref=main", {
          method: "POST",
          headers: { authorization: "Bearer root-secret" },
        }),
        env,
        secondCtx,
      );
      await secondCtx.drain();

      expect(first.status).toBe(200);
      expect(second.status).toBe(200);
      expect((await readJson(second)) as Record<string, unknown>).toMatchObject({
        claim: { created: false },
        release: { created: false },
      });
    });

    expect(queue.messages).toHaveLength(1);
    expect(indexedQueue.messages).toHaveLength(2);
    expect(indexedQueue.messages[1]).toMatchObject({
      type: "package.indexed",
      package_name: "minttea",
      package_version: "0.4.2",
      package_locator: "github.com/leostera/minttea",
      resolved_sha: SHA,
      package_index_key: "index/v1/mi/nt/minttea.json",
      package_index_url: "https://cdn.pkgs.ml/index/v1/mi/nt/minttea.json",
      latest: "0.4.2",
    });
  });

  test("publish refreshes an existing release when the same package version is republished", async () => {
    const { env, bucket, db, queue, indexedQueue } = makeEnv();
    const firstArchive = await makeTarGz({
      "tusk.toml": [
        "[package]",
        'name = "minttea"',
        'version = "0.4.2"',
        "public = true",
        'description = "Original description"',
        'license = "MIT"',
      ].join("\n"),
    });
    const secondArchive = await makeTarGz({
      "tusk.toml": [
        "[package]",
        'name = "minttea"',
        'version = "0.4.2"',
        "public = true",
        'description = "Updated description"',
        'license = "Apache-2.0"',
      ].join("\n"),
    });

    let commitLookupCount = 0;
    await withMockedFetch(async (input) => {
      const url = new URL(typeof input === "string" ? input : input.toString());
      if (url.pathname === "/repos/leostera/minttea") {
        return Response.json({ private: false });
      }
      if (url.pathname === "/repos/leostera/minttea/commits/main") {
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
      const first = await handleRequest(
        new Request("https://registry.test/package/leostera/minttea/-/publish?ref=main", {
          method: "POST",
          headers: { authorization: "Bearer root-secret" },
        }),
        env,
        firstCtx,
      );
      await firstCtx.drain();

      const secondCtx = new FakeExecutionContext();
      const second = await handleRequest(
        new Request("https://registry.test/package/leostera/minttea/-/publish?ref=main", {
          method: "POST",
          headers: { authorization: "Bearer root-secret" },
        }),
        env,
        secondCtx,
      );
      await secondCtx.drain();

      expect(first.status).toBe(200);
      expect(second.status).toBe(200);
      expect((await readJson(second)) as Record<string, unknown>).toMatchObject({
        release: { created: false },
        resolved_sha: NEXT_SHA,
      });
    });

    expect(queue.messages).toHaveLength(1);
    expect(indexedQueue.messages).toHaveLength(2);

    await applyMetadataMigrations(db as unknown as D1Database);
    const publishedRelease = await readPublishedRelease(
      db as unknown as D1Database,
      "minttea",
      "0.4.2",
    );
    expect(publishedRelease).toMatchObject({
      resolved_sha: NEXT_SHA,
      package_description: "Updated description",
      package_license: "Apache-2.0",
    });

    const packageDocument = JSON.parse(
      (await bucket.text(packageIndexKey(INDEX_CONFIG, "minttea"))) ?? "null",
    );
    expect(packageDocument.releases[0]).toMatchObject({
      version: "0.4.2",
      sha: NEXT_SHA,
      description: "Updated description",
      license: "Apache-2.0",
    });
  });

  test("publish rejects requests without root auth", async () => {
    const { env, bucket, queue, indexedQueue } = makeEnv();
    const ctx = new FakeExecutionContext();

    const response = await handleRequest(
      new Request("https://registry.test/package/leostera/minttea/-/publish?ref=main", {
        method: "POST",
      }),
      env,
      ctx,
    );
    await ctx.drain();

    expect(response.status).toBe(401);
    expect(await readJson(response)).toMatchObject({
      error: "unauthorized",
    });
    expect(queue.messages).toHaveLength(0);
    expect(indexedQueue.messages).toHaveLength(0);
    expect(bucket.keys().filter((key) => key.startsWith("claims/"))).toHaveLength(0);
    expect(bucket.keys().filter((key) => key.startsWith("releases/"))).toHaveLength(0);
  });

  test("publish rejects non-public packages after materialization", async () => {
    const { env, bucket, db, queue, indexedQueue } = makeEnv();
    const ctx = new FakeExecutionContext();
    const archive = await makeTarGz({
      "tusk.toml": [
        "[package]",
        'name = "minttea"',
        'version = "0.4.2"',
        "public = false",
        'description = "Terminal UI toolkit for Riot"',
        'license = "MIT"',
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
        new Request("https://registry.test/package/leostera/minttea/-/publish?ref=main", {
          method: "POST",
          headers: { authorization: "Bearer root-secret" },
        }),
        env,
        ctx,
      );
      await ctx.drain();

      expect(response.status).toBe(422);
      expect(await readJson(response)).toMatchObject({
        error: "package_not_public",
      });
    });

    expect(await bucket.text(`packages/github.com/leostera/minttea/${SHA}.manifest.json`)).not.toBeNull();
    expect(queue.messages).toHaveLength(0);
    expect(indexedQueue.messages).toHaveLength(0);
    await applyMetadataMigrations(db as unknown as D1Database);
    expect(await readPackageClaim(db as unknown as D1Database, "minttea")).toBeNull();
    expect(await readPublishedRelease(db as unknown as D1Database, "minttea", "0.4.2")).toBeNull();
  });

  test("publish rejects package name conflicts from different locators", async () => {
    const { env, bucket, db, queue, indexedQueue } = makeEnv();
    const firstArchive = await makeTarGz({
      "tusk.toml": [
        "[package]",
        'name = "minttea"',
        'version = "0.4.2"',
        "public = true",
        'description = "Terminal UI toolkit for Riot"',
        'license = "MIT"',
      ].join("\n"),
    });
    const secondArchive = await makeTarGz({
      "tusk.toml": [
        "[package]",
        'name = "minttea"',
        'version = "0.5.0"',
        "public = true",
        'description = "Other terminal UI toolkit"',
        'license = "MIT"',
      ].join("\n"),
    });

    await withMockedFetch(async (input) => {
      const url = new URL(typeof input === "string" ? input : input.toString());
      if (url.pathname === "/repos/leostera/minttea") {
        return Response.json({ private: false });
      }
      if (url.pathname === "/repos/leostera/othertea") {
        return Response.json({ private: false });
      }
      if (url.pathname === "/repos/leostera/minttea/commits/main") {
        return Response.json({ sha: SHA });
      }
      if (url.pathname === "/repos/leostera/othertea/commits/main") {
        return Response.json({ sha: NEXT_SHA });
      }
      if (url.pathname === `/repos/leostera/minttea/tarball/${SHA}`) {
        return new Response(firstArchive, { status: 200 });
      }
      if (url.pathname === `/repos/leostera/othertea/tarball/${NEXT_SHA}`) {
        return new Response(secondArchive, { status: 200 });
      }

      throw new Error(`Unexpected fetch to ${url.toString()}`);
    }, async () => {
      const firstCtx = new FakeExecutionContext();
      const first = await handleRequest(
        new Request("https://registry.test/package/leostera/minttea/-/publish?ref=main", {
          method: "POST",
          headers: { authorization: "Bearer root-secret" },
        }),
        env,
        firstCtx,
      );
      await firstCtx.drain();
      expect(first.status).toBe(200);

      const secondCtx = new FakeExecutionContext();
      const second = await handleRequest(
        new Request("https://registry.test/package/leostera/othertea/-/publish?ref=main", {
          method: "POST",
          headers: { authorization: "Bearer root-secret" },
        }),
        env,
        secondCtx,
      );
      await secondCtx.drain();

      expect(second.status).toBe(409);
      expect(await readJson(second)).toMatchObject({
        error: "package_name_taken",
      });
    });

    expect(queue.messages).toHaveLength(1);
    expect(indexedQueue.messages).toHaveLength(1);
    await applyMetadataMigrations(db as unknown as D1Database);
    expect(await readPackageClaim(db as unknown as D1Database, "minttea")).not.toBeNull();
    expect(await readPublishedRelease(db as unknown as D1Database, "minttea", "0.5.0")).toBeNull();
  });

  test("publish rejects packages without a description", async () => {
    const { env, db, queue, indexedQueue } = makeEnv();
    const ctx = new FakeExecutionContext();
    const archive = await makeTarGz({
      "tusk.toml": [
        "[package]",
        'name = "minttea"',
        'version = "0.4.2"',
        "public = true",
        'license = "MIT"',
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
        new Request("https://registry.test/package/leostera/minttea/-/publish?ref=main", {
          method: "POST",
          headers: { authorization: "Bearer root-secret" },
        }),
        env,
        ctx,
      );
      await ctx.drain();

      expect(response.status).toBe(422);
      expect(await readJson(response)).toMatchObject({
        error: "missing_package_description",
      });
    });

    expect(queue.messages).toHaveLength(0);
    expect(indexedQueue.messages).toHaveLength(0);
    await applyMetadataMigrations(db as unknown as D1Database);
    expect(await readPackageClaim(db as unknown as D1Database, "minttea")).toBeNull();
    expect(await readPublishedRelease(db as unknown as D1Database, "minttea", "0.4.2")).toBeNull();
  });

  test("publish rejects packages with non-SPDX licenses", async () => {
    const { env, db, queue, indexedQueue } = makeEnv();
    const ctx = new FakeExecutionContext();
    const archive = await makeTarGz({
      "tusk.toml": [
        "[package]",
        'name = "minttea"',
        'version = "0.4.2"',
        "public = true",
        'description = "Terminal UI toolkit for Riot"',
        'license = "not-a-license"',
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
        new Request("https://registry.test/package/leostera/minttea/-/publish?ref=main", {
          method: "POST",
          headers: { authorization: "Bearer root-secret" },
        }),
        env,
        ctx,
      );
      await ctx.drain();

      expect(response.status).toBe(422);
      expect(await readJson(response)).toMatchObject({
        error: "invalid_package_license",
      });
    });

    expect(queue.messages).toHaveLength(0);
    expect(indexedQueue.messages).toHaveLength(0);
    await applyMetadataMigrations(db as unknown as D1Database);
    expect(await readPackageClaim(db as unknown as D1Database, "minttea")).toBeNull();
    expect(await readPublishedRelease(db as unknown as D1Database, "minttea", "0.4.2")).toBeNull();
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

  test("private repositories materialize when github token is configured", async () => {
    const { env, bucket, queue } = makeEnv({
      GITHUB_TOKEN: "secret-token",
    });
    const ctx = new FakeExecutionContext();
    const archive = await makeTarGz({
      "tusk.toml": ['[package]', 'name = "minttea"', 'version = "0.4.2"', "public = true"].join(
        "\n",
      ),
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
    expect(queue.messages).toHaveLength(0);
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
