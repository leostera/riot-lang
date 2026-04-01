import { describe, expect, test } from "bun:test";

import { readArchiveFileFromTarGz } from "../src/archive.ts";
import { handleRequest } from "../src/routes.ts";
import {
  applyMetadataMigrations,
  listPackageRegistryEvents,
  listRequestLogs,
  readPackageClaim,
  readPublishedRelease,
} from "../src/metadata-db.ts";
import { FakeExecutionContext, makeEnv, makeTarGz } from "./helpers.ts";

describe("riot package registry routes", () => {
  test("root route returns service metadata and logs the request", async () => {
    const { env, db } = makeEnv();
    const ctx = new FakeExecutionContext();

    const response = await handleRequest(new Request("https://registry.test/"), env, ctx);
    await ctx.drain();

    expect(response.status).toBe(200);
    expect(await readJson(response)).toEqual({
      service: "riot-package-registry",
      routes: {
        publish_artifact: "/v1/publish",
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
        events: "/v1/events?limit=<count>&after=<event-id>",
        package_events: "/v1/packages/<package-name>/events?version=<version>&limit=<count>",
      },
      legacy_routes: {
        publish_artifact: "/api/v1/publish",
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
        events: "/api/v1/events?limit=<count>&after=<event-id>",
        package_events: "/api/v1/packages/<package-name>/events?version=<version>&limit=<count>",
      },
      cdn_base_url: "https://cdn.pkgs.ml",
    });

    await applyMetadataMigrations(db as unknown as D1Database);
    const logEntry = (await listRequestLogs(db as unknown as D1Database, 1))[0]!;
    expect(logEntry.route).toBe("root");
    expect(logEntry.success).toBe(true);
    expect(logEntry.status).toBe(200);
  });

  test("removed source-resolution routes now return 404", async () => {
    const { env } = makeEnv();

    const responses = await Promise.all([
      handleRequest(
        new Request("https://registry.test/v1/packages/github.com/leostera/minttea/resolve?ref=main"),
        env,
        new FakeExecutionContext(),
      ),
      handleRequest(
        new Request("https://registry.test/package/leostera/minttea/-/publish?ref=main", {
          method: "POST",
          headers: { authorization: "Bearer root-secret" },
        }),
        env,
        new FakeExecutionContext(),
      ),
    ]);

    expect(responses[0].status).toBe(404);
    expect(responses[1].status).toBe(404);
  });

  test("events route returns the latest registry events in reverse chronological order", async () => {
    const { env, db } = makeEnv();
    await applyMetadataMigrations(db as unknown as D1Database);
    await db.exec(`
      INSERT INTO registry_events (
        event_id,
        event_type,
        package_name,
        package_version,
        package_locator,
        payload_json,
        created_at
      ) VALUES
        (
          'evt-1',
          'package.submitted',
          'kernel',
          '0.0.1',
          '',
          '{"artifact_sha256":"sha-1"}',
          '2026-03-30T12:00:00.000Z'
        ),
        (
          'evt-2',
          'package.indexed',
          'kernel',
          '0.0.1',
          '',
          '{"artifact_sha256":"sha-1","latest":"0.0.1"}',
          '2026-03-30T12:01:00.000Z'
        )
    `);

    const response = await handleRequest(
      new Request("https://registry.test/v1/events?limit=1"),
      env,
      new FakeExecutionContext(),
    );

    expect(response.status).toBe(200);
    expect(await readJson(response)).toEqual({
      limit: 1,
      events: [
        {
          event_id: "evt-2",
          event_type: "package.indexed",
          package_name: "kernel",
          package_version: "0.0.1",
          package_locator: "",
          payload: {
            artifact_sha256: "sha-1",
            latest: "0.0.1",
          },
          created_at: "2026-03-30T12:01:00.000Z",
        },
      ],
    });
  });

  test("events route supports incremental polling via after event id", async () => {
    const { env, db } = makeEnv();
    await applyMetadataMigrations(db as unknown as D1Database);
    await db.exec(`
      INSERT INTO registry_events (
        event_id,
        event_type,
        package_name,
        package_version,
        package_locator,
        payload_json,
        created_at
      ) VALUES
        ('evt-1', 'package.submitted', 'kernel', '0.0.1', '', '{}', '2026-03-30T12:00:00.000Z'),
        ('evt-2', 'package.verified', 'kernel', '0.0.1', '', '{}', '2026-03-30T12:01:00.000Z'),
        ('evt-3', 'package.indexed', 'kernel', '0.0.1', '', '{}', '2026-03-30T12:02:00.000Z')
    `);

    const response = await handleRequest(
      new Request("https://registry.test/v1/events?after=evt-1&limit=10"),
      env,
      new FakeExecutionContext(),
    );

    expect(response.status).toBe(200);
    expect(await readJson(response)).toEqual({
      limit: 10,
      after: "evt-1",
      events: [
        expect.objectContaining({ event_id: "evt-2" }),
        expect.objectContaining({ event_id: "evt-3" }),
      ],
    });
  });

  test("package events route returns package timeline and supports version filtering", async () => {
    const { env, db } = makeEnv();
    await applyMetadataMigrations(db as unknown as D1Database);
    await db.exec(`
      INSERT INTO registry_events (
        event_id,
        event_type,
        package_name,
        package_version,
        package_locator,
        payload_json,
        created_at
      ) VALUES
        ('evt-1', 'package.submitted', 'kernel', '0.0.1', '', '{}', '2026-03-30T12:00:00.000Z'),
        ('evt-2', 'package.published', 'kernel', '0.0.1', '', '{}', '2026-03-30T12:01:00.000Z'),
        ('evt-3', 'package.published', 'kernel', '0.1.0', '', '{}', '2026-03-30T12:02:00.000Z')
    `);

    const response = await handleRequest(
      new Request("https://registry.test/v1/packages/kernel/events?version=0.0.1&limit=10"),
      env,
      new FakeExecutionContext(),
    );

    expect(response.status).toBe(200);
    expect(await readJson(response)).toEqual({
      package_name: "kernel",
      package_version: "0.0.1",
      limit: 10,
      events: [
        expect.objectContaining({ event_id: "evt-2", package_version: "0.0.1" }),
        expect.objectContaining({ event_id: "evt-1", package_version: "0.0.1" }),
      ],
    });
  });

  test("artifact publish stores a package-root tarball, claim, release, and events", async () => {
    const { env, bucket, db, queue, indexedQueue } = makeEnv();
    const response = await publishArtifact(
      env,
      await makePackageArtifact({
        packageName: "std",
        packageVersion: "0.1.0",
        description: "The Riot standard library",
        license: "Apache-2.0",
        files: {
          "src/std.ml": "let hello = \"riot\"\n",
          "README.md": "# std\n",
        },
      }),
    );

    expect(response.status).toBe(200);
    const payload = await readJson(response) as PublishResponse;
    expect(payload.package_name).toBe("std");
    expect(payload.package_version).toBe("0.1.0");
    expect(payload.artifact_sha256).toMatch(/^[0-9a-f]{64}$/);

    const sourceObject = await bucket.get(payload.source_archive.key);
    expect(sourceObject).not.toBeNull();
    const sourceBytes = new Uint8Array(await sourceObject!.arrayBuffer());
    expect(await readArchiveFileFromTarGz(sourceBytes, "tusk.toml")).toContain('name = "std"');
    expect(await readArchiveFileFromTarGz(sourceBytes, "src/std.ml")).toBe('let hello = "riot"\n');
    expect(await readArchiveFileFromTarGz(sourceBytes, "repo-root/tusk.toml")).toBeNull();

    await applyMetadataMigrations(db as unknown as D1Database);
    expect(await readPackageClaim(db as unknown as D1Database, "std")).toMatchObject({
      package_name: "std",
      package_locator: "",
    });
    expect(await readPublishedRelease(db as unknown as D1Database, "std", "0.1.0")).toMatchObject({
      package_name: "std",
      artifact_sha256: payload.artifact_sha256,
      source_archive_key: payload.source_archive.key,
      manifest_key: payload.manifest.key,
    });

    const events = await listPackageRegistryEvents(db as unknown as D1Database, "std", "0.1.0", 10);
    expect(events.map((event) => event.event_type)).toEqual([
      "package.indexed",
      "package.searchable",
      "package.published",
      "package.verified",
      "package.submitted",
    ]);
    expect(events[2]?.payload).toMatchObject({
      artifact_sha256: payload.artifact_sha256,
      release_created: true,
    });

    expect(queue.messages).toHaveLength(1);
    expect(indexedQueue.messages).toHaveLength(1);
  });

  test("artifact publish short-circuits duplicate package versions for the same artifact", async () => {
    const { env, db, queue, indexedQueue } = makeEnv();
    const archive = await makePackageArtifact({
      packageName: "std",
      packageVersion: "0.1.0",
      description: "The Riot standard library",
      license: "Apache-2.0",
    });

    const first = await publishArtifact(env, archive);
    const firstPayload = await readJson(first) as PublishResponse;
    const second = await publishArtifact(env, archive);

    expect(first.status).toBe(200);
    expect(second.status).toBe(200);
    expect(await readJson(second)).toMatchObject({
      package_name: "std",
      package_version: "0.1.0",
      artifact_sha256: firstPayload.artifact_sha256,
      claim: { created: false },
      release: { created: false },
    });

    await applyMetadataMigrations(db as unknown as D1Database);
    const events = await listPackageRegistryEvents(db as unknown as D1Database, "std", "0.1.0", 10);
    expect(events).toHaveLength(5);
    expect(queue.messages).toHaveLength(1);
    expect(indexedQueue.messages).toHaveLength(1);
  });

  test("artifact publish rejects republishing the same version with a different artifact", async () => {
    const { env } = makeEnv();
    const first = await publishArtifact(env, await makePackageArtifact({
      packageName: "std",
      packageVersion: "0.1.0",
      description: "The Riot standard library",
      license: "Apache-2.0",
      files: {
        "src/std.ml": "let hello = \"riot\"\n",
      },
    }));
    expect(first.status).toBe(200);

    const second = await publishArtifact(env, await makePackageArtifact({
      packageName: "std",
      packageVersion: "0.1.0",
      description: "The Riot standard library, revised",
      license: "Apache-2.0",
      files: {
        "src/std.ml": "let hello = \"riot again\"\n",
      },
    }));

    expect(second.status).toBe(409);
    expect(await readJson(second)).toMatchObject({
      error: "package_version_already_published",
    });
  });

  test("artifact publish rejects requests without root auth", async () => {
    const { env } = makeEnv();
    const response = await handleRequest(
      new Request("https://registry.test/v1/publish", {
        method: "POST",
        headers: {
          "content-type": "application/gzip",
        },
        body: await makePackageArtifact({
          packageName: "std",
          packageVersion: "0.1.0",
          description: "The Riot standard library",
          license: "Apache-2.0",
        }),
      }),
      env,
      new FakeExecutionContext(),
    );

    expect(response.status).toBe(401);
    expect(await readJson(response)).toMatchObject({
      error: "unauthorized",
    });
  });

  test("artifact publish rejects unsupported media types", async () => {
    const { env } = makeEnv();
    const response = await handleRequest(
      new Request("https://registry.test/v1/publish", {
        method: "POST",
        headers: {
          authorization: "Bearer root-secret",
          "content-type": "application/json",
        },
        body: JSON.stringify({ nope: true }),
      }),
      env,
      new FakeExecutionContext(),
    );

    expect(response.status).toBe(415);
    expect(await readJson(response)).toMatchObject({
      error: "unsupported_media_type",
    });
  });

  test("artifact publish rejects non-public packages", async () => {
    const { env, db, queue, indexedQueue } = makeEnv();
    const response = await publishArtifact(env, await makePackageArtifact({
      packageName: "std",
      packageVersion: "0.1.0",
      description: "The Riot standard library",
      license: "Apache-2.0",
      publicPackage: false,
    }));

    expect(response.status).toBe(422);
    expect(await readJson(response)).toMatchObject({
      error: "package_not_public",
    });

    await applyMetadataMigrations(db as unknown as D1Database);
    expect(await readPackageClaim(db as unknown as D1Database, "std")).toBeNull();
    expect(await readPublishedRelease(db as unknown as D1Database, "std", "0.1.0")).toBeNull();
    expect(queue.messages).toHaveLength(0);
    expect(indexedQueue.messages).toHaveLength(0);
  });

  test("artifact publish accepts a new version of an existing package name", async () => {
    const { env, db, queue, indexedQueue } = makeEnv();
    expect((await publishArtifact(env, await makePackageArtifact({
      packageName: "std",
      packageVersion: "0.1.0",
      description: "The Riot standard library",
      license: "Apache-2.0",
    }))).status).toBe(200);

    const second = await publishArtifact(env, await makePackageArtifact({
      packageName: "std",
      packageVersion: "0.2.0",
      description: "The Riot standard library",
      license: "Apache-2.0",
    }));

    expect(second.status).toBe(200);
    await applyMetadataMigrations(db as unknown as D1Database);
    expect(await readPublishedRelease(db as unknown as D1Database, "std", "0.2.0")).not.toBeNull();
    expect(queue.messages).toHaveLength(2);
    expect(indexedQueue.messages).toHaveLength(2);
  });

  test("artifact publish rejects packages without a description", async () => {
    const { env } = makeEnv();
    const response = await publishArtifact(env, await makePackageArtifact({
      packageName: "std",
      packageVersion: "0.1.0",
      license: "Apache-2.0",
      omitDescription: true,
    }));

    expect(response.status).toBe(422);
    expect(await readJson(response)).toMatchObject({
      error: "missing_package_description",
    });
  });

  test("artifact publish rejects packages with non-SPDX licenses", async () => {
    const { env } = makeEnv();
    const response = await publishArtifact(env, await makePackageArtifact({
      packageName: "std",
      packageVersion: "0.1.0",
      description: "The Riot standard library",
      license: "lolnope",
    }));

    expect(response.status).toBe(422);
    expect(await readJson(response)).toMatchObject({
      error: "invalid_package_license",
    });
  });

  test("artifact publish rejects packages whose semver dependencies have not been published yet", async () => {
    const { env } = makeEnv();
    const response = await publishArtifact(env, await makePackageArtifact({
      packageName: "std",
      packageVersion: "0.1.0",
      description: "The Riot standard library",
      license: "Apache-2.0",
      dependencyLines: [
        "[dependencies]",
        'kernel = "0.0.1"',
      ],
    }));

    expect(response.status).toBe(422);
    expect(await readJson(response)).toMatchObject({
      error: "missing_dependency",
    });
  });

  test("artifact publish accepts dependencies on already-published packages", async () => {
    const { env } = makeEnv();
    expect((await publishArtifact(env, await makePackageArtifact({
      packageName: "kernel",
      packageVersion: "0.0.1",
      description: "Kernel primitives",
      license: "Apache-2.0",
    }))).status).toBe(200);

    const response = await publishArtifact(env, await makePackageArtifact({
      packageName: "std",
      packageVersion: "0.1.0",
      description: "The Riot standard library",
      license: "Apache-2.0",
      dependencyLines: [
        "[dependencies]",
        'kernel = "0.0.1"',
      ],
    }));

    expect(response.status).toBe(200);
  });

  test("artifact publish accepts built-in OCaml dependencies without registry lookup", async () => {
    const { env } = makeEnv();
    const response = await publishArtifact(env, await makePackageArtifact({
      packageName: "std",
      packageVersion: "0.1.0",
      description: "The Riot standard library",
      license: "Apache-2.0",
      dependencyLines: [
        "[dependencies]",
        'stdlib = "latest"',
        'unix = "latest"',
      ],
    }));

    expect(response.status).toBe(200);
  });

  test("artifact publish accepts git-based dependencies without registry lookup", async () => {
    const { env } = makeEnv();
    const response = await publishArtifact(env, await makePackageArtifact({
      packageName: "std",
      packageVersion: "0.1.0",
      description: "The Riot standard library",
      license: "Apache-2.0",
      dependencyLines: [
        "[dependencies]",
        'minttea = { github = "leostera/minttea" }',
      ],
    }));

    expect(response.status).toBe(200);
  });

  test("artifact publish rejects invalid dependency references", async () => {
    const { env } = makeEnv();
    const response = await publishArtifact(env, await makePackageArtifact({
      packageName: "std",
      packageVersion: "0.1.0",
      description: "The Riot standard library",
      license: "Apache-2.0",
      dependencyLines: [
        "[dependencies]",
        'minttea = { github = "" }',
      ],
    }));

    expect(response.status).toBe(422);
    expect(await readJson(response)).toMatchObject({
      error: "invalid_dependency_reference",
    });
  });
});

interface MakePackageArtifactOptions {
  packageName: string;
  packageVersion: string;
  description?: string;
  license?: string;
  publicPackage?: boolean;
  omitDescription?: boolean;
  dependencyLines?: string[];
  files?: Record<string, string>;
}

interface PublishResponse {
  package_name: string;
  package_version: string;
  artifact_sha256: string;
  manifest: {
    key: string;
  };
  source_archive: {
    key: string;
  };
  claim: {
    created: boolean;
  };
  release: {
    created: boolean;
  };
}

async function publishArtifact(env: ReturnType<typeof makeEnv>["env"], archive: Uint8Array<ArrayBuffer>) {
  const ctx = new FakeExecutionContext();
  const response = await handleRequest(
    new Request("https://registry.test/v1/publish", {
      method: "POST",
      headers: {
        authorization: "Bearer root-secret",
        "content-type": "application/gzip",
      },
      body: archive,
    }),
    env,
    ctx,
  );
  await ctx.drain();
  return response;
}

async function makePackageArtifact(options: MakePackageArtifactOptions): Promise<Uint8Array<ArrayBuffer>> {
  const packageLines = [
    "[package]",
    `name = "${options.packageName}"`,
    `version = "${options.packageVersion}"`,
    `public = ${options.publicPackage === false ? "false" : "true"}`,
  ];

  if (!options.omitDescription) {
    packageLines.push(`description = "${options.description ?? "Package description"}"`);
  }

  packageLines.push(`license = "${options.license ?? "Apache-2.0"}"`);

  const tuskToml = [
    ...packageLines,
    ...(options.dependencyLines ?? []),
  ].join("\n");

  return await makeTarGz({
    "tusk.toml": tuskToml,
    ...(options.files ?? {}),
  }, "");
}

async function readJson(response: Response): Promise<unknown> {
  return JSON.parse(await response.text());
}
