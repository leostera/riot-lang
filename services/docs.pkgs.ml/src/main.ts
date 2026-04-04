import {
  claimPackageReleaseToProcess,
  enqueuePackageReleaseToProcess,
  listDuePackageReleasesToProcess,
  markPackageReleaseToProcessFinished,
  readLatestPackageReleaseToProcess,
  readLatestPackagePipelineRun,
  reschedulePackageReleaseToProcess,
  writePackagePipelineRunRecord,
} from "../../api.pkgs.ml/src/pipeline-db.ts";
import { writeRegistryEvent } from "../../api.pkgs.ml/src/metadata-db.ts";
import type {
  DocsBuildRequest,
  PackageBuildRequest,
  PackagePipelineRunRecord,
  PackagePublishedEvent,
  RegistryEventType,
} from "../../api.pkgs.ml/src/types.ts";
import { v7 as uuidv7 } from "uuid";

interface AssetFetcher {
  fetch(request: Request): Response | Promise<Response>;
}

interface StoredObject {
  key: string;
  size: number;
  body: ReadableStream | null;
  httpEtag: string;
  writeHttpMetadata(headers: Headers): void;
}

interface ObjectBucket {
  get(key: string): Promise<StoredObject | null>;
  put(
    key: string,
    value: string | ArrayBuffer | ArrayBufferView,
    options?: { httpMetadata?: { contentType?: string } },
  ): Promise<unknown>;
}

export interface Env {
  ASSETS: AssetFetcher;
  ML_PKGS_CDN: ObjectBucket;
  SEARCH_DB: D1Database;
}

const TEXT_CONTENT_TYPE = "text/plain; charset=utf-8";
const PKGS_WEB_BASE_URL = "https://pkgs.ml";
const DOCS_WEB_BASE_URL = "https://docs.pkgs.ml";
const CDN_BASE_URL = "https://cdn.pkgs.ml";
const RIOT_INSTALL_SCRIPT_URL = "https://get.riot.ml";
const RIOT_RELEASE_METADATA_URL = `${CDN_BASE_URL}/riot/latest.json`;
const RELEASE_PROCESSING_BATCH_SIZE = 10;
const RELEASE_PROCESSING_LEASE_MS = 5 * 60 * 1000;
const RELEASE_PROCESSING_RETRY_DELAY_MS = 5 * 60 * 1000;

export default {
  async fetch(request: Request, env: Env): Promise<Response> {
    return await handleRequest(request, env);
  },

  async queue(batch: MessageBatch<PackagePublishedEvent>, env: Env, _ctx: ExecutionContext): Promise<void> {
    for (const message of batch.messages) {
      await enqueuePackageReleaseToProcess(env.SEARCH_DB, message.body);
      await writeRegistryEvent(
        env.SEARCH_DB,
        makePipelineEvent("package.processing.queued", new Date().toISOString(), message.body, {
          status: "pending",
        }),
      );
      message.ack();
    }
  },

  async scheduled(_controller: ScheduledController, env: Env, ctx: ExecutionContext): Promise<void> {
    ctx.waitUntil(processQueuedReleases(env));
  },
};

async function handleRequest(request: Request, env: Env): Promise<Response> {
  if (request.method !== "GET" && request.method !== "HEAD") {
    return new Response("Method Not Allowed", {
      status: 405,
      headers: {
        allow: "GET, HEAD",
        "content-type": TEXT_CONTENT_TYPE,
      },
    });
  }

  const url = new URL(request.url);
  const match = matchPackageDocsPath(url.pathname);

  if (match === null) {
    return redirectToPkgs(url);
  }

  if (!url.pathname.endsWith("/") && match.rest.length === 0) {
    url.pathname = `${url.pathname}/`;
    return Response.redirect(url.toString(), 308);
  }

  const objectKey = resolveDocsObjectKey(match.packageName, match.version, match.rest);
  const object = await env.ML_PKGS_CDN.get(objectKey);
  if (object === null) {
    const run = await readLatestPackagePipelineRun(env.SEARCH_DB, match.packageName, match.version, "docs");
    if (run !== null) {
      return new Response(
        [
          `Package docs for ${match.packageName}@${match.version} have not been generated yet.`,
          `Current pipeline status: ${run.status}.`,
          run.status_message ?? "A container-backed docs runner has not claimed this run yet.",
        ].join(" "),
        {
          status: 404,
          headers: {
            "content-type": TEXT_CONTENT_TYPE,
            "cache-control": "no-store",
          },
        },
      );
    }

    const pendingRelease = await readLatestPackageReleaseToProcess(
      env.SEARCH_DB,
      match.packageName,
      match.version,
    );
    if (pendingRelease !== null) {
      return new Response(
        [
          `Package docs for ${match.packageName}@${match.version} have not been generated yet.`,
          `Current release-processing status: ${pendingRelease.status}.`,
          pendingRelease.status_message ?? "The release is waiting for the docs pipeline timer worker.",
        ].join(" "),
        {
          status: 404,
          headers: {
            "content-type": TEXT_CONTENT_TYPE,
            "cache-control": "no-store",
          },
        },
      );
    }

    return new Response("Package docs not found", {
      status: 404,
      headers: {
        "content-type": TEXT_CONTENT_TYPE,
      },
    });
  }

  return await respondWithObject(request, object);
}

function redirectToPkgs(url: URL): Response {
  const target = new URL(PKGS_WEB_BASE_URL);
  target.search = url.search;
  return Response.redirect(target.toString(), 302);
}

function matchPackageDocsPath(pathname: string):
  | { packageName: string; version: string; rest: string }
  | null {
  const segments = pathname.split("/").filter((segment) => segment.length > 0);
  if (segments[0] !== "p" || segments.length < 3) {
    return null;
  }

  const packageName = decodeURIComponent(segments[1] ?? "");
  const version = decodeURIComponent(segments[2] ?? "");
  const rest = segments.slice(3).map((segment) => decodeURIComponent(segment)).join("/");

  if (packageName.length === 0 || version.length === 0) {
    return null;
  }

  return {
    packageName,
    version,
    rest,
  };
}

function resolveDocsObjectKey(packageName: string, version: string, rest: string): string {
  if (rest.length === 0) {
    return `docs/${packageName}/${version}/index.html`;
  }

  if (rest.endsWith("/")) {
    return `docs/${packageName}/${version}/${rest}index.html`;
  }

  if (!rest.includes(".") && !rest.endsWith(".html")) {
    return `docs/${packageName}/${version}/${rest}/index.html`;
  }

  return `docs/${packageName}/${version}/${rest}`;
}

async function respondWithObject(request: Request, object: StoredObject): Promise<Response> {
  const etag = object.httpEtag;
  if (request.headers.get("if-none-match") === etag) {
    return new Response(null, {
      status: 304,
      headers: {
        etag,
        "cache-control": "public, max-age=300",
      },
    });
  }

  const headers = new Headers();
  object.writeHttpMetadata(headers);
  if (!headers.has("content-type")) {
    const fallbackContentType = contentTypeForKey(object.key);
    if (fallbackContentType !== null) {
      headers.set("content-type", fallbackContentType);
    }
  }
  headers.set("cache-control", cacheControlForKey(object.key));
  headers.set("etag", etag);
  headers.set("content-length", String(object.size));

  if (request.method === "HEAD") {
    return new Response(null, {
      status: 200,
      headers,
    });
  }

  return new Response(object.body, {
    status: 200,
    headers,
  });
}

function cacheControlForKey(key: string): string {
  if (key.endsWith(".html")) {
    return "public, max-age=300";
  }

  return "public, max-age=31536000, immutable";
}

function contentTypeForKey(key: string): string | null {
  if (key.endsWith(".html")) return "text/html; charset=utf-8";
  if (key.endsWith(".css")) return "text/css; charset=utf-8";
  if (key.endsWith(".js")) return "text/javascript; charset=utf-8";
  if (key.endsWith(".json")) return "application/json; charset=utf-8";
  if (key.endsWith(".svg")) return "image/svg+xml";
  if (key.endsWith(".txt")) return "text/plain; charset=utf-8";
  if (key.endsWith(".xml")) return "application/xml; charset=utf-8";
  if (key.endsWith(".wasm")) return "application/wasm";
  if (key.endsWith(".ico")) return "image/x-icon";
  if (key.endsWith(".png")) return "image/png";
  if (key.endsWith(".jpg") || key.endsWith(".jpeg")) return "image/jpeg";
  if (key.endsWith(".webp")) return "image/webp";
  return null;
}

async function stageDocsBuild(
  env: Env,
  event: PackagePublishedEvent,
  createdAt = new Date().toISOString(),
): Promise<void> {
  const now = createdAt;
  const runId = `docs:${event.package_name}:${event.package_version}:${event.artifact_sha256}`;
  const outputPrefix = `docs/${event.package_name}/${event.package_version}/`;
  const requestKey = `${outputPrefix}_pipeline/request.json`;
  const sourceArchiveUrl = `${CDN_BASE_URL}/${event.source_archive_key}`;
  const publicDocsUrl = `${DOCS_WEB_BASE_URL}/p/${encodeURIComponent(event.package_name)}/${encodeURIComponent(event.package_version)}/`;
  const notes = buildRunnerNotes();

  const request: DocsBuildRequest = {
    run_id: runId,
    run_kind: "docs",
    package_name: event.package_name,
    package_version: event.package_version,
    artifact_sha256: event.artifact_sha256,
    source_archive_key: event.source_archive_key,
    source_archive_url: sourceArchiveUrl,
    output_prefix: outputPrefix,
    riot_install_url: RIOT_INSTALL_SCRIPT_URL,
    riot_release_metadata_url: RIOT_RELEASE_METADATA_URL,
    public_docs_url: publicDocsUrl,
    command: ["riot", "doc"],
    runner: {
      kind: "cloudflare-container",
      status: "pending_runner",
      notes,
    },
    steps: [
      {
        kind: "download",
        detail: `Download the package artifact from ${sourceArchiveUrl}.`,
      },
      {
        kind: "unpack",
        detail: "Extract the published package-root artifact into a clean workspace directory.",
      },
      {
        kind: "install-riot",
        detail: "Install Riot inside the sandboxed runtime so `riot doc` is available.",
      },
      {
        kind: "generate-docs",
        detail: "Run `riot doc` in the unpacked package workspace.",
      },
      {
        kind: "upload",
        detail: `Upload the generated static site into ${outputPrefix}.`,
      },
    ],
    created_at: now,
  };

  await env.ML_PKGS_CDN.put(requestKey, JSON.stringify(request, null, 2), {
    httpMetadata: {
      contentType: "application/json; charset=utf-8",
    },
  });

  const record: PackagePipelineRunRecord = {
    run_id: runId,
    run_kind: "docs",
    package_name: event.package_name,
    package_version: event.package_version,
    artifact_sha256: event.artifact_sha256,
    source_archive_key: event.source_archive_key,
    runner_kind: "cloudflare-container",
    status: "staged",
    output_prefix: outputPrefix,
    request_key: requestKey,
    created_at: now,
    updated_at: now,
    status_message: "Docs build request staged; awaiting a container-backed runner.",
    metadata: {
      package_locator: event.package_locator,
      source_url: event.source_url,
      package_subdir: event.package_subdir,
      source_archive_url: sourceArchiveUrl,
      public_docs_url: publicDocsUrl,
      notes,
    },
  };

  await writePackagePipelineRunRecord(env.SEARCH_DB, record);
  await writeRegistryEvent(
    env.SEARCH_DB,
    makePipelineEvent("package.docs.staged", now, event, {
      run_kind: "docs",
      request_key: requestKey,
      output_prefix: outputPrefix,
    }),
  );
}

async function stageBuildVerification(
  env: Env,
  event: PackagePublishedEvent,
  createdAt = new Date().toISOString(),
): Promise<void> {
  const now = createdAt;
  const runId = `build:${event.package_name}:${event.package_version}:${event.artifact_sha256}`;
  const outputPrefix = `pipelines/builds/${event.package_name}/${event.package_version}/${event.artifact_sha256}/`;
  const requestKey = `${outputPrefix}request.json`;
  const resultKey = `${outputPrefix}result.json`;
  const logsKey = `${outputPrefix}build.log`;
  const sourceArchiveUrl = `${CDN_BASE_URL}/${event.source_archive_key}`;
  const notes = buildRunnerNotes();

  const request: PackageBuildRequest = {
    run_id: runId,
    run_kind: "build",
    package_name: event.package_name,
    package_version: event.package_version,
    artifact_sha256: event.artifact_sha256,
    source_archive_key: event.source_archive_key,
    source_archive_url: sourceArchiveUrl,
    output_prefix: outputPrefix,
    riot_install_url: RIOT_INSTALL_SCRIPT_URL,
    riot_release_metadata_url: RIOT_RELEASE_METADATA_URL,
    result_key: resultKey,
    logs_key: logsKey,
    command: ["riot", "build"],
    runner: {
      kind: "cloudflare-container",
      status: "pending_runner",
      notes,
    },
    steps: [
      {
        kind: "download",
        detail: `Download the package artifact from ${sourceArchiveUrl}.`,
      },
      {
        kind: "unpack",
        detail: "Extract the published package-root artifact into a clean workspace directory.",
      },
      {
        kind: "install-riot",
        detail: `Install Riot inside the sandboxed runtime by fetching ${RIOT_INSTALL_SCRIPT_URL}.`,
      },
      {
        kind: "build-package",
        detail: "Run `riot build` in the unpacked package workspace to verify that the published artifact builds in isolation.",
      },
      {
        kind: "upload-report",
        detail: `Upload stdout and stderr logs to ${logsKey} and the structured result summary to ${resultKey}.`,
      },
    ],
    created_at: now,
  };

  await env.ML_PKGS_CDN.put(requestKey, JSON.stringify(request, null, 2), {
    httpMetadata: {
      contentType: "application/json; charset=utf-8",
    },
  });

  const record: PackagePipelineRunRecord = {
    run_id: runId,
    run_kind: "build",
    package_name: event.package_name,
    package_version: event.package_version,
    artifact_sha256: event.artifact_sha256,
    source_archive_key: event.source_archive_key,
    runner_kind: "cloudflare-container",
    status: "staged",
    output_prefix: outputPrefix,
    request_key: requestKey,
    created_at: now,
    updated_at: now,
    status_message: "Build verification request staged; awaiting a container-backed runner.",
    metadata: {
      package_locator: event.package_locator,
      source_url: event.source_url,
      package_subdir: event.package_subdir,
      source_archive_url: sourceArchiveUrl,
      riot_install_url: RIOT_INSTALL_SCRIPT_URL,
      riot_release_metadata_url: RIOT_RELEASE_METADATA_URL,
      result_key: resultKey,
      logs_key: logsKey,
      notes,
    },
  };

  await writePackagePipelineRunRecord(env.SEARCH_DB, record);
  await writeRegistryEvent(
    env.SEARCH_DB,
    makePipelineEvent("package.build.staged", now, event, {
      run_kind: "build",
      request_key: requestKey,
      output_prefix: outputPrefix,
      result_key: resultKey,
      logs_key: logsKey,
    }),
  );
}

async function processQueuedReleases(env: Env): Promise<void> {
  const now = new Date().toISOString();
  const dueReleases = await listDuePackageReleasesToProcess(
    env.SEARCH_DB,
    now,
    RELEASE_PROCESSING_BATCH_SIZE,
  );

  for (const release of dueReleases) {
    const claimed = await claimPackageReleaseToProcess(
      env.SEARCH_DB,
      release.release_id,
      now,
      toIso(now, RELEASE_PROCESSING_LEASE_MS),
    );
    if (claimed === null) {
      continue;
    }

    try {
      const startedAt = new Date().toISOString();
      const docsStagedAt = toIso(startedAt, 1);
      const buildStagedAt = toIso(startedAt, 2);
      const finishedAt = toIso(startedAt, 3);
      await writeRegistryEvent(
        env.SEARCH_DB,
        makePipelineEvent("package.processing.started", startedAt, claimed.payload, {
          status: "processing",
          attempt_count: claimed.attempt_count + 1,
        }),
      );
      await stageDocsBuild(env, claimed.payload, docsStagedAt);
      await stageBuildVerification(env, claimed.payload, buildStagedAt);
      await markPackageReleaseToProcessFinished(
        env.SEARCH_DB,
        claimed.release_id,
        finishedAt,
        "Docs and build-verification requests staged for a future container-backed runner.",
      );
      await writeRegistryEvent(
        env.SEARCH_DB,
        makePipelineEvent("package.processing.finished", finishedAt, claimed.payload, {
          status: "finished",
        }),
      );
    } catch (error) {
      const failedAt = new Date().toISOString();
      await reschedulePackageReleaseToProcess(
        env.SEARCH_DB,
        claimed.release_id,
        failedAt,
        toIso(failedAt, RELEASE_PROCESSING_RETRY_DELAY_MS),
        normalizePipelineError(error),
      );
      await writeRegistryEvent(
        env.SEARCH_DB,
        makePipelineEvent("package.processing.requeued", failedAt, claimed.payload, {
          status: "pending",
          next_attempt_at: toIso(failedAt, RELEASE_PROCESSING_RETRY_DELAY_MS),
          error: error instanceof Error ? error.message : "unknown_error",
        }),
      );
    }
  }
}

function buildRunnerNotes(): string[] {
  return [
    "Standard Cloudflare Workers cannot execute arbitrary binaries like `riot` directly.",
    "A future Container-backed or Sandbox-backed runner should claim this request, download the published artifact, unpack it into a clean workspace, install Riot, and execute the requested command.",
    "Cloudflare bindings such as R2 and D1 live on the Worker or Durable Object side; the container process itself should communicate with that companion layer rather than expecting native bucket bindings.",
  ];
}

function normalizePipelineError(error: unknown): string {
  if (error instanceof Error) {
    return `Release processing failed and was requeued: ${error.message}`;
  }

  return "Release processing failed and was requeued for another timer pass.";
}

function toIso(baseIso: string, deltaMs: number): string {
  return new Date(new Date(baseIso).getTime() + deltaMs).toISOString();
}

function makePipelineEvent(
  eventType: RegistryEventType,
  createdAt: string,
  event: PackagePublishedEvent,
  payload: Record<string, unknown>,
) {
  return {
    event_id: uuidv7({
      msecs: Date.parse(createdAt),
    }),
    event_type: eventType,
    package_name: event.package_name,
    package_version: event.package_version,
    package_locator: event.package_locator.length === 0 ? undefined : event.package_locator,
    payload: {
      artifact_sha256: event.artifact_sha256,
      ...payload,
    },
    created_at: createdAt,
  } as const;
}
