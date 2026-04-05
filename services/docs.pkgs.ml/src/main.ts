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
  PackagePipelineRunStatus,
  PackagePublishedEvent,
  RegistryEventType,
} from "../../api.pkgs.ml/src/types.ts";
import { Buffer } from "node:buffer";
import { v7 as uuidv7 } from "uuid";
import type { DocsPipelineProcessResult, PackagePipelineExecutor } from "./pipeline-types.ts";

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
  DOCS_PIPELINE_CONTAINER?: DurableObjectNamespace;
  PIPELINE_EXECUTOR?: PackagePipelineExecutor;
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

interface DocsRunContext {
  runId: string;
  outputPrefix: string;
  requestKey: string;
  sourceArchiveUrl: string;
  publicDocsUrl: string;
  request: DocsBuildRequest;
}

interface BuildRunContext {
  runId: string;
  outputPrefix: string;
  requestKey: string;
  resultKey: string;
  logsKey: string;
  sourceArchiveUrl: string;
  request: PackageBuildRequest;
}

function buildRunnerNotes(): string[] {
  return [
    "The timer worker claims a queued release, records the run in D1, and hands the artifact URL to a dedicated container runner.",
    "The container downloads the published package artifact, installs Riot with `curl -sSL https://get.riot.ml | sh -`, unpacks it into a clean workspace, and runs `riot doc --release` and/or `riot build`.",
    "Only the Worker writes to R2 and D1. The container never receives bucket credentials directly.",
  ];
}

function buildDocsRunContext(
  event: PackagePublishedEvent,
  createdAt = new Date().toISOString(),
): DocsRunContext {
  const outputPrefix = `docs/${event.package_name}/${event.package_version}/`;
  const requestKey = `${outputPrefix}_pipeline/request.json`;
  const sourceArchiveUrl = `${CDN_BASE_URL}/${event.source_archive_key}`;
  const publicDocsUrl = `${DOCS_WEB_BASE_URL}/p/${encodeURIComponent(event.package_name)}/${encodeURIComponent(event.package_version)}/`;

  return {
    runId: `docs:${event.package_name}:${event.package_version}:${event.artifact_sha256}`,
    outputPrefix,
    requestKey,
    sourceArchiveUrl,
    publicDocsUrl,
    request: {
      run_id: `docs:${event.package_name}:${event.package_version}:${event.artifact_sha256}`,
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
      command: ["riot", "doc", "--release", "-p", event.package_name],
      runner: {
        kind: "cloudflare-container",
        status: "pending_runner",
        notes: buildRunnerNotes(),
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
          detail: `Install Riot inside the container by fetching ${RIOT_INSTALL_SCRIPT_URL}.`,
        },
        {
          kind: "generate-docs",
          detail: "Run `riot doc --release -p <package>` in the unpacked package workspace.",
        },
        {
          kind: "upload",
          detail: `Upload the generated static site into ${outputPrefix}.`,
        },
      ],
      created_at: createdAt,
    },
  };
}

function buildBuildRunContext(
  event: PackagePublishedEvent,
  createdAt = new Date().toISOString(),
): BuildRunContext {
  const outputPrefix = `pipelines/builds/${event.package_name}/${event.package_version}/${event.artifact_sha256}/`;
  const requestKey = `${outputPrefix}request.json`;
  const resultKey = `${outputPrefix}result.json`;
  const logsKey = `${outputPrefix}build.log`;
  const sourceArchiveUrl = `${CDN_BASE_URL}/${event.source_archive_key}`;

  return {
    runId: `build:${event.package_name}:${event.package_version}:${event.artifact_sha256}`,
    outputPrefix,
    requestKey,
    resultKey,
    logsKey,
    sourceArchiveUrl,
    request: {
      run_id: `build:${event.package_name}:${event.package_version}:${event.artifact_sha256}`,
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
      command: ["riot", "build", event.package_name],
      runner: {
        kind: "cloudflare-container",
        status: "pending_runner",
        notes: buildRunnerNotes(),
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
          detail: `Install Riot inside the container by fetching ${RIOT_INSTALL_SCRIPT_URL}.`,
        },
        {
          kind: "build-package",
          detail: "Run `riot build <package>` in the unpacked package workspace to verify the published artifact builds in isolation.",
        },
        {
          kind: "upload-report",
          detail: `Upload stdout and stderr logs to ${logsKey} and the structured result summary to ${resultKey}.`,
        },
      ],
      created_at: createdAt,
    },
  };
}

function buildRunRecord(
  event: PackagePublishedEvent,
  runKind: "docs" | "build",
  status: PackagePipelineRunStatus,
  createdAt: string,
  requestKey: string,
  outputPrefix: string,
  statusMessage: string,
  metadata: Record<string, unknown>,
): PackagePipelineRunRecord {
  return {
    run_id: `${runKind}:${event.package_name}:${event.package_version}:${event.artifact_sha256}`,
    run_kind: runKind,
    package_name: event.package_name,
    package_version: event.package_version,
    artifact_sha256: event.artifact_sha256,
    source_archive_key: event.source_archive_key,
    runner_kind: "cloudflare-container",
    status,
    output_prefix: outputPrefix,
    request_key: requestKey,
    created_at: createdAt,
    updated_at: createdAt,
    started_at: status === "running" ? createdAt : undefined,
    finished_at: status === "succeeded" || status === "failed" ? createdAt : undefined,
    status_message: statusMessage,
    metadata: {
      package_locator: event.package_locator,
      source_url: event.source_url,
      package_subdir: event.package_subdir,
      ...metadata,
    },
  };
}

async function putJson(
  env: Env,
  key: string,
  payload: unknown,
): Promise<void> {
  await env.ML_PKGS_CDN.put(key, JSON.stringify(payload, null, 2), {
    httpMetadata: {
      contentType: "application/json; charset=utf-8",
    },
  });
}

async function putText(
  env: Env,
  key: string,
  body: string,
  contentType: string,
): Promise<void> {
  await env.ML_PKGS_CDN.put(key, body, {
    httpMetadata: {
      contentType,
    },
  });
}

function decodeBase64Bytes(contentBase64: string): Uint8Array {
  return new Uint8Array(Buffer.from(contentBase64, "base64"));
}

async function writeDocsArtifacts(
  env: Env,
  docs: NonNullable<DocsPipelineProcessResult["docs"]>,
  docsRun: DocsRunContext,
): Promise<string[]> {
  const uploadedKeys: string[] = [];

  for (const file of docs.files) {
    const key = `${docsRun.outputPrefix}${file.path}`;
    await env.ML_PKGS_CDN.put(key, decodeBase64Bytes(file.content_base64), {
      httpMetadata: {
        contentType: file.content_type ?? contentTypeForKey(key) ?? "application/octet-stream",
      },
    });
    uploadedKeys.push(key);
  }

  return uploadedKeys;
}

async function writeBuildArtifacts(
  env: Env,
  event: PackagePublishedEvent,
  build: NonNullable<DocsPipelineProcessResult["build"]>,
  buildRun: BuildRunContext,
): Promise<void> {
  const logBody = [
    `$ ${build.command.join(" ")}`,
    "",
    build.stdout,
    build.stderr.length > 0 ? `\n[stderr]\n${build.stderr}` : "",
  ].join("\n");

  await putText(env, buildRun.logsKey, logBody, "text/plain; charset=utf-8");
  await putJson(env, buildRun.resultKey, {
    package_name: event.package_name,
    package_version: event.package_version,
    artifact_sha256: event.artifact_sha256,
    source_archive_key: event.source_archive_key,
    generated_at: new Date().toISOString(),
    ...build,
  });
}

async function recordDocsRunStarted(
  env: Env,
  event: PackagePublishedEvent,
  docsRun: DocsRunContext,
  startedAt: string,
): Promise<void> {
  await putJson(env, docsRun.requestKey, docsRun.request);
  await writePackagePipelineRunRecord(
    env.SEARCH_DB,
    buildRunRecord(
      event,
      "docs",
      "running",
      startedAt,
      docsRun.requestKey,
      docsRun.outputPrefix,
      "Docs generation is running in a container-backed worker.",
      {
        source_archive_url: docsRun.sourceArchiveUrl,
        public_docs_url: docsRun.publicDocsUrl,
      },
    ),
  );
}

async function recordBuildRunStarted(
  env: Env,
  event: PackagePublishedEvent,
  buildRun: BuildRunContext,
  startedAt: string,
): Promise<void> {
  await putJson(env, buildRun.requestKey, buildRun.request);
  await writePackagePipelineRunRecord(
    env.SEARCH_DB,
    buildRunRecord(
      event,
      "build",
      "running",
      startedAt,
      buildRun.requestKey,
      buildRun.outputPrefix,
      "Build verification is running in a container-backed worker.",
      {
        source_archive_url: buildRun.sourceArchiveUrl,
        result_key: buildRun.resultKey,
        logs_key: buildRun.logsKey,
      },
    ),
  );
}

async function isDocsSatisfied(env: Env, event: PackagePublishedEvent): Promise<boolean> {
  const run = await readLatestPackagePipelineRun(env.SEARCH_DB, event.package_name, event.package_version, "docs");
  if (run?.status !== "succeeded") {
    return false;
  }

  return (await env.ML_PKGS_CDN.get(`docs/${event.package_name}/${event.package_version}/index.html`)) !== null;
}

async function isBuildSatisfied(env: Env, event: PackagePublishedEvent): Promise<boolean> {
  const run = await readLatestPackagePipelineRun(env.SEARCH_DB, event.package_name, event.package_version, "build");
  if (run?.status !== "succeeded") {
    return false;
  }

  return (await env.ML_PKGS_CDN.get(`pipelines/builds/${event.package_name}/${event.package_version}/${event.artifact_sha256}/result.json`)) !== null;
}

async function selectPipelineExecutor(env: Env): Promise<PackagePipelineExecutor> {
  if (env.PIPELINE_EXECUTOR !== undefined) {
    return env.PIPELINE_EXECUTOR;
  }

  const { ContainerPackagePipelineExecutor } = await import("./pipeline-executor.ts");
  return new ContainerPackagePipelineExecutor(env);
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

    const event = claimed.payload;
    const docsRun = buildDocsRunContext(event);
    const buildRun = buildBuildRunContext(event);
    const releaseStartedAt = new Date().toISOString();
    const shouldGenerateDocs = !(await isDocsSatisfied(env, event));
    const shouldVerifyBuild = !(await isBuildSatisfied(env, event));

    try {
      await writeRegistryEvent(
        env.SEARCH_DB,
        makePipelineEvent("package.processing.started", releaseStartedAt, event, {
          status: "processing",
          attempt_count: claimed.attempt_count,
        }),
      );

      if (!shouldGenerateDocs && !shouldVerifyBuild) {
        await markPackageReleaseToProcessFinished(
          env.SEARCH_DB,
          claimed.release_id,
          new Date().toISOString(),
          "Package docs and build verification already completed.",
        );
        await writeRegistryEvent(
          env.SEARCH_DB,
          makePipelineEvent("package.processing.finished", new Date().toISOString(), event, {
            status: "finished",
          }),
        );
        continue;
      }

      if (shouldGenerateDocs) {
        await recordDocsRunStarted(env, event, docsRun, toIso(releaseStartedAt, 1));
      }

      if (shouldVerifyBuild) {
        await recordBuildRunStarted(env, event, buildRun, toIso(releaseStartedAt, 2));
      }

      const execution = await (await selectPipelineExecutor(env)).processRelease({
        package_name: event.package_name,
        package_version: event.package_version,
        artifact_sha256: event.artifact_sha256,
        source_archive_key: event.source_archive_key,
        source_archive_url: `${CDN_BASE_URL}/${event.source_archive_key}`,
        riot_install_url: RIOT_INSTALL_SCRIPT_URL,
        riot_release_metadata_url: RIOT_RELEASE_METADATA_URL,
        generate_docs: shouldGenerateDocs,
        verify_build: shouldVerifyBuild,
      });

      let releaseError: string | null = null;

      if (shouldGenerateDocs) {
        const docsFinishedAt = new Date().toISOString();
        const docs = execution.docs;

        if (docs === undefined) {
          releaseError = "docs runner returned no docs result";
        } else if (!docs.success) {
          releaseError = docs.stderr.length > 0 ? docs.stderr : "riot doc failed";
          await writePackagePipelineRunRecord(
            env.SEARCH_DB,
            {
              ...buildRunRecord(
                event,
                "docs",
                "failed",
                docsFinishedAt,
                docsRun.requestKey,
                docsRun.outputPrefix,
                "Docs generation failed in the container-backed runner.",
                {
                  source_archive_url: docsRun.sourceArchiveUrl,
                  public_docs_url: docsRun.publicDocsUrl,
                  stdout: docs.stdout,
                  stderr: docs.stderr,
                  exit_code: docs.exit_code,
                },
              ),
              started_at: releaseStartedAt,
            },
          );
          await writeRegistryEvent(
            env.SEARCH_DB,
            makePipelineEvent("package.docs.failed", docsFinishedAt, event, {
              run_kind: "docs",
              exit_code: docs.exit_code,
            }),
          );
        } else {
          const uploadedKeys = await writeDocsArtifacts(env, docs, docsRun);
          await writePackagePipelineRunRecord(
            env.SEARCH_DB,
            {
              ...buildRunRecord(
                event,
                "docs",
                "succeeded",
                docsFinishedAt,
                docsRun.requestKey,
                docsRun.outputPrefix,
                "Package docs generated and uploaded successfully.",
                {
                  source_archive_url: docsRun.sourceArchiveUrl,
                  public_docs_url: docsRun.publicDocsUrl,
                  uploaded_keys: uploadedKeys,
                  output_dir: docs.output_dir,
                  stdout: docs.stdout,
                  stderr: docs.stderr,
                  exit_code: docs.exit_code,
                },
              ),
              started_at: releaseStartedAt,
            },
          );
          await writeRegistryEvent(
            env.SEARCH_DB,
            makePipelineEvent("package.docs.generated", docsFinishedAt, event, {
              run_kind: "docs",
              output_prefix: docsRun.outputPrefix,
              public_docs_url: docsRun.publicDocsUrl,
            }),
          );
        }
      }

      if (releaseError === null && shouldVerifyBuild) {
        const buildFinishedAt = new Date().toISOString();
        const build = execution.build;

        if (build === undefined) {
          releaseError = "build runner returned no build result";
        } else if (!build.success) {
          releaseError = build.stderr.length > 0 ? build.stderr : "riot build failed";
          await writeBuildArtifacts(env, event, build, buildRun);
          await writePackagePipelineRunRecord(
            env.SEARCH_DB,
            {
              ...buildRunRecord(
                event,
                "build",
                "failed",
                buildFinishedAt,
                buildRun.requestKey,
                buildRun.outputPrefix,
                "Build verification failed in the container-backed runner.",
                {
                  source_archive_url: buildRun.sourceArchiveUrl,
                  result_key: buildRun.resultKey,
                  logs_key: buildRun.logsKey,
                  stdout: build.stdout,
                  stderr: build.stderr,
                  exit_code: build.exit_code,
                },
              ),
              started_at: releaseStartedAt,
            },
          );
          await writeRegistryEvent(
            env.SEARCH_DB,
            makePipelineEvent("package.build.failed", buildFinishedAt, event, {
              run_kind: "build",
              result_key: buildRun.resultKey,
              logs_key: buildRun.logsKey,
              exit_code: build.exit_code,
            }),
          );
        } else {
          await writeBuildArtifacts(env, event, build, buildRun);
          await writePackagePipelineRunRecord(
            env.SEARCH_DB,
            {
              ...buildRunRecord(
                event,
                "build",
                "succeeded",
                buildFinishedAt,
                buildRun.requestKey,
                buildRun.outputPrefix,
                "Build verification completed successfully.",
                {
                  source_archive_url: buildRun.sourceArchiveUrl,
                  result_key: buildRun.resultKey,
                  logs_key: buildRun.logsKey,
                  stdout: build.stdout,
                  stderr: build.stderr,
                  exit_code: build.exit_code,
                },
              ),
              started_at: releaseStartedAt,
            },
          );
          await writeRegistryEvent(
            env.SEARCH_DB,
            makePipelineEvent("package.build.verified", buildFinishedAt, event, {
              run_kind: "build",
              result_key: buildRun.resultKey,
              logs_key: buildRun.logsKey,
            }),
          );
        }
      }

      if (releaseError !== null) {
        throw new Error(releaseError);
      }

      const finishedAt = new Date().toISOString();
      await markPackageReleaseToProcessFinished(
        env.SEARCH_DB,
        claimed.release_id,
        finishedAt,
        "Package docs and build verification completed successfully.",
      );
      await writeRegistryEvent(
        env.SEARCH_DB,
        makePipelineEvent("package.processing.finished", finishedAt, event, {
          status: "finished",
        }),
      );
    } catch (error) {
      const failedAt = new Date().toISOString();
      const nextAttemptAt = toIso(failedAt, RELEASE_PROCESSING_RETRY_DELAY_MS);
      await reschedulePackageReleaseToProcess(
        env.SEARCH_DB,
        claimed.release_id,
        failedAt,
        nextAttemptAt,
        normalizePipelineError(error),
      );
      await writeRegistryEvent(
        env.SEARCH_DB,
        makePipelineEvent("package.processing.requeued", failedAt, event, {
          status: "pending",
          next_attempt_at: nextAttemptAt,
          error: error instanceof Error ? error.message : "unknown_error",
        }),
      );
    }
  }
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
