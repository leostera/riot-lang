import { getConfig } from "./config.ts";
import { HttpError } from "./errors.ts";
import { immutableHeaders, json, methodNotAllowed } from "./http.ts";
import {
  canonicalSourceUrl,
  isFullSha,
  normalizeLocator,
  packageSubdir,
} from "./locator.ts";
import { readCachedMaterialization } from "./publication.ts";
import { writeRequestLog } from "./request-log.ts";
import {
  manifestKey,
  manifestRoutePath,
  prettyManifestUrl,
  prettySourceUrl,
  sourceArchiveKey,
  sourceRoutePath,
} from "./storage.ts";
import type { Env, PackageLocator, PublishedPackageRelease, RequestLogEntry } from "./types.ts";

export async function handleRequest(
  request: Request,
  env: Env,
  ctx: ExecutionContext,
): Promise<Response> {
  const url = new URL(request.url);
  const logEntry: RequestLogEntry = {
    request_id: crypto.randomUUID(),
    request_timestamp: new Date().toISOString(),
    method: request.method,
    path: url.pathname,
    route: "unknown",
    status: 500,
    success: false,
    user_agent: request.headers.get("user-agent"),
  };

  try {
    const response = await routeRequest(request, env, logEntry);
    logEntry.status = response.status;
    logEntry.success = response.ok;
    return response;
  } catch (error) {
    const httpError = normalizeError(error);
    logEntry.status = httpError.status;
    logEntry.error_category = httpError.error;
    logEntry.error_message = httpError.message;

    return json(
      {
        error: httpError.error,
        message: httpError.message,
        request_id: logEntry.request_id,
      },
      { status: httpError.status },
    );
  } finally {
    ctx.waitUntil(writeRequestLog(env, logEntry));
  }
}

async function routeRequest(
  request: Request,
  env: Env,
  logEntry: RequestLogEntry,
): Promise<Response> {
  const url = new URL(request.url);
  const path = trimSlashes(url.pathname);

  if (path === "") {
    if (request.method !== "GET") {
      logEntry.route = "method_not_allowed";
      return methodNotAllowed(["GET"]);
    }

    logEntry.route = "root";
    return json({
      service: "riot-package-registry",
      routes: {
        resolve: "/package/<locator>/-/resolve?ref=<selector>",
        manifest: "/package/<locator>/-/manifest/<sha>.json",
        source: "/package/<locator>/-/source/<sha>.tar.gz",
        publish: "/package/<locator>/-/publish?ref=<selector>",
      },
      cdn_base_url: getConfig(env).cdnBaseUrl,
    });
  }

  if (!path.startsWith("package/")) {
    throw new HttpError(404, "not_found", "Route does not exist.");
  }

  const remainder = path.slice("package/".length);
  const separatorIndex = remainder.indexOf("/-/");
  if (separatorIndex === -1) {
    throw new HttpError(404, "not_found", "Package route is missing the /-/ separator.");
  }

  const rawLocator = decodeURIComponent(remainder.slice(0, separatorIndex));
  const operationPath = remainder.slice(separatorIndex + 3);
  const locator = normalizeLocator(rawLocator);
  logEntry.package_locator = locator.normalized;

  if (operationPath === "resolve") {
    if (request.method !== "GET") {
      logEntry.route = "method_not_allowed";
      return methodNotAllowed(["GET"]);
    }

    logEntry.route = "resolve";
    const selector = url.searchParams.get("ref") ?? "main";
    logEntry.selector = selector;
    return await handleResolve(request, env, locator, selector, logEntry);
  }

  if (operationPath.startsWith("manifest/") && operationPath.endsWith(".json")) {
    if (request.method !== "GET") {
      logEntry.route = "method_not_allowed";
      return methodNotAllowed(["GET"]);
    }

    logEntry.route = "manifest";
    const sha = operationPath.slice("manifest/".length, -".json".length);
    logEntry.resolved_sha = sha;
    return await handleManifest(env, locator, sha);
  }

  if (operationPath.startsWith("source/") && operationPath.endsWith(".tar.gz")) {
    if (request.method !== "GET") {
      logEntry.route = "method_not_allowed";
      return methodNotAllowed(["GET"]);
    }

    logEntry.route = "source";
    const sha = operationPath.slice("source/".length, -".tar.gz".length);
    logEntry.resolved_sha = sha;
    return await handleSource(env, locator, sha);
  }

  if (operationPath === "publish") {
    if (request.method !== "POST") {
      logEntry.route = "method_not_allowed";
      return methodNotAllowed(["POST"]);
    }

    logEntry.route = "publish";
    const selector = url.searchParams.get("ref") ?? "main";
    logEntry.selector = selector;
    return await handlePublish(request, env, locator, selector, logEntry);
  }

  throw new HttpError(404, "not_found", "Package route does not exist.");
}

async function handleResolve(
  request: Request,
  env: Env,
  locator: PackageLocator,
  selector: string,
  logEntry: RequestLogEntry,
): Promise<Response> {
  const config = getConfig(env);
  const publication =
    (await readCachedMaterialization(env, locator, selector)) ??
    (await materializeThroughCoordinator(env, locator, selector));

  const requestUrl = new URL(request.url);
  logEntry.resolved_sha = publication.resolvedSha;

  return json({
    package: locator.normalized,
    source_url: canonicalSourceUrl(locator),
    package_subdir: packageSubdir(locator),
    selector,
    resolved_sha: publication.resolvedSha,
    manifest: {
      key: publication.manifestKey,
      url: `${requestUrl.origin}${manifestRoutePath(locator, publication.resolvedSha)}`,
      cdn_url: prettyManifestUrl(config, locator, publication.resolvedSha),
    },
    source_archive: {
      key: publication.sourceKey,
      url: `${requestUrl.origin}${sourceRoutePath(locator, publication.resolvedSha)}`,
      cdn_url: prettySourceUrl(config, locator, publication.resolvedSha),
    },
    cache: {
      manifest: !publication.manifestCreated,
      source: !publication.sourceCreated,
    },
  });
}

async function handlePublish(
  request: Request,
  env: Env,
  locator: PackageLocator,
  selector: string,
  logEntry: RequestLogEntry,
): Promise<Response> {
  requireRootAuth(request, env);

  const publishResult = await publishThroughCoordinator(env, locator, selector);
  const requestUrl = new URL(request.url);
  logEntry.resolved_sha = publishResult.resolvedSha;

  return json({
    package: locator.normalized,
    source_url: canonicalSourceUrl(locator),
    package_subdir: packageSubdir(locator),
    selector,
    resolved_sha: publishResult.resolvedSha,
    package_name: publishResult.packageName,
    package_version: publishResult.packageVersion,
    manifest: {
      key: publishResult.manifestKey,
      url: `${requestUrl.origin}${manifestRoutePath(locator, publishResult.resolvedSha)}`,
      cdn_url: prettyManifestUrl(getConfig(env), locator, publishResult.resolvedSha),
    },
    source_archive: {
      key: publishResult.sourceKey,
      url: `${requestUrl.origin}${sourceRoutePath(locator, publishResult.resolvedSha)}`,
      cdn_url: prettySourceUrl(getConfig(env), locator, publishResult.resolvedSha),
    },
    claim: {
      key: publishResult.claimKey,
      created: publishResult.claimCreated,
    },
    release: {
      key: publishResult.releaseKey,
      created: publishResult.releaseCreated,
    },
    materialization: {
      manifest: !publishResult.manifestCreated,
      source: !publishResult.sourceCreated,
    },
  });
}

async function materializeThroughCoordinator(
  env: Env,
  locator: PackageLocator,
  selector: string,
): Promise<{
  selector: string;
  resolvedSha: string;
  sourceKey: string;
  manifestKey: string;
  sourceCreated: boolean;
  manifestCreated: boolean;
}> {
  const id = env.PUBLICATION_COORDINATOR.idFromName("global");
  const stub = env.PUBLICATION_COORDINATOR.get(id);
  const response = await stub.fetch("https://publication-coordinator.internal/publish", {
    method: "POST",
    headers: {
      "content-type": "application/json; charset=utf-8",
    },
    body: JSON.stringify({
      operation: "materialize",
      locator: locator.normalized,
      selector,
    }),
  });

  if (!response.ok) {
    const payload = (await response.json()) as { error?: string; message?: string };
    throw new HttpError(
      response.status,
      payload.error ?? "publication_failed",
      payload.message ?? "Publication coordinator failed.",
    );
  }

  const payload = (await response.json()) as {
    selector: string;
    resolved_sha: string;
    source_key: string;
    manifest_key: string;
    source_created: boolean;
    manifest_created: boolean;
  };

  return {
    selector: payload.selector,
    resolvedSha: payload.resolved_sha,
    sourceKey: payload.source_key,
    manifestKey: payload.manifest_key,
    sourceCreated: payload.source_created,
    manifestCreated: payload.manifest_created,
  };
}

async function publishThroughCoordinator(
  env: Env,
  locator: PackageLocator,
  selector: string,
): Promise<PublishedPackageRelease> {
  const id = env.PUBLICATION_COORDINATOR.idFromName("global");
  const stub = env.PUBLICATION_COORDINATOR.get(id);
  const response = await stub.fetch("https://publication-coordinator.internal/publish", {
    method: "POST",
    headers: {
      "content-type": "application/json; charset=utf-8",
    },
    body: JSON.stringify({
      operation: "publish",
      locator: locator.normalized,
      selector,
    }),
  });

  if (!response.ok) {
    const payload = (await response.json()) as { error?: string; message?: string };
    throw new HttpError(
      response.status,
      payload.error ?? "publish_failed",
      payload.message ?? "Package publish failed.",
    );
  }

  const payload = (await response.json()) as {
    selector: string;
    resolved_sha: string;
    source_key: string;
    manifest_key: string;
    source_created: boolean;
    manifest_created: boolean;
    package_name: string;
    package_version: string;
    claim_key: string;
    release_key: string;
    claim_created: boolean;
    release_created: boolean;
    index_changed: boolean;
  };

  return {
    selector: payload.selector,
    resolvedSha: payload.resolved_sha,
    sourceKey: payload.source_key,
    manifestKey: payload.manifest_key,
    sourceCreated: payload.source_created,
    manifestCreated: payload.manifest_created,
    packageName: payload.package_name,
    packageVersion: payload.package_version,
    claimKey: payload.claim_key,
    releaseKey: payload.release_key,
    claimCreated: payload.claim_created,
    releaseCreated: payload.release_created,
    indexChanged: payload.index_changed,
  };
}

async function handleManifest(
  env: Env,
  locator: PackageLocator,
  sha: string,
): Promise<Response> {
  if (!isFullSha(sha)) {
    throw new HttpError(400, "invalid_sha", "Manifest requests require a full commit SHA.");
  }

  const object = await env.ML_PKGS_CDN.get(manifestKey(locator, sha));
  if (object === null) {
    throw new HttpError(404, "manifest_not_found", "Package manifest was not found.");
  }

  const headers = immutableHeaders("application/json; charset=utf-8");
  object.writeHttpMetadata(headers);
  return new Response(object.body, { headers });
}

async function handleSource(
  env: Env,
  locator: PackageLocator,
  sha: string,
): Promise<Response> {
  if (!isFullSha(sha)) {
    throw new HttpError(400, "invalid_sha", "Source archive requests require a full commit SHA.");
  }

  const object = await env.ML_PKGS_CDN.head(sourceArchiveKey(locator, sha));
  if (object === null) {
    throw new HttpError(404, "source_not_found", "Source archive was not found.");
  }

  return Response.redirect(prettySourceUrl(getConfig(env), locator, sha), 307);
}

function trimSlashes(value: string): string {
  return value.replace(/^\/+|\/+$/g, "");
}

function requireRootAuth(request: Request, env: Env): void {
  if (typeof env.ROOT_AUTH_TOKEN !== "string" || env.ROOT_AUTH_TOKEN.length === 0) {
    throw new HttpError(
      503,
      "publish_auth_not_configured",
      "Publish auth is not configured for this registry.",
    );
  }

  const authorization = request.headers.get("authorization");
  if (authorization !== `Bearer ${env.ROOT_AUTH_TOKEN}`) {
    throw new HttpError(401, "unauthorized", "Publish requests require valid root auth.");
  }
}

function normalizeError(error: unknown): HttpError {
  if (error instanceof HttpError) {
    return error;
  }

  if (error instanceof Error) {
    return new HttpError(500, "internal_error", error.message);
  }

  return new HttpError(500, "internal_error", "Unknown error.");
}
