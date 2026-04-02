import {
  buildClearedSessionCookie,
  buildSessionCookie,
  completeGitHubAuthorization,
  createGitHubAuthorizationUrl,
  createPublishApiToken,
  logoutSession,
  readAuthenticatedSession,
  requireAuthenticatedSession,
  requirePublishActor,
  resolveReturnTo,
  revokeApiToken,
} from "./auth.ts";
import { getConfig } from "./config.ts";
import { HttpError } from "./errors.ts";
import { json, methodNotAllowed } from "./http.ts";
import { writeRequestLog } from "./request-log.ts";
import {
  listRegistryEvents,
  listPackageRegistryEvents,
  listApiTokenRecords,
  readCategoriesIndexDocument,
  readOwnerPackagesDocument,
  readPackageOverviewDocument,
  readPackageRelationsDocument,
  readPopularPackagesDocument,
  readRecentPackagesDocument,
} from "./metadata-db.ts";
import { searchPackages } from "./search-db.ts";
import {
  artifactManifestKey,
  artifactSourceArchiveKey,
  buildIndexConfigDocument,
  cdnObjectUrl,
  indexConfigKey,
} from "./storage.ts";
import type {
  ApiTokenRecord,
  AuthenticatedActor,
  Env,
  IndexedPackageRelease,
  PackageIndexDocument,
  PublishedPackageRelease,
  RequestLogEntry,
} from "./types.ts";

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
        publish_artifact: "/v1/publish",
        index_config: "/v1/index/config.json",
        index_package: "/v1/index/<sharded-package-document>.json",
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
        index_config: "/api/v1/index/config.json",
        index_package: "/api/v1/index/<sharded-package-document>.json",
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
      cdn_base_url: getConfig(env).cdnBaseUrl,
      index_base_url: `${getConfig(env).indexBaseUrl}/${getConfig(env).indexRoutePath}`,
    });
  }

  const indexStorageKey = resolveIndexStorageKey(path, getConfig(env));
  if (indexStorageKey !== null) {
    if (request.method !== "GET" && request.method !== "HEAD") {
      logEntry.route = "method_not_allowed";
      return methodNotAllowed(["GET", "HEAD"]);
    }

    logEntry.route = "api.index";
    return await handleIndexDocument(request, env, indexStorageKey);
  }

  if (matchesPath(path, "v1/auth/github/start", "auth/github/start")) {
    if (request.method !== "GET") {
      logEntry.route = "method_not_allowed";
      return methodNotAllowed(["GET"]);
    }

    logEntry.route = "auth.github.start";
    const authorizationUrl = await createGitHubAuthorizationUrl(
      env,
      url,
      url.searchParams.get("return_to"),
    );
    return redirect(authorizationUrl);
  }

  if (matchesPath(path, "v1/auth/github/callback", "auth/github/callback")) {
    if (request.method !== "GET") {
      logEntry.route = "method_not_allowed";
      return methodNotAllowed(["GET"]);
    }

    logEntry.route = "auth.github.callback";
    const code = url.searchParams.get("code");
    const state = url.searchParams.get("state");
    if (code === null || state === null) {
      throw new HttpError(
        400,
        "invalid_oauth_callback",
        "GitHub OAuth callback requires both code and state parameters.",
      );
    }

    const { session, returnTo } = await completeGitHubAuthorization(env, url, code, state);
    return redirect(returnTo, {
      "set-cookie": buildSessionCookie(env, session),
    });
  }

  if (matchesPath(path, "v1/auth/logout", "auth/logout")) {
    if (request.method !== "POST") {
      logEntry.route = "method_not_allowed";
      return methodNotAllowed(["POST"]);
    }

    logEntry.route = "auth.logout";
    return await handleLogout(request, env, url);
  }

  if (matchesPath(path, "v1/me", "api/v1/me")) {
    if (request.method !== "GET") {
      logEntry.route = "method_not_allowed";
      return methodNotAllowed(["GET"]);
    }

    logEntry.route = "api.me";
    return await handleCurrentSession(request, env);
  }

  if (matchesPath(path, "v1/search", "api/v1/search")) {
    if (request.method !== "GET") {
      logEntry.route = "method_not_allowed";
      return methodNotAllowed(["GET"]);
    }

    logEntry.route = "api.search";
    return await handleSearch(env, url);
  }

  if (matchesPath(path, "v1/events", "api/v1/events")) {
    if (request.method !== "GET") {
      logEntry.route = "method_not_allowed";
      return methodNotAllowed(["GET"]);
    }

    logEntry.route = "api.events";
    return await handleEvents(env, url);
  }

  const packageEventsRoute = parsePackageEventsRoute(path);
  if (packageEventsRoute !== null) {
    if (request.method !== "GET") {
      logEntry.route = "method_not_allowed";
      return methodNotAllowed(["GET"]);
    }

    logEntry.route = "api.package_events";
    return await handlePackageEvents(env, url, packageEventsRoute.packageName);
  }

  const viewRoute = parseViewRoute(path);
  if (viewRoute !== null) {
    if (request.method !== "GET") {
      logEntry.route = "method_not_allowed";
      return methodNotAllowed(["GET"]);
    }

    logEntry.route = `api.views.${viewRoute.kind}`;
    return await handleViewDocument(env, viewRoute);
  }

  if (matchesPath(path, "v1/me/tokens", "api/v1/me/tokens")) {
    if (request.method === "GET") {
      logEntry.route = "api.me.tokens";
      return await handleListTokens(request, env);
    }

    if (request.method === "POST") {
      logEntry.route = "api.me.tokens.create";
      return await handleCreateToken(request, env);
    }

    logEntry.route = "method_not_allowed";
    return methodNotAllowed(["GET", "POST"]);
  }

  if (path.startsWith("v1/me/tokens/") || path.startsWith("api/v1/me/tokens/")) {
    if (request.method !== "DELETE") {
      logEntry.route = "method_not_allowed";
      return methodNotAllowed(["DELETE"]);
    }

    logEntry.route = "api.me.tokens.delete";
    const tokenId = decodeURIComponent(
      path.startsWith("v1/me/tokens/")
        ? path.slice("v1/me/tokens/".length)
        : path.slice("api/v1/me/tokens/".length),
    );
    return await handleDeleteToken(request, env, tokenId);
  }

  if (matchesPath(path, "v1/publish", "api/v1/publish")) {
    if (request.method !== "POST") {
      logEntry.route = "method_not_allowed";
      return methodNotAllowed(["POST"]);
    }

    logEntry.route = "publish.artifact";
    return await handleArtifactPublish(request, env, logEntry);
  }

  throw new HttpError(404, "not_found", "Route does not exist.");
}

async function handleArtifactPublish(
  request: Request,
  env: Env,
  logEntry: RequestLogEntry,
): Promise<Response> {
  const contentType = request.headers.get("content-type") ?? "";
  if (!contentType.includes("application/gzip") && !contentType.includes("application/x-gzip")) {
    throw new HttpError(
      415,
      "unsupported_media_type",
      "Artifact publish requires Content-Type: application/gzip.",
    );
  }

  const actor = await requirePublishActor(request, env);
  const archiveBytes = new Uint8Array(await request.arrayBuffer());
  if (archiveBytes.byteLength === 0) {
    throw new HttpError(400, "invalid_package_archive", "Artifact publish requires a non-empty tarball body.");
  }

  const publishResult = await publishArtifactThroughCoordinator(env, archiveBytes, actor);
  logEntry.artifact_sha256 = publishResult.artifactSha256;

  return json({
    package_name: publishResult.packageName,
    package_version: publishResult.packageVersion,
    artifact_sha256: publishResult.artifactSha256,
    manifest: {
      key: publishResult.manifestKey,
      cdn_url: cdnObjectUrl(getConfig(env), publishResult.manifestKey),
    },
    source_archive: {
      key: publishResult.sourceKey,
      cdn_url: cdnObjectUrl(getConfig(env), publishResult.sourceKey),
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

async function publishArtifactThroughCoordinator(
  env: Env,
  archiveBytes: Uint8Array<ArrayBuffer>,
  actor: AuthenticatedActor,
): Promise<PublishedPackageRelease> {
  const id = env.PUBLICATION_COORDINATOR.idFromName("global");
  const stub = env.PUBLICATION_COORDINATOR.get(id);
  const response = await stub.fetch("https://publication-coordinator.internal/publish", {
    method: "POST",
    headers: {
      "content-type": "application/gzip",
      "x-publication-operation": "publish-artifact",
      "x-publication-actor": JSON.stringify(actor),
    },
    body: archiveBytes,
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
    artifact_sha256: string;
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
    artifactSha256: payload.artifact_sha256,
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

async function handleLogout(request: Request, env: Env, url: URL): Promise<Response> {
  await logoutSession(request, env);
  const returnTo =
    (await readReturnToFromRequest(request)) ?? url.searchParams.get("return_to");

  if (wantsJson(request)) {
    return json(
      { ok: true },
      {
        headers: {
          "set-cookie": buildClearedSessionCookie(env),
        },
      },
    );
  }

  return redirect(resolveReturnTo(env, returnTo), {
    "set-cookie": buildClearedSessionCookie(env),
  });
}

async function handleCurrentSession(request: Request, env: Env): Promise<Response> {
  const authenticated = await readAuthenticatedSession(request, env);
  if (authenticated === null) {
    return json({
      authenticated: false,
    });
  }

  return json({
    authenticated: true,
    user: authenticated.user,
  });
}

async function handleSearch(env: Env, url: URL): Promise<Response> {
  const query = url.searchParams.get("q");

  if (query === null || query.trim().length === 0) {
    const config = getConfig(env);
    return json({
      service: "riot-package-registry",
      route: "/v1/search?q=<query>",
      source: {
        package_index_base_url: `${config.indexBaseUrl}/${config.indexRoutePath}`,
        updated_during_publish: true,
      },
    });
  }

  const limit = clampInteger(url.searchParams.get("limit"), 20, 1, 100);
  const offset = clampInteger(url.searchParams.get("offset"), 0, 0, 10_000);
  const results = await searchPackages(env.SEARCH_DB, query, limit, offset);

  return json({
    query,
    count: results.length,
    results,
  });
}

async function handleEvents(env: Env, url: URL): Promise<Response> {
  const limit = clampInteger(url.searchParams.get("limit"), 100, 1, 500);
  const after = url.searchParams.get("after") ?? undefined;
  const events = await listRegistryEvents(env.SEARCH_DB, limit, after);

  return json({
    limit,
    after,
    events,
  });
}

async function handlePackageEvents(
  env: Env,
  url: URL,
  packageName: string,
): Promise<Response> {
  const limit = clampInteger(url.searchParams.get("limit"), 25, 1, 100);
  const packageVersion = url.searchParams.get("version") ?? undefined;
  const events = await listPackageRegistryEvents(env.SEARCH_DB, packageName, packageVersion, limit);

  return json({
    package_name: packageName,
    package_version: packageVersion,
    limit,
    events,
  });
}

async function handleListTokens(request: Request, env: Env): Promise<Response> {
  const { user } = await requireAuthenticatedSession(request, env);
  const records = await listApiTokenRecords(env.SEARCH_DB, user.user_id);

  return json({
    user,
    tokens: records.filter((record) => record.revoked_at === undefined).map(serializeTokenRecord),
  });
}

async function handleViewDocument(
  env: Env,
  viewRoute: ParsedViewRoute,
): Promise<Response> {
  switch (viewRoute.kind) {
    case "package_overview": {
      const document = await readPackageOverviewDocument(env.SEARCH_DB, viewRoute.packageName);
      if (document === null) {
        throw new HttpError(404, "view_not_found", "Package overview was not found.");
      }

      return json(document);
    }
    case "package_relations": {
      const document = await readPackageRelationsDocument(env.SEARCH_DB, viewRoute.packageName);
      if (document === null) {
        throw new HttpError(404, "view_not_found", "Package relations were not found.");
      }

      return json(document);
    }
    case "recent_packages": {
      const document = await readRecentPackagesDocument(env.SEARCH_DB);
      if (document === null) {
        throw new HttpError(404, "view_not_found", "Recent packages view was not found.");
      }

      return json(document);
    }
    case "popular_packages": {
      const document = await readPopularPackagesDocument(env.SEARCH_DB);
      if (document === null) {
        throw new HttpError(404, "view_not_found", "Popular packages view was not found.");
      }

      return json(document);
    }
    case "categories": {
      const document = await readCategoriesIndexDocument(env.SEARCH_DB);
      if (document === null) {
        throw new HttpError(404, "view_not_found", "Categories view was not found.");
      }

      return json(document);
    }
    case "owner_packages": {
      const document = await readOwnerPackagesDocument(env.SEARCH_DB, viewRoute.ownerGithubLogin);
      if (document === null) {
        throw new HttpError(404, "view_not_found", "Owner packages view was not found.");
      }

      return json(document);
    }
  }
}

async function handleIndexDocument(
  request: Request,
  env: Env,
  storageKey: string,
): Promise<Response> {
  const config = getConfig(env);
  if (storageKey === indexConfigKey(config)) {
    return await respondWithIndexJson(request, buildIndexConfigDocument(config));
  }

  const object = await env.ML_PKGS_CDN.get(storageKey);
  if (object === null) {
    throw new HttpError(404, "index_not_found", "Index document was not found.");
  }

  const document = sanitizePackageIndexDocument(await object.json<PackageIndexDocument>());
  if (document === null) {
    throw new HttpError(404, "index_not_found", "Index document was not found.");
  }

  return await respondWithIndexJson(request, document);
}

async function handleCreateToken(request: Request, env: Env): Promise<Response> {
  const { user } = await requireAuthenticatedSession(request, env);
  const payload = await readBodyObject(request);
  const rawName = payload.name;

  if (typeof rawName !== "string") {
    throw new HttpError(400, "invalid_token_name", "Token creation requires a name string.");
  }

  const { plaintext, record } = await createPublishApiToken(env, user, rawName);
  return json(
    {
      plaintext_token: plaintext,
      token: serializeTokenRecord(record),
    },
    { status: 201 },
  );
}

async function handleDeleteToken(
  request: Request,
  env: Env,
  tokenId: string,
): Promise<Response> {
  const { user } = await requireAuthenticatedSession(request, env);
  const revoked = await revokeApiToken(env, user, tokenId);
  if (revoked === null) {
    throw new HttpError(404, "token_not_found", "Token does not exist.");
  }

  return json({
    token: serializeTokenRecord(revoked),
  });
}

interface ParsedPackageEventsRoute {
  packageName: string;
}

type ParsedViewRoute =
  | { kind: "package_overview"; packageName: string }
  | { kind: "package_relations"; packageName: string }
  | { kind: "recent_packages" }
  | { kind: "popular_packages" }
  | { kind: "categories" }
  | { kind: "owner_packages"; ownerGithubLogin: string };

function trimSlashes(value: string): string {
  return value.replace(/^\/+|\/+$/g, "");
}

function matchesPath(path: string, ...candidates: string[]): boolean {
  return candidates.includes(path);
}

function parseViewRoute(path: string): ParsedViewRoute | null {
  const normalizedPath = path.startsWith("api/v1/views/")
    ? path.slice("api/v1/views/".length)
    : path.startsWith("v1/views/")
      ? path.slice("v1/views/".length)
      : null;

  if (normalizedPath === null) {
    return null;
  }

  if (normalizedPath === "recent/packages") {
    return { kind: "recent_packages" };
  }

  if (normalizedPath === "popular/packages") {
    return { kind: "popular_packages" };
  }

  if (normalizedPath === "categories") {
    return { kind: "categories" };
  }

  const packageOverviewMatch = normalizedPath.match(/^packages\/([^/]+)\/overview$/);
  if (packageOverviewMatch !== null) {
    return {
      kind: "package_overview",
      packageName: decodeURIComponent(packageOverviewMatch[1] ?? ""),
    };
  }

  const packageRelationsMatch = normalizedPath.match(/^packages\/([^/]+)\/relations$/);
  if (packageRelationsMatch !== null) {
    return {
      kind: "package_relations",
      packageName: decodeURIComponent(packageRelationsMatch[1] ?? ""),
    };
  }

  const ownerPackagesMatch = normalizedPath.match(/^owners\/([^/]+)\/packages$/);
  if (ownerPackagesMatch !== null) {
    return {
      kind: "owner_packages",
      ownerGithubLogin: decodeURIComponent(ownerPackagesMatch[1] ?? ""),
    };
  }

  throw new HttpError(404, "not_found", "View route does not exist.");
}

function resolveIndexStorageKey(path: string, config: ReturnType<typeof getConfig>): string | null {
  const normalizedPath = trimSlashes(path);
  const routePrefixes = [config.indexRoutePath, `api/${config.indexRoutePath}`];

  for (const prefix of routePrefixes) {
    if (normalizedPath === `${prefix}/config.json`) {
      return indexConfigKey(config);
    }

    if (normalizedPath.startsWith(`${prefix}/`)) {
      const suffix = normalizedPath.slice(prefix.length + 1);
      if (suffix.length === 0 || !suffix.endsWith(".json")) {
        return null;
      }

      return `${config.indexBasePath}/${suffix}`;
    }
  }

  return null;
}

function sanitizePackageIndexDocument(document: PackageIndexDocument): PackageIndexDocument | null {
  const releases = document.releases.filter((release) => isServableIndexedRelease(document.name, release));
  if (releases.length === 0) {
    return null;
  }

  const updatedAt = releases
    .map((release) => Date.parse(release.published_at))
    .filter((value) => Number.isFinite(value))
    .sort((left, right) => right - left)[0];

  return {
    ...document,
    latest: releases[0]?.version ?? document.latest,
    updated_at: updatedAt === undefined ? document.updated_at : new Date(updatedAt).toISOString(),
    releases,
  };
}

function isServableIndexedRelease(packageName: string, release: IndexedPackageRelease): boolean {
  if (
    typeof release.version !== "string" ||
    typeof release.artifact_sha256 !== "string" ||
    release.version.length === 0 ||
    release.artifact_sha256.length === 0
  ) {
    return false;
  }

  return (
    release.manifest_key === artifactManifestKey(packageName, release.version, release.artifact_sha256) &&
    release.source_key === artifactSourceArchiveKey(packageName, release.version, release.artifact_sha256)
  );
}

async function respondWithIndexJson(request: Request, body: unknown): Promise<Response> {
  const payload = JSON.stringify(body, null, 2);
  const etag = `"${await sha256Hex(payload)}"`;

  if (request.headers.get("if-none-match") === etag) {
    return new Response(null, {
      status: 304,
      headers: {
        etag,
        "cache-control": "public, max-age=0, must-revalidate",
      },
    });
  }

  const headers = new Headers({
    "content-type": "application/json; charset=utf-8",
    "cache-control": "public, max-age=0, must-revalidate",
    etag,
  });

  if (request.method === "HEAD") {
    return new Response(null, {
      status: 200,
      headers,
    });
  }

  return new Response(payload, {
    status: 200,
    headers,
  });
}

async function sha256Hex(value: string): Promise<string> {
  const bytes = new TextEncoder().encode(value);
  const digest = await crypto.subtle.digest("SHA-256", bytes);
  return [...new Uint8Array(digest)].map((byte) => byte.toString(16).padStart(2, "0")).join("");
}

function parsePackageEventsRoute(path: string): ParsedPackageEventsRoute | null {
  const normalizedPath = trimSlashes(path);
  const packageEventsMatch = normalizedPath.match(/^(?:api\/)?v1\/packages\/([^/]+)\/events$/);
  if (packageEventsMatch === null) {
    return null;
  }

  const packageName = packageEventsMatch[1];
  if (packageName === undefined) {
    return null;
  }

  return {
    packageName: decodeURIComponent(packageName),
  };
}

function clampInteger(
  value: string | null,
  fallback: number,
  minimum: number,
  maximum: number,
): number {
  if (value === null) {
    return fallback;
  }

  const parsed = Number.parseInt(value, 10);
  if (!Number.isFinite(parsed)) {
    return fallback;
  }

  return Math.min(maximum, Math.max(minimum, parsed));
}

async function readBodyObject(request: Request): Promise<Record<string, unknown>> {
  const contentType = request.headers.get("content-type") ?? "";
  if (contentType.includes("application/json")) {
    return (await request.json()) as Record<string, unknown>;
  }

  if (
    contentType.includes("application/x-www-form-urlencoded") ||
    contentType.includes("multipart/form-data")
  ) {
    const form = await request.formData();
    const entries = [...form.entries()].map(([key, value]) => [key, String(value)]);
    return Object.fromEntries(entries);
  }

  return {};
}

async function readReturnToFromRequest(request: Request): Promise<string | null> {
  const body = await readBodyObject(request);
  return typeof body.return_to === "string" ? body.return_to : null;
}

function serializeTokenRecord(record: ApiTokenRecord): Record<string, unknown> {
  return {
    token_id: record.token_id,
    user_id: record.user_id,
    github_login: record.github_login,
    name: record.name,
    capabilities: record.capabilities,
    created_at: record.created_at,
    last_used_at: record.last_used_at,
    revoked_at: record.revoked_at,
  };
}

function wantsJson(request: Request): boolean {
  return request.headers.get("accept")?.includes("application/json") ?? false;
}

function redirect(location: string, extraHeaders?: Record<string, string>): Response {
  const headers = new Headers(extraHeaders);
  headers.set("location", location);
  return new Response(null, {
    status: 302,
    headers,
  });
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
