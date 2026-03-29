import { generateState, GitHub } from "arctic";
import { parse as parseCookieHeader, serialize as serializeCookie } from "cookie";

import { getConfig, getGitHubApiBaseUrl } from "./config.ts";
import { HttpError } from "./errors.ts";
import {
  deleteSessionRecord,
  deleteOAuthStateRecord,
  readApiTokenLookupRecord,
  readApiTokenRecord,
  readOAuthStateRecord,
  readSessionRecord,
  readUserLoginRecord,
  readUserRecord,
  writeApiTokenLookupRecord,
  writeApiTokenRecord,
  writeOAuthStateRecord,
  writeSessionRecord,
  writeUserLoginRecord,
  writeUserRecord,
} from "./storage.ts";
import type {
  ApiTokenCapability,
  ApiTokenLookupRecord,
  ApiTokenRecord,
  AuthenticatedActor,
  AuthenticatedActorUser,
  Env,
  OAuthStateRecord,
  SessionRecord,
  UserRecord,
} from "./types.ts";

const SESSION_COOKIE_NAME = "pkgs_session";
const SESSION_TTL_SECONDS = 60 * 60 * 24 * 30;
const OAUTH_STATE_TTL_MS = 10 * 60 * 1000;
const API_TOKEN_PREFIX = "sk-";

interface GitHubUserProfile {
  id: number;
  login: string;
  name?: string | null;
  avatar_url?: string | null;
}

export async function createGitHubAuthorizationUrl(
  env: Env,
  requestUrl: URL,
  rawReturnTo: string | null,
): Promise<string> {
  const client = getGitHubOAuthClient(env, buildGitHubCallbackUrl(requestUrl));
  const state = generateState();
  const returnTo = resolveReturnTo(env, rawReturnTo);
  const createdAt = new Date().toISOString();

  const record: OAuthStateRecord = {
    state_id: state,
    return_to: returnTo,
    created_at: createdAt,
  };

  await writeOAuthStateRecord(env.ML_PKGS_CDN, record);
  return client.createAuthorizationURL(state, ["read:user"]).toString();
}

export async function completeGitHubAuthorization(
  env: Env,
  requestUrl: URL,
  code: string,
  state: string,
): Promise<{ user: UserRecord; session: SessionRecord; returnTo: string }> {
  const client = getGitHubOAuthClient(env, buildGitHubCallbackUrl(requestUrl));
  const stateRecord = await readOAuthStateRecord(env.ML_PKGS_CDN, state);

  if (stateRecord === null) {
    throw new HttpError(400, "invalid_oauth_state", "GitHub login state is missing or invalid.");
  }

  await deleteOAuthStateRecord(env.ML_PKGS_CDN, state);

  const createdAt = new Date(stateRecord.created_at);
  if (Number.isNaN(createdAt.getTime()) || Date.now() - createdAt.getTime() > OAUTH_STATE_TTL_MS) {
    throw new HttpError(400, "expired_oauth_state", "GitHub login state has expired.");
  }

  let tokens;
  try {
    tokens = await client.validateAuthorizationCode(code);
  } catch {
    throw new HttpError(
      502,
      "github_oauth_exchange_failed",
      "GitHub OAuth code exchange failed.",
    );
  }

  const githubUser = await fetchGitHubUser(env, tokens.accessToken());
  const user = await upsertUser(env, githubUser);
  const session = await createSession(env, user);

  return {
    user,
    session,
    returnTo: stateRecord.return_to,
  };
}

export async function logoutSession(
  request: Request,
  env: Env,
): Promise<void> {
  const sessionId = readSessionIdFromCookies(request);
  if (sessionId === null) {
    return;
  }

  await deleteSessionRecord(env.ML_PKGS_CDN, sessionId);
}

export function buildSessionCookie(env: Env, session: SessionRecord): string {
  const config = getConfig(env);
  return serializeCookie(SESSION_COOKIE_NAME, session.session_id, {
    path: "/",
    httpOnly: true,
    sameSite: "lax",
    secure: true,
    domain: config.authCookieDomain,
    maxAge: SESSION_TTL_SECONDS,
    expires: new Date(session.expires_at),
  });
}

export function buildClearedSessionCookie(env: Env): string {
  const config = getConfig(env);
  return serializeCookie(SESSION_COOKIE_NAME, "", {
    path: "/",
    httpOnly: true,
    sameSite: "lax",
    secure: true,
    domain: config.authCookieDomain,
    maxAge: 0,
    expires: new Date(0),
  });
}

export async function readAuthenticatedSession(
  request: Request,
  env: Env,
): Promise<{ session: SessionRecord; user: UserRecord } | null> {
  const sessionId = readSessionIdFromCookies(request);
  if (sessionId === null) {
    return null;
  }

  const session = await readSessionRecord(env.ML_PKGS_CDN, sessionId);
  if (session === null) {
    return null;
  }

  const expiresAt = new Date(session.expires_at);
  if (Number.isNaN(expiresAt.getTime()) || expiresAt.getTime() <= Date.now()) {
    return null;
  }

  const user = await readUserRecord(env.ML_PKGS_CDN, session.user_id);
  if (user === null) {
    return null;
  }

  return { session, user };
}

export async function requireAuthenticatedSession(
  request: Request,
  env: Env,
): Promise<{ session: SessionRecord; user: UserRecord }> {
  const authenticated = await readAuthenticatedSession(request, env);
  if (authenticated === null) {
    throw new HttpError(401, "unauthorized", "This route requires an authenticated user session.");
  }

  return authenticated;
}

export async function requirePublishActor(
  request: Request,
  env: Env,
): Promise<AuthenticatedActor> {
  const authorization = request.headers.get("authorization");

  if (typeof env.ROOT_AUTH_TOKEN === "string" && env.ROOT_AUTH_TOKEN.length > 0) {
    if (authorization === `Bearer ${env.ROOT_AUTH_TOKEN}`) {
      return { kind: "root" };
    }
  }

  const bearerToken = readBearerToken(authorization);
  if (bearerToken === null) {
    throw new HttpError(401, "unauthorized", "Publish requests require a valid API token.");
  }

  return await authenticateApiToken(env, bearerToken, ["publish"]);
}

export async function createPublishApiToken(
  env: Env,
  user: UserRecord,
  name: string,
): Promise<{ plaintext: string; record: ApiTokenRecord }> {
  const trimmedName = name.trim();
  if (trimmedName.length === 0) {
    throw new HttpError(400, "invalid_token_name", "Token name must be a non-empty string.");
  }

  const plaintext = `${API_TOKEN_PREFIX}${generateState()}`;
  const secretHash = await hashSecret(plaintext);
  const record: ApiTokenRecord = {
    token_id: crypto.randomUUID(),
    user_id: user.user_id,
    github_login: user.github_login,
    name: trimmedName,
    secret_hash: secretHash,
    capabilities: ["publish"],
    created_at: new Date().toISOString(),
  };

  const lookup: ApiTokenLookupRecord = {
    token_id: record.token_id,
    user_id: record.user_id,
    github_login: record.github_login,
    capabilities: record.capabilities,
  };

  await writeApiTokenRecord(env.ML_PKGS_CDN, record);
  await writeApiTokenLookupRecord(env.ML_PKGS_CDN, secretHash, lookup);

  return { plaintext, record };
}

export async function revokeApiToken(
  env: Env,
  user: UserRecord,
  tokenId: string,
): Promise<ApiTokenRecord | null> {
  const record = await readApiTokenRecord(env.ML_PKGS_CDN, user.user_id, tokenId);
  if (record === null) {
    return null;
  }

  if (record.revoked_at !== undefined) {
    return record;
  }

  const revokedAt = new Date().toISOString();
  const nextRecord: ApiTokenRecord = {
    ...record,
    revoked_at: revokedAt,
  };

  const lookup = await readApiTokenLookupRecord(env.ML_PKGS_CDN, record.secret_hash);
  if (lookup !== null) {
    await writeApiTokenLookupRecord(env.ML_PKGS_CDN, record.secret_hash, {
      ...lookup,
      revoked_at: revokedAt,
    });
  }

  await writeApiTokenRecord(env.ML_PKGS_CDN, nextRecord);
  return nextRecord;
}

function getGitHubOAuthClient(env: Env, callbackUrl: string): GitHub {
  if (
    typeof env.GITHUB_OAUTH_CLIENT_ID !== "string" ||
    env.GITHUB_OAUTH_CLIENT_ID.length === 0 ||
    typeof env.GITHUB_OAUTH_CLIENT_SECRET !== "string" ||
    env.GITHUB_OAUTH_CLIENT_SECRET.length === 0
  ) {
    throw new HttpError(503, "github_oauth_not_configured", "GitHub OAuth is not configured.");
  }

  return new GitHub(
    env.GITHUB_OAUTH_CLIENT_ID,
    env.GITHUB_OAUTH_CLIENT_SECRET,
    callbackUrl,
  );
}

function buildGitHubCallbackUrl(requestUrl: URL): string {
  return new URL("/v1/auth/github/callback", requestUrl.origin).toString();
}

async function fetchGitHubUser(env: Env, accessToken: string): Promise<GitHubUserProfile> {
  const response = await fetch(`${getGitHubApiBaseUrl(env)}/user`, {
    headers: {
      accept: "application/vnd.github+json",
      authorization: `Bearer ${accessToken}`,
      "user-agent": "riot-package-registry",
    },
  });

  if (!response.ok) {
    throw new HttpError(
      502,
      "github_user_lookup_failed",
      `GitHub user lookup failed with status ${response.status}.`,
    );
  }

  const payload = (await response.json()) as Partial<GitHubUserProfile>;
  if (typeof payload.id !== "number" || typeof payload.login !== "string") {
    throw new HttpError(502, "github_user_lookup_failed", "GitHub user response was invalid.");
  }

  return payload as GitHubUserProfile;
}

async function upsertUser(env: Env, githubUser: GitHubUserProfile): Promise<UserRecord> {
  const now = new Date().toISOString();
  const existingLogin = await readUserLoginRecord(env.ML_PKGS_CDN, githubUser.login);
  const userId = existingLogin?.user_id ?? crypto.randomUUID();
  const existingUser = await readUserRecord(env.ML_PKGS_CDN, userId);

  const record: UserRecord = {
    user_id: userId,
    github_id: githubUser.id,
    github_login: githubUser.login,
    github_name: githubUser.name ?? undefined,
    github_avatar_url: githubUser.avatar_url ?? undefined,
    created_at: existingUser?.created_at ?? now,
    updated_at: now,
  };

  await writeUserRecord(env.ML_PKGS_CDN, record);
  await writeUserLoginRecord(env.ML_PKGS_CDN, {
    github_login: githubUser.login,
    user_id: userId,
    updated_at: now,
  });

  return record;
}

async function createSession(env: Env, user: UserRecord): Promise<SessionRecord> {
  const createdAt = new Date();
  const expiresAt = new Date(createdAt.getTime() + SESSION_TTL_SECONDS * 1000);
  const record: SessionRecord = {
    session_id: crypto.randomUUID(),
    user_id: user.user_id,
    github_login: user.github_login,
    created_at: createdAt.toISOString(),
    expires_at: expiresAt.toISOString(),
  };

  await writeSessionRecord(env.ML_PKGS_CDN, record);
  return record;
}

export function resolveReturnTo(env: Env, rawReturnTo: string | null): string {
  const baseUrl = new URL(getConfig(env).pkgsWebBaseUrl);

  if (rawReturnTo === null || rawReturnTo.trim().length === 0) {
    return baseUrl.toString();
  }

  const trimmed = rawReturnTo.trim();
  if (trimmed.startsWith("/")) {
    return new URL(trimmed, baseUrl).toString();
  }

  try {
    const target = new URL(trimmed);
    if (target.origin !== baseUrl.origin) {
      return baseUrl.toString();
    }

    return target.toString();
  } catch {
    return baseUrl.toString();
  }
}

function readSessionIdFromCookies(request: Request): string | null {
  const header = request.headers.get("cookie");
  if (header === null) {
    return null;
  }

  const cookies = parseCookieHeader(header);
  const sessionId = cookies[SESSION_COOKIE_NAME];
  return typeof sessionId === "string" && sessionId.length > 0 ? sessionId : null;
}

function readBearerToken(authorization: string | null): string | null {
  if (authorization === null || !authorization.startsWith("Bearer ")) {
    return null;
  }

  const token = authorization.slice("Bearer ".length).trim();
  return token.length > 0 ? token : null;
}

async function authenticateApiToken(
  env: Env,
  plaintext: string,
  requiredCapabilities: ApiTokenCapability[],
): Promise<AuthenticatedActorUser> {
  if (!plaintext.startsWith(API_TOKEN_PREFIX)) {
    throw new HttpError(401, "unauthorized", "Publish requests require a valid API token.");
  }

  const secretHash = await hashSecret(plaintext);
  const lookup = await readApiTokenLookupRecord(env.ML_PKGS_CDN, secretHash);
  if (lookup === null || lookup.revoked_at !== undefined) {
    throw new HttpError(401, "unauthorized", "Publish requests require a valid API token.");
  }

  const record = await readApiTokenRecord(env.ML_PKGS_CDN, lookup.user_id, lookup.token_id);
  if (record === null || record.revoked_at !== undefined) {
    throw new HttpError(401, "unauthorized", "Publish requests require a valid API token.");
  }

  for (const capability of requiredCapabilities) {
    if (!record.capabilities.includes(capability)) {
      throw new HttpError(403, "token_missing_capability", "API token does not grant publish access.");
    }
  }

  const now = new Date().toISOString();
  await writeApiTokenRecord(env.ML_PKGS_CDN, {
    ...record,
    last_used_at: now,
  });

  return {
    kind: "user",
    userId: record.user_id,
    githubLogin: record.github_login,
    tokenId: record.token_id,
  };
}

async function hashSecret(secret: string): Promise<string> {
  const digest = await crypto.subtle.digest("SHA-256", new TextEncoder().encode(secret));
  return [...new Uint8Array(digest)]
    .map((byte) => byte.toString(16).padStart(2, "0"))
    .join("");
}
