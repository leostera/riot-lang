import {
  buildClearedSessionCookie,
  buildSessionCookie,
  consumeSessionHandoff,
  readAuthenticatedSession,
} from "../../../api.pkgs.ml/src/auth.ts";
import { HttpError } from "../../../api.pkgs.ml/src/errors.ts";
import { readSessionRecord } from "../../../api.pkgs.ml/src/metadata-db.ts";
import type { Env as RegistryEnv, UserRecord } from "../../../api.pkgs.ml/src/types.ts";

export interface PlayAuthEnv extends Pick<RegistryEnv, "SEARCH_DB" | "AUTH_COOKIE_DOMAIN" | "PKGS_WEB_BASE_URL"> {}

export async function readPlayAuthenticatedSession(env: PlayAuthEnv, request: Request): Promise<UserRecord | null> {
  const authenticated = await readAuthenticatedSession(request, env as unknown as RegistryEnv);
  return authenticated?.user ?? null;
}

export async function completePlaySessionHandoff(
  env: PlayAuthEnv,
  handoffId: string,
): Promise<{ returnTo: string; sessionCookie: string }> {
  const authEnv = env as unknown as RegistryEnv;
  const handoff = await consumeSessionHandoff(authEnv, handoffId);
  if (handoff === null) {
    throw new HttpError(400, "invalid_session_handoff", "Playground session handoff is missing or has expired.");
  }

  const session = await readSessionRecord(authEnv.SEARCH_DB, handoff.session_id);
  if (session === null) {
    throw new HttpError(400, "invalid_session_handoff", "Playground session handoff did not reference an active session.");
  }

  return {
    returnTo: handoff.return_to,
    sessionCookie: buildSessionCookie(authEnv, session),
  };
}

export function buildPlayClearedSessionCookie(env: PlayAuthEnv): string {
  return buildClearedSessionCookie(env as unknown as RegistryEnv);
}
