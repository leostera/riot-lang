import { HttpError } from "./errors.ts";
import { normalizeLocator } from "./locator.ts";
import { ensurePublication } from "./publication.ts";
import { json } from "./http.ts";
import type { Env, ResolvedPublication } from "./types.ts";

interface PublicationRequest {
  locator: string;
  selector: string;
}

export async function handlePublicationCoordinatorRequest(
  request: Request,
  env: Env,
): Promise<Response> {
  const body = (await request.json()) as Partial<PublicationRequest>;
  const rawLocator = body.locator;
  const selector = body.selector;

  if (typeof rawLocator !== "string" || rawLocator.length === 0) {
    throw new HttpError(400, "invalid_locator", "Coordinator request must include a locator.");
  }

  if (typeof selector !== "string" || selector.length === 0) {
    throw new HttpError(400, "invalid_selector", "Coordinator request must include a selector.");
  }

  const locator = normalizeLocator(rawLocator);
  const publication = await ensurePublication(env, locator, selector);
  return json(serializePublication(publication));
}

function serializePublication(publication: ResolvedPublication): Record<string, unknown> {
  return {
    selector: publication.selector,
    resolved_sha: publication.resolvedSha,
    source_key: publication.sourceKey,
    manifest_key: publication.manifestKey,
    source_created: publication.sourceCreated,
    manifest_created: publication.manifestCreated,
  };
}
