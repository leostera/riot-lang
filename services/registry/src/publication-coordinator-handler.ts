import { HttpError } from "./errors.ts";
import { normalizeLocator } from "./locator.ts";
import { ensureSourceMaterialization, publishPackageRelease } from "./publication.ts";
import { json } from "./http.ts";
import type {
  AuthenticatedActor,
  Env,
  PublishedPackageRelease,
  ResolvedPublication,
} from "./types.ts";

interface PublicationRequest {
  operation?: "materialize" | "publish";
  locator: string;
  selector: string;
  actor?: AuthenticatedActor;
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
  if (body.operation === "publish") {
    if (body.actor?.kind !== "root" && body.actor?.kind !== "user") {
      throw new HttpError(400, "invalid_actor", "Coordinator publish requests must include an actor.");
    }

    const publication = await publishPackageRelease(env, locator, selector, body.actor);
    return json(serializePublishedRelease(publication));
  }

  const publication = await ensureSourceMaterialization(env, locator, selector);
  return json(serializeMaterialization(publication));
}

function serializeMaterialization(publication: ResolvedPublication): Record<string, unknown> {
  return {
    selector: publication.selector,
    resolved_sha: publication.resolvedSha,
    source_key: publication.sourceKey,
    manifest_key: publication.manifestKey,
    source_created: publication.sourceCreated,
    manifest_created: publication.manifestCreated,
  };
}

function serializePublishedRelease(publication: PublishedPackageRelease): Record<string, unknown> {
  return {
    ...serializeMaterialization(publication),
    package_name: publication.packageName,
    package_version: publication.packageVersion,
    claim_key: publication.claimKey,
    release_key: publication.releaseKey,
    claim_created: publication.claimCreated,
    release_created: publication.releaseCreated,
    index_changed: publication.indexChanged,
  };
}
