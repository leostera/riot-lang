import { getGitHubApiBaseUrl } from "./config.ts";
import { HttpError } from "./errors.ts";
import type { Env, PackageLocator } from "./types.ts";

interface GitHubCommitResponse {
  sha?: string;
}

export async function resolveGitHubSelector(
  env: Env,
  locator: PackageLocator,
  selector: string,
): Promise<string> {
  ensureGitHubLocator(locator);

  const response = await fetchFromGitHub(
    env,
    `/repos/${locator.owner}/${locator.repo}/commits/${encodeURIComponent(selector)}`,
    {
      headers: {
        accept: "application/vnd.github+json",
      },
    },
  );

  if (!response.ok) {
    throw githubError(response, selector);
  }

  const payload = (await response.json()) as GitHubCommitResponse;
  const sha = payload.sha;

  if (typeof sha !== "string" || sha.length === 0) {
    throw new HttpError(
      502,
      "github_invalid_response",
      `GitHub returned an invalid commit payload for selector ${selector}.`,
    );
  }

  return sha;
}

export async function fetchGitHubTarball(
  env: Env,
  locator: PackageLocator,
  ref: string,
): Promise<Uint8Array<ArrayBuffer>> {
  ensureGitHubLocator(locator);

  const response = await fetchFromGitHub(
    env,
    `/repos/${locator.owner}/${locator.repo}/tarball/${encodeURIComponent(ref)}`,
    {
      headers: {
        accept: "application/vnd.github+json",
      },
      redirect: "follow",
    },
  );

  if (!response.ok) {
    throw githubError(response, ref);
  }

  return new Uint8Array(await response.arrayBuffer());
}

function ensureGitHubLocator(locator: PackageLocator): void {
  if (locator.provider !== "github.com") {
    throw new HttpError(
      400,
      "unsupported_provider",
      `Provider ${locator.provider} is not supported yet.`,
    );
  }
}

async function fetchFromGitHub(
  env: Env,
  path: string,
  init: RequestInit,
): Promise<Response> {
  const headers = new Headers(init.headers);
  if (!headers.has("user-agent")) {
    headers.set("user-agent", "riot-package-registry");
  }

  const token = env.GITHUB_TOKEN?.trim();
  if (token !== undefined && token.length > 0) {
    headers.set("authorization", `Bearer ${token}`);
  }

  return await fetch(`${getGitHubApiBaseUrl(env)}${path}`, {
    ...init,
    headers,
  });
}

function githubError(response: Response, selector: string): HttpError {
  if (response.status === 404) {
    return new HttpError(
      404,
      "github_ref_not_found",
      `GitHub could not resolve selector ${selector}.`,
    );
  }

  if (response.status >= 500) {
    return new HttpError(
      502,
      "github_unavailable",
      `GitHub failed while resolving selector ${selector}.`,
    );
  }

  return new HttpError(
    502,
    "github_request_failed",
    `GitHub returned status ${response.status} while resolving selector ${selector}.`,
  );
}
