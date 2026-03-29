# riot-package-registry

Cloudflare Worker scaffold for Riot's package publication service.

Production endpoint: `https://registry.pkgs.ml`

## Local development

1. Install dependencies with `bun install`.
2. Create a `.env` file next to `wrangler.toml`.
3. Run `bun run dev`.

Wrangler reads `.env` during local development, and the Worker expects at least:

```dotenv
CDN_BASE_URL=https://cdn.pkgs.ml
INDEX_BASE_PATH=index/v1
GITHUB_TOKEN=
ROOT_AUTH_TOKEN=
GITHUB_OAUTH_CLIENT_ID=
GITHUB_OAUTH_CLIENT_SECRET=
AUTH_COOKIE_DOMAIN=pkgs.ml
PKGS_WEB_BASE_URL=https://pkgs.ml
```

If `GITHUB_TOKEN` can read private repositories, the registry can publish from
those upstreams. This is useful for on-premise or private testing setups.

## Current surface

- `GET /` returns service metadata.
- `GET /package/<locator>/-/resolve?ref=<selector>` materializes or reuses a source-backed package snapshot.
- `GET /package/<locator>/-/manifest/<sha>.json` reads immutable manifests from R2.
- `GET /package/<locator>/-/source/<sha>.tar.gz` redirects to immutable source archives on `cdn.pkgs.ml`.
- `GET /auth/github/start?return_to=<url>` starts GitHub OAuth login.
- `GET /auth/github/callback?code=<code>&state=<state>` completes GitHub OAuth, creates a user session, and redirects back to `pkgs.ml`.
- `POST /auth/logout` clears the session cookie.
- `GET /api/v1/me` returns the authenticated user session, if one exists.
- `GET /api/v1/me/tokens` lists publish tokens for the authenticated user.
- `POST /api/v1/me/tokens` creates a new publish token and returns the plaintext token once.
- `DELETE /api/v1/me/tokens/<token-id>` revokes a publish token.
- `POST /package/<locator>/-/publish?ref=<selector>` publishes a named package release, synchronously updates the sparse package index under `cdn.pkgs.ml/index/v1`, and accepts either `Authorization: Bearer <ROOT_AUTH_TOKEN>` or a user publish token created through `/api/v1/me/tokens`.

The Worker logs every request into `ml-pkgs-cdn/requests/...`.

## Live smoke tests

Set a live registry base URL in `.env` to run end-to-end smoke tests against a
deployed Worker:

```dotenv
REGISTRY_E2E_BASE_URL=https://registry.pkgs.ml
REGISTRY_E2E_PACKAGE_LOCATOR=github.com/leostera/riot-new/packages/kernel
REGISTRY_E2E_ROOT_AUTH_TOKEN=
REGISTRY_E2E_PUBLISH_PACKAGE_LOCATOR=github.com/owner/repo/path/to/public-package
REGISTRY_INDEX_E2E_CDN_BASE_URL=https://cdn.pkgs.ml
REGISTRY_INDEX_E2E_BASE_PATH=index/v1
```

Then run:

```bash
bun run test:e2e
```

The live tests are skipped when `REGISTRY_E2E_BASE_URL` is not set.
The live publish smoke test is skipped unless `REGISTRY_E2E_ROOT_AUTH_TOKEN` is
set. If `REGISTRY_E2E_PUBLISH_PACKAGE_LOCATOR` is omitted, it defaults to
`REGISTRY_E2E_PACKAGE_LOCATOR`.

`bun run test` only runs the local unit suite. The live registry smoke tests are
kept behind `bun run test:e2e`.
