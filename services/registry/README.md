# riot-package-registry

Cloudflare Worker scaffold for Riot's package publication service.

Production endpoint: `https://api.pkgs.ml`

## Local development

1. Install dependencies with `bun install`.
2. Create a `.env` file next to `wrangler.toml`.
3. Run `bun run dev`.

Apply D1 schema changes with migrations before first run:

```bash
bun run migrate
```

For production databases:

```bash
bun run migrate:remote
```

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
- `GET /v1/packages/<locator>/resolve?ref=<selector>` materializes or reuses a source-backed package snapshot.
- `GET /v1/packages/<locator>/manifest/<sha>.json` reads immutable manifests from R2.
- `GET /v1/packages/<locator>/source/<sha>.tar.gz` redirects to immutable source archives on `cdn.pkgs.ml`.
- `GET /v1/auth/github/start?return_to=<url>` starts GitHub OAuth login.
- `GET /v1/auth/github/callback?code=<code>&state=<state>` completes GitHub OAuth, creates a user session, and redirects back to `pkgs.ml`.
- `POST /v1/auth/logout` clears the session cookie.
- `GET /v1/me` returns the authenticated user session, if one exists.
- `GET /v1/search?q=<query>` returns one search result per indexed package, backed by D1 + FTS5.
- `GET /v1/me/tokens` lists publish tokens for the authenticated user.
- `POST /v1/me/tokens` creates a new publish token and returns the plaintext token once.
- `DELETE /v1/me/tokens/<token-id>` revokes a publish token.
- `POST /v1/packages/<locator>/publish?ref=<selector>` publishes a named package release, synchronously updates the sparse package index under `cdn.pkgs.ml/index/v1`, updates the registry search database, and accepts either `Authorization: Bearer <ROOT_AUTH_TOKEN>` or a user publish token created through `/v1/me/tokens`.

Legacy compatibility aliases under `registry.pkgs.ml` and `/api/v1`/`/package/.../-/...` remain available during the transition.

The Worker logs every request into `ml-pkgs-cdn/requests/...`.
The Worker also uses a D1 binding for registry control-plane metadata:
auth, sessions, API tokens, package claims, published releases, search, and
derived web views.

## Live smoke tests

Set a live registry base URL in `.env` to run end-to-end smoke tests against a
deployed Worker:

```dotenv
REGISTRY_E2E_BASE_URL=https://api.pkgs.ml
REGISTRY_E2E_PACKAGE_LOCATOR=github.com/leostera/riot-new/packages/kernel
REGISTRY_E2E_ROOT_AUTH_TOKEN=
REGISTRY_E2E_PUBLISH_PACKAGE_LOCATOR=github.com/owner/repo/path/to/public-package
REGISTRY_E2E_SESSION_COOKIE=pkgs_session=...
REGISTRY_E2E_GITHUB_LOGIN=leostera
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
`REGISTRY_E2E_PACKAGE_LOCATOR`. The authenticated token-management smoke tests
are skipped unless both `REGISTRY_E2E_SESSION_COOKIE` and
`REGISTRY_E2E_GITHUB_LOGIN` are set.

`bun run test` only runs the local unit suite. The live registry smoke tests are
kept behind `bun run test:e2e`.
