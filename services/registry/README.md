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
GITHUB_TOKEN=
```

If `GITHUB_TOKEN` can read private repositories, the registry can publish from
those upstreams. This is useful for on-premise or private testing setups.

## Current scaffold

- `GET /` returns service metadata.
- `GET /package/<locator>/-/resolve?ref=<selector>` resolves already-cached SHA publications.
- `GET /package/<locator>/-/manifest/<sha>.json` reads immutable manifests from R2.
- `GET /package/<locator>/-/source/<sha>.tar.gz` reads source archives from R2.

The Worker logs every request into `ml-pkgs-cdn/requests/...`.

## Live smoke tests

Set a live registry base URL in `.env` to run end-to-end smoke tests against a
deployed Worker:

```dotenv
REGISTRY_E2E_BASE_URL=https://registry.pkgs.ml
REGISTRY_E2E_PACKAGE_LOCATOR=github.com/leostera/riot-new/packages/kernel
```

Then run:

```bash
bun run test:e2e
```

The live tests are skipped when `REGISTRY_E2E_BASE_URL` is not set.
