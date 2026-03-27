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

## Current scaffold

- `GET /` returns service metadata.
- `GET /package/<locator>/-/resolve?ref=<selector>` resolves already-cached SHA publications.
- `GET /package/<locator>/-/manifest/<sha>.json` reads immutable manifests from R2.
- `GET /package/<locator>/-/source/<sha>.tar.gz` reads source archives from R2.

The Worker logs every request into `ml-pkgs-cdn/requests/...`.
Upstream GitHub resolution and publication are still the next step.
