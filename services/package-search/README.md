# riot-package-search

Cloudflare Worker search API for published Riot packages.

Production endpoint:

- `https://search.pkgs.ml/?q=<query>`

## Local development

1. Install dependencies with `bun install`.
2. Create a `.env` file next to `wrangler.toml`.
3. Create the D1 database and replace the placeholder ids in `wrangler.toml`:

   ```bash
   wrangler d1 create riot-package-search
   ```

4. Run `bun run dev`.

Wrangler reads `.env` during local development, and the Worker expects:

```dotenv
CDN_BASE_URL=https://cdn.pkgs.ml
INDEX_BASE_PATH=index/v1
```

## Current surface

- `GET /` returns service metadata when `q` is absent.
- `GET /?q=<query>` returns one search result per package.
- Queue consumer for `package.indexed`.

## Tests

- `bun run test` runs the local route/consumer suite.
- `bun run test:e2e` publishes a real package through `registry.pkgs.ml` and
  polls `search.pkgs.ml` until that package is searchable. It reads the live
  settings from `.env`, and the publish-path smoke only runs when
  `SEARCH_E2E_PUBLISH_PACKAGE_LOCATOR` points at a package that satisfies the
  current publish rules (`public = true`, description present, SPDX license).

The service reads indexed package documents from `ml-pkgs-cdn`, projects them
into a D1 search corpus, and serves FTS5-backed package search.
