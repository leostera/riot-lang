# docs.riot.ml

This service now uses Mintlify for authoring and navigation.

## Local development

1. Install the Mintlify CLI:

```sh
bun x --bun mint --help
```

2. Sync the repository RFDs into the local docs tree:

```sh
npm run sync:rfds
```

3. Start the local preview:

```sh
npm run dev
```

Mintlify serves the site locally at `http://localhost:3000`.

The service scripts force Mintlify to run under Bun. That avoids Node 25 compatibility failures from
Mintlify’s CLI runtime check.

## Layout

- `docs.json`: Mintlify site configuration
- `index.mdx`: landing page
- `getting-started/`: onboarding and first steps
- `tooling/`: CLI, workflows, and machine-readable interfaces
- `runtime/`: runtime and standard library concepts
- `registry/`: package registry and publishing docs
- `rfds/`: synced Request for Discussion pages

## Deployment

`wrangler.toml` is still the domain entrypoint for `docs.riot.ml`.

The Worker proxies traffic to the configured Mintlify origin. Set `MINTLIFY_ORIGIN` to your
Mintlify hostname before deploying, for example:

```sh
wrangler secret put MINTLIFY_ORIGIN
```

Then deploy with:

```sh
npm run deploy
```
