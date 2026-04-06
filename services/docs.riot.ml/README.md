# docs.riot.ml

Self-hosted documentation for Riot, built with Astro and Starlight.

## Local development

```sh
bun install
bun run dev
```

## Build

```sh
bun run build
```

This generates a static site in `dist/`.

## Deploy

```sh
bun run deploy
```

Deployment:

1. syncs the RFD content into the Starlight docs tree
2. builds the static site with Astro
3. deploys `dist/` through Wrangler assets to `docs.riot.ml`
