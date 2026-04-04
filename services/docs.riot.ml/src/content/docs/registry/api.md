---
title: API and Sparse Index
description: Where to find the registry API docs and how clients should approach the index.
---

For human-facing registry API docs, start with:

- `https://pkgs.ml/api`
- `https://pkgs.ml/llms.txt`

## Client model

Registry clients should think in two planes:

### Control plane

Use `api.pkgs.ml` for:

- `POST /v1/publish`
- `GET /v1/search`
- package views
- registry events
- stats views
- auth and account flows

### Read plane

Use `cdn.pkgs.ml` for:

- `index/v1/config.json`
- sparse package shard documents
- immutable package artifacts

## Sparse index flow

1. Fetch `https://cdn.pkgs.ml/index/v1/config.json`
2. Read the sharded package document for the package you need
3. Join `artifact_base_url` and `source_key`
4. Download the install artifact directly

## Publish flow

1. Build a package-root `tar.gz`
2. Ensure `tusk.toml` is at archive root
3. Authenticate with a publish token
4. Upload the artifact to `POST https://api.pkgs.ml/v1/publish`

The current public schema docs for these endpoints are maintained on
`pkgs.ml/api`.
