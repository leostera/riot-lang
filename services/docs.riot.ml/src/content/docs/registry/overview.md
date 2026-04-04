---
title: Registry Overview
description: How pkgs.ml, api.pkgs.ml, cdn.pkgs.ml, and docs.pkgs.ml fit together.
---

Riot’s package distribution story is split across four public surfaces:

- `pkgs.ml`: the human registry UI
- `api.pkgs.ml`: the registry control-plane API
- `cdn.pkgs.ml`: the sparse index and immutable artifact downloads
- `docs.pkgs.ml`: generated package documentation

## What belongs where

### pkgs.ml

This is the user-facing registry:

- package pages
- author pages
- stats
- activity
- API tokens
- privacy docs
- API overview

### api.pkgs.ml

This is the control plane:

- package publish
- package search
- package views
- events
- stats views
- auth and account actions

### cdn.pkgs.ml

This is the read plane:

- sparse index config
- sparse package documents
- published install artifacts
- Riot metadata and binary downloads
- OCaml toolchain downloads

### docs.pkgs.ml

This is reserved for generated package documentation. It should host package
docs, not generic registry docs.

## Artifact model

Published packages are artifact-native. Clients upload a package-root `tar.gz`
to the registry, and the published install artifact becomes the canonical
download unit for that release.

That means:

- the package artifact is immutable
- the sparse index points at the install artifact
- installers do not need repository-layout knowledge to unpack a package

## What Riot the CLI does here

The normal CLI path is:

1. `riot add` and `riot update` resolve through the sparse index and rewrite
   `riot.lock`
2. `riot search` queries `api.pkgs.ml`
3. `riot publish` uploads a package-root artifact to `api.pkgs.ml`
4. installed artifacts are fetched from `cdn.pkgs.ml`

That split is intentional: the API owns mutable control-plane actions, while
the CDN owns the read path for immutable package data.

## Related RFDs

- [RFD0022 Riot Package Registry](/rfds/rfd0022-riot-package-registry/)
- [RFD0023 Riot Package Index](/rfds/rfd0023-riot-package-index/)
- [RFD0026 Riot Package Management](/rfds/rfd0026-riot-package-management/)
- [RFD0028 Local Artifact Publishing](/rfds/rfd0028-local-artifact-publishing/)
