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
