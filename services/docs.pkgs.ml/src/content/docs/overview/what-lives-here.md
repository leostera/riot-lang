---
title: What lives here
description: Scope of docs.pkgs.ml and how it differs from pkgs.ml.
---

`docs.pkgs.ml` is reserved for generated package documentation.

That means this site should eventually host:

- rendered package API docs
- module and type documentation
- versioned package docs

This site should **not** be the main human registry UI. For that, use:

- `pkgs.ml` for search, package pages, stats, activity, and account management
- `api.pkgs.ml` for control-plane API access
- `cdn.pkgs.ml` for sparse index and immutable package artifact downloads

## Current status

This site is only bootstrapped right now. Generated package docs will be added
once the package documentation pipeline is in place.
