---
title: Publishing Packages
description: How Riot publishes packages to pkgs.ml and what the registry expects.
---

Riot publishes packages as local package artifacts.

That means the unit of publication is not a raw repository archive. It is a
package-root `tar.gz` whose root contains the package manifest and package
files directly.

## The normal flow

1. Save a publish token with `riot login`
2. Run local checks with `riot publish --dry-run`
3. Publish with `riot publish`

```sh
riot login
riot publish --dry-run
riot publish
```

To publish a specific package from a workspace:

```sh
riot publish -p my-package
```

## What the registry expects

The registry expects:

- a package-root artifact
- `tusk.toml` at archive root
- publishable dependency shapes
- authenticated publishing through a `pkgs.ml` token

The important invariant is that the published artifact is the same shape that
clients later install. Installers do not need to know about repository roots or
subdirectories to unpack a release.

## Control plane and read plane

Publishing goes to:

- `POST https://api.pkgs.ml/v1/publish`

After a successful publish:

- package metadata is visible through `pkgs.ml`
- search and views come from `api.pkgs.ml`
- sparse index and artifacts are served from `cdn.pkgs.ml`

## Tokens

Publish tokens are managed through your `pkgs.ml` account and saved locally by
`riot login`.

The web UI shows a token exactly once when created. After that, only token
metadata and a derived hash are stored by the service.

## Related docs

- [Registry Overview](/registry/overview/)
- [API and Sparse Index](/registry/api/)
- `https://pkgs.ml/api`
