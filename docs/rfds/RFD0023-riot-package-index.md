# RFD0023 - Riot Package Index

- Feature Name: `riot_package_index`
- Start Date: `2026-03-27`
- Status: `implemented`
- RFD PR: [leostera/riot#0000](https://github.com/leostera/riot/pull/0000)
- Riot Issue: [leostera/riot#0000](https://github.com/leostera/riot/issues/0000)

## Summary
[summary]: #summary

Riot's sparse package index is a Cargo-style sharded JSON index rooted at:

```text
https://cdn.pkgs.ml/index/v1/
```

It is implemented inside `services/registry`, not as a separate worker.
That is intentional: publish blocks until the index has been updated, so a
successful publish is immediately installable.

The index only contains explicitly published packages with unique public names.
Direct source dependencies do not appear in the index unless an author later
publishes them.

## Motivation
[motivation]: #motivation

The registry solves publication and provenance, but package installation by
name needs a much cheaper hot path.

The install fast path for:

```text
riot add kernel
```

should be:

1. fetch sparse index config once
2. compute one package shard path
3. fetch one small package document
4. solve locally in `riot`
5. download immutable artifacts by SHA

That means the index must be:

- static
- sharded
- cache-friendly
- independent from GitHub at read time

## Final design
[guide-level-explanation]: #guide-level-explanation

### Ownership

The index does not own:

- source materialization
- authentication
- package-name claims
- publish authorization
- search
- docs

Those are registry concerns or downstream concerns.

The index owns only the install read model for named packages.

### What gets indexed

A release is indexable when:

1. it was explicitly published
2. its package name is claimed
3. its version is valid semver
4. the package is public

The index does not include:

- unpublished source materializations
- mutable refs like `main`
- raw SHAs without a named release

### Sparse layout

The implemented shard layout matches the Cargo sparse index strategy with JSON
files:

```text
index/v1/
  config.json
  1/x.json
  2/io.json
  3/m/mcp.json
  ke/rn/kernel.json
  mi/nt/minttea.json
```

### Package document model

Each package shard contains one package document with:

- package name
- latest release version
- updated timestamp
- all published releases for that package

Each release entry includes:

- version
- published timestamp
- canonical source locator
- repo URL
- repo subdirectory
- resolved SHA
- description
- license
- homepage
- repository
- root module
- categories
- keywords
- manifest key
- source key
- dependency metadata

This lets `riot` solve locally without consulting the registry Worker on the
install hot path.

## Publish-time behavior
[reference-level-explanation]: #reference-level-explanation

On successful publish, the registry now performs index work synchronously:

1. read the current package document from R2, if any
2. upsert the release into the package document
3. semver-sort releases
4. recompute `latest`
5. write the package shard back to R2 if it changed
6. emit `package.searchable`
7. emit `package.indexed`

Because this work happens inline with publish, the user-visible behavior is:

- publish succeeds
- the package is already visible in the sparse index
- `riot add <package>` can use it immediately

## Root config

The root config document at `index/v1/config.json` tells clients:

- index kind
- shard strategy
- index base URL
- artifact base URL

This gives `riot` a single bootstrap file to cache and use for all future
named-package installs.

## Client contract for future `riot add`

The index is the main contract for named installs.

`riot add <package>` should:

1. fetch `config.json`
2. compute the shard path for the package name
3. fetch the package document
4. select a compatible version locally
5. fetch the immutable source tarball referenced by the chosen release

The index deliberately does not choose versions for the client.
That belongs in Riot's package-management layer.
