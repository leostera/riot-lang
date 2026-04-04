---
title: "RFD0024 - Riot Package Search"
description: "Riot Request for Discussion · implemented"
---

> Canonical source: `docs/rfds/RFD0024-riot-package-search.md`

> Status: **Implemented**

- Feature Name: `riot_package_search`
- Start Date: `2026-03-27`
- Status: `implemented`
- RFD PR: [leostera/riot#0000](https://github.com/leostera/riot/pull/0000)
- Riot Issue: [leostera/riot#0000](https://github.com/leostera/riot/issues/0000)

## Summary
[summary]: #summary

Riot package search is now part of `services/api.pkgs.ml`.
It is exposed at:

```text
GET https://api.pkgs.ml/v1/search?q=<query>
```

The implementation uses Cloudflare D1 plus SQLite FTS5 and maintains one
search row per published package.

Search is a discovery read model.
It only returns packages that are already published, indexed, and installable.

## Motivation
[motivation]: #motivation

The sparse index is optimized for exact installs, not discovery.
Without search, users must already know package names.

Riot needs a search surface that can answer queries over:

- package names
- descriptions
- repository owner and repo
- source subdirectory
- package metadata such as root module, license, and latest version

At the same time, search should not become the authority on whether a package
exists.
That authority stays in the registry's publish and index data.

## Final design
[guide-level-explanation]: #guide-level-explanation

### Service boundary

Search is no longer a separate worker.
It is a read path inside `services/api.pkgs.ml`.

That means:

- one API origin: `api.pkgs.ml`
- one D1 database for control-plane and search data
- one publish flow that can update publish records, index data, search data,
  and web views together

### Search model

Search keeps one row per package, representing the latest indexed state of the
package.

The search row is derived from the package's sparse index document and latest
release metadata.
It is intentionally package-oriented, not release-oriented.

That means a package with ten releases still returns one result.

### Indexed fields

The current search corpus includes:

- package name
- normalized package name
- latest version
- description
- license
- homepage
- repository
- root module
- canonical source locator
- repo owner
- repo name
- source subdirectory
- release count
- updated timestamp

This is enough for fuzzy matching on:

- exact package names
- partial package names
- owner or repo names
- source provenance
- natural-language description text

### Query semantics

The API is:

```text
GET /v1/search?q=<query>
```

Behavior:

- empty or missing query returns route metadata
- a match returns one entry per package
- a miss returns an empty `results` array, not an error

The ranking model is intentionally simple:

1. exact normalized package-name match
2. package-name prefix match
3. package-name substring match
4. FTS rank fallback

That keeps obvious package-name queries fast and intuitive without overbuilding
ranking logic too early.

## Update model
[reference-level-explanation]: #reference-level-explanation

Search updates happen synchronously during publish-time indexing.

The flow is:

1. publish succeeds
2. sparse index document is updated
3. search row is upserted in D1
4. `package.searchable` is emitted

This means the search invariant is:

- if search returns a package, that package is already installable through the
  sparse index

## Why D1 + FTS5

The current implementation uses D1 because it is already the registry's
control-plane database and FTS5 is sufficient for the initial search problem.

That gives Riot:

- low operational overhead
- one row per package
- transactional updates alongside other publish-time metadata work
- acceptable fuzzy search for the current corpus size

If search requirements later exceed what D1 and FTS5 provide, the external API
shape can stay stable while the implementation changes.

## Relationship to `riot add`

Search is not on the install critical path.
It exists for discovery.

Future `riot add` should use:

- search for package discovery UX
- the sparse index for actual install resolution

That distinction is important:

- `riot add kernel` should not require search to work
- search should help users find `kernel`
- the sparse index should remain the authoritative install fast path
