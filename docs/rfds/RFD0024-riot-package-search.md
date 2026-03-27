# RFD0024 - Riot Package Search

- Feature Name: `riot_package_search`
- Start Date: `2026-03-27`
- Status: `implemented`
- RFD PR: [leostera/riot#0000](https://github.com/leostera/riot/pull/0000)
- Riot Issue: [leostera/riot#0000](https://github.com/leostera/riot/issues/0000)

## Summary
[summary]: #summary

This RFD proposes `services/package-search`, an API-only search service for
published Riot packages.
It consumes `package.indexed`, builds a denormalized search corpus in D1, and
serves fuzzy package search over package names, descriptions, and source
provenance using SQLite FTS5.

The service is intentionally narrow.
It does not solve dependencies, publish packages, render a web UI, or own the
package index format.
It only answers search queries over packages that are already indexed and
installable.

## Motivation
[motivation]: #motivation

The registry and sparse index now give Riot a coherent publish-and-install
story:

- `tusk publish` creates a named release
- `services/registry` synchronously writes `index/v1/...`
- `tusk add <package>` can install from the sparse package index

That is enough for correctness, but not for discovery.
Without a search system, package names must be learned out of band or guessed.
That is acceptable for early smoke tests and private deployments, but it is not
enough for a real package ecosystem.

The search problem is adjacent to, but distinct from, dependency solving:

- dependency solving wants a tiny, exact, semver-oriented package document
- search wants richer text fields and ranking behavior
- publish should remain authoritative, but not become a read-heavy search API

The search service should therefore be a separate read model built from
`package.indexed`.
That event is the right trigger because it means:

- the package name is already claimed
- the release is already indexed
- the package is already installable through `tusk add <package>`

This gives downstream systems a clean invariant:

- if search returns a package, that package is already visible in the package
  index and installable

### Use cases this RFD addresses

- A user wants to search for `kernel` and discover the published `kernel`
  package by exact name.
- A user searches for `mint tea tui` and finds `minttea` because the package
  description and repository metadata match the query.
- A user searches for `leostera riot kernel` and finds a package because the
  source owner, repo, and package subdirectory are indexed.
- Another downstream service wants a queryable package catalog, but should not
  need to read every package document under `index/v1/`.

## Guide-level explanation
[guide-level-explanation]: #guide-level-explanation

Package search should be understood in terms of five concepts:

- `search corpus`
  The denormalized package metadata stored in D1 for searching.
- `search row`
  One row per published package name, representing the latest indexed state of
  that package.
- `search document`
  The concatenated text fields indexed by FTS5 for a package.
- `query`
  A user-supplied string such as `kernel`, `mint tea tui`, or
  `leostera/minttea`.
- `result`
  One package summary returned by the API, always one entry per package.

### Contributor model

Contributors should think about `services/package-search` as a derived,
downstream discovery service.

It does not decide whether a package exists.
It only makes already-indexed packages searchable.

The source of truth stays in:

- `claims/<package>.json`
- `releases/<package>/<version>.json`
- `index/v1/...`
- immutable source manifests under `packages/<locator>/<sha>.manifest.json`

The search service reads those derived artifacts and projects them into a
search-oriented schema.

### Example: publish then search

Assume `tusk publish` successfully publishes:

```toml
name = "kernel"
version = "0.0.1"
public = true
description = "Actor runtime kernel primitives for Riot"
license = "Apache-2.0"
homepage = "https://riot.ml"
repository = "https://github.com/leostera/riot-new"
root_module = "Kernel"
```

The flow is:

1. the registry writes `claims/kernel.json`
2. the registry writes `releases/kernel/0.0.1.json`
3. the registry updates `index/v1/ke/rn/kernel.json`
4. the registry emits `package.indexed`
5. `services/package-search` consumes `package.indexed`
6. it reads the latest package document and release/manifest metadata
7. it upserts one search row for `kernel` into D1
8. a later request to `search.pkgs.ml/?q=kernel` returns a single `kernel`
   result

### Example: fuzzy search over metadata

If a package is named `minttea`, but its description mentions `terminal UI`,
and the repo URL is `https://github.com/leostera/minttea`, then these queries
should all be able to surface it:

- `minttea`
- `mint tea`
- `terminal ui`
- `leostera minttea`

The package name should still rank highest, but the description and source
metadata should be searchable too.

### Example: one result per package

If a package has versions:

- `0.1.0`
- `0.2.0`
- `0.3.0`

then search should return one result for that package, not one result per
version.
The result should represent the latest indexed release and may include summary
metadata such as:

- latest version
- latest published time
- release count

### API shape

The search service is API-only in v1.
It does not render HTML.

The primary endpoint is:

```text
GET /?q=<query>
```

Example:

```text
GET https://search.pkgs.ml/?q=kernel
```

Response shape:

```json
{
  "query": "kernel",
  "results": [
    {
      "name": "kernel",
      "latest_version": "0.0.1",
      "description": "Actor runtime kernel primitives for Riot",
      "license": "Apache-2.0",
      "homepage": "https://riot.ml",
      "repository": "https://github.com/leostera/riot-new",
      "root_module": "Kernel",
      "canonical_locator": "github.com/leostera/riot-new/packages/kernel",
      "repo_owner": "leostera",
      "repo_name": "riot-new",
      "subdir": "packages/kernel",
      "release_count": 1,
      "updated_at": "2026-03-27T17:40:00Z"
    }
  ]
}
```

## Reference-level explanation
[reference-level-explanation]: #reference-level-explanation

## 1. Service boundary

`services/package-search` owns:

- consuming `package.indexed`
- reading indexed package metadata
- maintaining a search-oriented D1 schema
- answering search queries over package metadata

It does not own:

- package publication
- package indexing
- dependency solving
- docs rendering
- package checks

## 2. Trigger and source of truth

The search service consumes:

- `package.indexed`

not:

- `package.published`

This is important because `package.indexed` means the package is already
installable through the sparse package index.

On each `package.indexed` event, the service should:

1. read the package document referenced by `package_index_key`
2. identify the latest release or the specific indexed release from the event
3. read any additional metadata needed from the authoritative release record
   and immutable source manifest
4. upsert one search row for the package name

This keeps search aligned with the install surface.

## 3. Metadata requirements

The current registry manifest stores:

- package name
- package version
- public flag
- dependencies
- source provenance

Search needs richer fields.
The publish/materialization metadata should therefore be extended to include:

- `description`
- `license`
- `homepage`
- `repository`
- `root_module`

These fields should be optional, but when present they should be persisted in:

- immutable source manifests
- indexed package documents when appropriate
- the D1 search row

## 4. D1 schema

The initial schema should be one row per package plus one FTS5 virtual table.

Example logical schema:

```sql
CREATE TABLE packages (
  package_name TEXT PRIMARY KEY,
  latest_version TEXT NOT NULL,
  description TEXT,
  license TEXT,
  homepage TEXT,
  repository TEXT,
  root_module TEXT,
  canonical_locator TEXT NOT NULL,
  repo_url TEXT NOT NULL,
  repo_owner TEXT NOT NULL,
  repo_name TEXT NOT NULL,
  subdir TEXT NOT NULL,
  release_count INTEGER NOT NULL,
  updated_at TEXT NOT NULL
);

CREATE VIRTUAL TABLE package_search USING fts5(
  package_name,
  description,
  repo_owner,
  repo_name,
  subdir,
  repository,
  content='packages',
  content_rowid='rowid'
);
```

The exact SQL may vary depending on D1 constraints, but the intended model is:

- one canonical package row
- one FTS5 document per package row

## 5. Query semantics

Search should be fuzzy in the sense of broad text matching and ranking, not
edit-distance spell correction.

The v1 query behavior should combine:

- exact package-name boost
- prefix package-name boost
- FTS5 ranking over:
  - package name
  - description
  - repo owner
  - repo name
  - package subdirectory
  - repository URL

The ranking policy should prefer:

1. exact package name matches
2. prefix package name matches
3. strong FTS matches in package name or description
4. weaker provenance matches such as owner/repo/subdir

This mirrors the useful parts of crates.io and Hex:

- crates.io explicitly boosts exact name matches before full-text ranking
- Hex matches both package names and descriptions and treats `repo/package`
  shaped queries as meaningful

## 6. API surface

The initial API surface should stay small:

- `GET /?q=<query>`

Optional query parameters may include:

- `limit`
- `offset`

The response should return:

- the normalized query
- one result per package
- enough metadata to render CLI or web search results later

The API should not return every release.
It is a discovery surface, not a package-history endpoint.

## 7. Write model

The write path should be idempotent.

On reprocessing the same `package.indexed` event:

- if the package row is already current, no semantic change occurs
- the FTS row remains in sync
- duplicate downstream effects are avoided

If the package latest release changes because a newer version was indexed, the
service should:

- update the main package row
- rebuild the corresponding FTS entry

## 8. Storage and deployment

The service should be deployed as a Worker bound to:

- D1 for search state
- the package bucket for reading indexed package documents and manifests
- a queue consumer for `package.indexed`

The public endpoint is:

```text
https://search.pkgs.ml/?q=<query>
```

No HTML front page is part of v1.

## Drawbacks
[drawbacks]: #drawbacks

- Search becomes another derived system that can lag behind publish/index if
  queue processing stalls.
- D1 introduces another stateful subsystem to operate and migrate.
- FTS5 gives strong text search, but not true typo correction.
- Search quality depends on package authors providing useful metadata.

## Rationale and alternatives
[rationale-and-alternatives]: #rationale-and-alternatives

### Why not search directly over `index/v1/...`?

The sparse index is optimized for dependency solving, not text search.
Walking every package document on every query would be wasteful and slow.

### Why not fold search into `services/registry`?

The registry already owns source materialization, name claims, publish, and
index writes.
Adding live query search to that same Worker would blur responsibilities again.

### Why not use a static JSON search corpus?

A static corpus is simpler, but ranking and fuzzy matching become awkward and
push more work to the client or the Worker on every request.
D1 + FTS5 is a better fit for server-side search queries.

### Why not use Cloudflare AI search products?

This problem is package lookup, not semantic retrieval.
FTS5 is the more appropriate and more predictable primitive.

## Prior art
[prior-art]: #prior-art

- crates.io uses exact-name boosting plus full-text ranking over search text.
  The search controller in `3rdparty/crates.io/src/controllers/krate/search.rs`
  orders exact crate-name matches ahead of broader text relevance.
- Hex matches both package names and descriptions, and treats
  `repository/package`-shaped searches as meaningful. See
  `3rdparty/hexpm/lib/hexpm/repository/package.ex`.
- SQLite FTS5 is a mature, lightweight full-text search engine and is already
  supported by Cloudflare D1.

Riot should borrow:

- exact-name boosting from crates.io
- owner/repo-aware matching from Hex
- a small API surface tailored to package discovery

## Unresolved questions
[unresolved-questions]: #unresolved-questions

- How much search metadata should be mandatory vs optional in `tusk.toml`?
- Should v1 expose cursor pagination instead of `limit` + `offset`?
- Do we want light synonym normalization, such as treating `-`, `_`, and
  spaces more uniformly in queries and indexed documents?
- Should search rank packages with more releases or recent updates slightly
  higher when textual relevance is equal?

## Future possibilities
[future-possibilities]: #future-possibilities

- Add a small HTML frontend later without changing the search API.
- Add typo correction or query suggestions on top of FTS5 results.
- Add filters such as `license:`, `owner:`, or `depends:` once there is demand.
- Add download counts or package-check health as ranking signals.
- Add package docs indexing later, but keep it as a separate corpus or field
  family rather than conflating it with the initial package metadata search.
