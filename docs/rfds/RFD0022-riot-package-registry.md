# RFD0022 - Riot Package Registry

- Feature Name: `riot_package_registry`
- Start Date: `2026-03-27`
- Status: `implemented`
- RFD PR: [leostera/riot#0000](https://github.com/leostera/riot/pull/0000)
- Riot Issue: [leostera/riot#0000](https://github.com/leostera/riot/issues/0000)

## Summary
[summary]: #summary

Riot's package registry lives in `services/registry` and is deployed at
`https://api.pkgs.ml`.

It is the single control-plane service for:

- source package materialization from public source locators such as
  `github.com/owner/repo[/path]`
- explicit authenticated package publication
- package-name claims
- immutable published release records
- synchronous sparse-index updates
- synchronous search updates
- derived web-view generation for `pkgs.ml`

The final model is intentionally explicit:

- `tusk add github.com/owner/repo[/path]` is a source dependency flow
- `tusk publish` is the only way to claim a public package name
- `tusk add <package-name>` installs from the sparse index
- `tusk add` never implicitly publishes or claims names

GitHub is the first supported upstream provider.
The public API is provider-neutral enough that more providers can be added
later.

## Motivation
[motivation]: #motivation

The design pressure was to support both:

1. source-based installs that feel like Go
2. named-package installs that feel fast like Bun or Cargo

The registry therefore separates two concerns:

- source materialization
- public package publication

That gives Riot a clean mental model:

- source locators identify where code comes from
- published package names identify stable install targets

The registry deliberately does not own:

- dependency solving
- lockfile generation
- workspace resolution
- compatibility selection
- build execution

Those belong in `tusk`.

## Final user model
[guide-level-explanation]: #guide-level-explanation

### Source installs

`tusk add github.com/leostera/minttea`

This asks the registry to:

1. normalize the source locator
2. resolve the requested selector, defaulting to `main`
3. fetch and store the source tarball in R2 if needed
4. inspect the package and write an immutable manifest if needed
5. return a concrete resolved SHA and immutable artifact locations

This flow does not:

- claim a package name
- create a published release
- update the sparse index

### Named publishes

`tusk publish`

This asks the registry to:

1. materialize the source package if needed
2. validate that the package is publishable
3. authenticate the publisher
4. claim the package name if allowed
5. create the immutable published release record
6. synchronously update the sparse index
7. synchronously update search and derived web views
8. emit lifecycle events

After a successful publish, the package is immediately available through:

- `GET /v1/search`
- the sparse index under `cdn.pkgs.ml/index/v1/...`
- `pkgs.ml`

### Named installs

`tusk add minttea`

This does not go through GitHub.
It should eventually use:

1. `GET /v1/search` only for discovery UX, if needed
2. the sparse index config at `cdn.pkgs.ml/index/v1/config.json`
3. the package shard document at `cdn.pkgs.ml/index/v1/...`
4. the immutable tarball URL referenced by the chosen release

## Authentication model

The current implemented auth model is:

- GitHub OAuth for user identity
- session cookies for `pkgs.ml`
- API tokens for `tusk publish`
- temporary `ROOT_AUTH_TOKEN` support for operator and e2e flows

Users authenticate through:

- `GET /v1/auth/github/start`
- `GET /v1/auth/github/callback`

Users can then create publish tokens through:

- `GET /v1/me/tokens`
- `POST /v1/me/tokens`
- `DELETE /v1/me/tokens/<token-id>`

Tokens are currently scoped to:

- `publish`

## Package-name and release rules

The implemented publish rules are:

1. package names are globally unique
2. a package name may only be claimed by one canonical source lineage
3. versions must be valid semver
4. versions are immutable

The version immutability rule is important:

- same package name + same version + same source SHA: publish short-circuits as
  an idempotent success
- same package name + same version + different source SHA: publish fails with
  conflict

This means Riot does not support overwriting an already-published version.

## Publish validation rules

The registry currently enforces:

- `package.public = true`
- `package.name` exists
- `package.version` is semver-valid
- `package.description` exists
- `package.license` exists and is SPDX-compatible
- semver dependencies must already exist in the registry
- git dependencies are allowed if the git reference parses

The current exception is compiler-shipped OCaml libraries.
These are treated as built-in dependencies and do not require publication:

- `stdlib`
- `unix`
- `dynlink`

Workspace publish ordering is intentionally not solved in the registry.
That orchestration belongs in `tusk publish`.

## API surface
[reference-level-explanation]: #reference-level-explanation

The main registry endpoints are:

- `GET /`
- `GET /v1/packages/<locator>/resolve?ref=<selector>`
- `GET /v1/packages/<locator>/manifest/<sha>.json`
- `GET /v1/packages/<locator>/source/<sha>.tar.gz`
- `POST /v1/packages/<locator>/publish?ref=<selector>`
- `GET /v1/search?q=<query>`
- `GET /v1/events?limit=<count>&after=<event-id>`
- `GET /v1/packages/<package-name>/events?version=<version>&limit=<count>`
- `GET /v1/views/packages/<package-name>/overview`
- `GET /v1/views/packages/<package-name>/relations`
- `GET /v1/views/recent/packages`
- `GET /v1/views/popular/packages`
- `GET /v1/views/categories`
- `GET /v1/views/owners/<github-login>/packages`

Legacy aliases under `registry.pkgs.ml`, `/api/v1`, and `/package/.../-/...`
still exist during the transition, but the intended stable API is
`api.pkgs.ml/v1/...`.

## Storage model

The implementation is database-first for control-plane data and R2-first for
heavy immutable artifacts.

### D1

Cloudflare D1 stores:

- users
- sessions
- oauth state
- api tokens
- package claims
- published releases
- selector resolutions
- request-driven registry events
- search rows and FTS tables
- derived web-view documents

### R2

Cloudflare R2 stores:

- immutable source tarballs
- immutable source manifests
- sparse package-index files
- request logs
- cached user avatars
- exported D1 backups

## Events

The registry records lifecycle events in D1 and exposes them through
`/v1/events`.

The currently emitted package lifecycle events are:

- `package.submitted`
- `package.verified`
- `package.published`
- `package.searchable`
- `package.indexed`

These events drive:

- the public activity page
- debugging of publish pipelines
- future downstream systems such as docs, security checks, or analytics

## Relationship to `tusk add` and `tusk publish`

This RFD captures the registry contract that `tusk` should build against.

`tusk add` needs two modes:

- source mode via `resolve`
- named-package mode via the sparse index

`tusk publish` needs:

- Git-aware package locator detection
- auth token support
- workspace publish ordering
- clear handling of immutable version conflicts

Those client-side concerns are intentionally deferred to a later Tusk package
management RFD.
