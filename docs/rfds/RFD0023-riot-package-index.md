# RFD0023 - Riot Package Index

- Feature Name: `riot_package_index`
- Start Date: `2026-03-27`
- Status: `implemented`
- RFD PR: [leostera/riot#0000](https://github.com/leostera/riot/pull/0000)
- Riot Issue: [leostera/riot#0000](https://github.com/leostera/riot/issues/0000)

## Summary
[summary]: #summary

This RFD proposes Riot's sparse, static package index for `tusk`.
The current implementation lives inside `services/registry` so that publish
blocks until the package is visible in the index, while the external index
format stays the same.

The index is intentionally read-optimized.
It is not responsible for source materialization, authentication, package-name
claims, or fetching from GitHub.
Those responsibilities stay in `services/registry`.

The index publishes:

- one root `config.json`
- one sharded JSON document per published package name
- one `package.indexed` event after a package document changes

The index only contains explicitly published packages with unique public names
and semver-valid versions.
Direct source dependencies such as `tusk add github.com/leostera/minttea` do
not appear in the index unless an authenticated author later publishes them.

## Motivation
[motivation]: #motivation

`services/registry` now gives Riot a clean source-backed publication model:

- `tusk add github.com/owner/repo[/path]` materializes a source dependency
- `tusk publish` explicitly binds a public package name and semver version to a
  source materialization

That solves provenance and durability, but it is not yet a fast package-manager
read path.
If normal installs had to consult the registry Worker or GitHub on every
request, Riot would never approach the ergonomics or speed of mature package
managers.

The hot path for `tusk add kernel` should instead look like this:

1. fetch a tiny package metadata document from `cdn.pkgs.ml`
2. solve locally
3. download an immutable source archive by SHA from `cdn.pkgs.ml`

That requires a read model built from publish events.

Cargo provides the most relevant serving model.
Its sparse index serves one sharded metadata file per package instead of one
global mutable catalog.
Bun provides the most relevant client-side lesson: fetch package metadata on
demand, cache it locally, and keep the install path off the origin when
possible.

Riot should combine those ideas:

- Cargo-style sparse sharding on the network
- Bun-style aggressive local metadata caching in `tusk`
- source-backed immutable artifacts underneath every indexed release

## Guide-level explanation
[guide-level-explanation]: #guide-level-explanation

The package index should be understood in terms of four concepts:

- `published package name`
  The globally unique name claimed by `tusk publish`, such as `kernel` or
  `minttea`.
- `release`
  One semver version of a published package name, such as `kernel@0.0.1`.
- `source provenance`
  The canonical source locator, repo URL, package subdirectory, and resolved
  SHA that identify where a release came from.
- `package document`
  The sharded JSON file that contains all published releases for one package
  name.

### Contributor model

Contributors should think about the index as a derived cache, not as the
authority on package publication.

The authority stays in:

- immutable source manifests under `packages/<locator>/<sha>.manifest.json`
- mutable publish records under `claims/<package>.json` and
  `releases/<package>/<version>.json`

The registry's index-building path turns those records into a fast static view
for `tusk`.

### Example: `tusk publish` then `tusk add kernel`

Assume an author publishes:

```toml
name = "kernel"
version = "0.0.1"
public = true
```

from:

```text
github.com/leostera/riot-new/packages/kernel
```

The flow is:

1. `tusk publish` asks `services/registry` to publish the package
2. the registry validates the name claim and writes:
   - `claims/kernel.json`
   - `releases/kernel/0.0.1.json`
3. the registry emits `package.published`
4. Riot reads the release record and source manifest from R2
5. it rebuilds the package document for `kernel`
7. it writes:
   - `index/v1/config.json`
   - `index/v1/ke/rn/kernel.json`
8. it emits `package.indexed`
9. later, `tusk add kernel` fetches only `kernel.json`, solves locally, and
   downloads the immutable source tarball by SHA

### Example: direct source install stays out of the index

If a user runs:

```text
tusk add github.com/leostera/riot-new/packages/kernel
```

then Riot materializes a source dependency through `services/registry`.
No package name is claimed and no package document is updated.

Only an explicit authenticated publish should cause an index update.

### Example: two repos try to publish the same package name

If `kernel` is already claimed by one canonical source locator, then a later
attempt to publish:

```toml
name = "kernel"
```

from another locator must fail in `services/registry`.

That means the index builder can rely on a simple invariant:

- one public package name maps to one package lineage

This is the key property that keeps dependency resolution by package name
simple and deterministic.

### Fast path goals

The index exists to make the normal install path cheap.
For a package name install such as `tusk add kernel`, the hot path should be:

1. read `config.json` once and cache it
2. compute the shard path for `kernel`
3. fetch `kernel.json` with `ETag`/`If-None-Match`
4. solve locally
5. fetch the immutable source tarball directly from `cdn.pkgs.ml`

No GitHub calls should appear in this path.
No registry Worker should appear in this path.

## Reference-level explanation
[reference-level-explanation]: #reference-level-explanation

## 1. Scope

The index implementation owns:

- rebuilding package documents during publish
- validating that a release is indexable
- building sparse package documents
- writing index files to R2/CDN paths
- maintaining the root index config
- emitting `package.indexed` when a package document changes

The index implementation does not own:

- source materialization
- GitHub access
- authentication or login
- package-name claim rules
- direct source dependency installs
- full-text search
- package checks or sandbox execution

Those remain outside the index implementation.

## 2. Indexability rules

A release is indexable if and only if:

1. it came from an explicit authenticated publish
2. the package name is claimed
3. the published version is valid semver
4. the package is public
5. the name/version pair is unique

The index must not include:

- direct source-only materializations
- mutable refs like `main`
- raw SHAs without a named semver release
- unpublished private packages

## 3. Package name restrictions

To keep sharding, lookup, and dependency resolution sane, the registry should
enforce package-name restrictions compatible with the index:

- ASCII only
- only alphanumeric characters, `-`, and `_`
- first character must be alphabetic
- maximum length 64
- case-insensitive uniqueness
- `-` and `_` collisions are rejected

These restrictions belong primarily to `services/registry`, but the index
depends on them.

## 4. Serving model

The index is a sparse static tree rooted at:

```text
https://cdn.pkgs.ml/index/v1/
```

It contains:

```text
index/v1/
  config.json
  1/
    x.json
  2/
    io.json
  3/
    m/mcp.json
  ke/rn/kernel.json
  mi/nt/minttea.json
```

The sharding strategy matches Cargo's sparse index layout, adapted to `.json`
files:

- 1-character names: `1/<name>.json`
- 2-character names: `2/<name>.json`
- 3-character names: `3/<first>/<name>.json`
- 4+ character names: `<first-two>/<second-two>/<name>.json`

Sharding uses the lowercase package name.

## 5. Root config

The root config file is:

```text
index/v1/config.json
```

Its initial JSON shape is:

```json
{
  "schema_version": 1,
  "kind": "sparse",
  "package_path_strategy": "cargo-lowercase-v1",
  "index_base_url": "https://cdn.pkgs.ml/index/v1",
  "artifact_base_url": "https://cdn.pkgs.ml"
}
```

This gives `tusk` enough information to:

- compute package document URLs locally
- derive immutable manifest and source URLs from stored R2 keys

## 6. Package document schema

Each package document contains the full indexed release history for one
published package name.

Initial schema:

```json
{
  "schema_version": 1,
  "name": "kernel",
  "latest": "0.0.1",
  "updated_at": "2026-03-27T15:27:35Z",
  "releases": [
    {
      "version": "0.0.1",
      "published_at": "2026-03-27T15:27:35Z",
      "canonical_locator": "github.com/leostera/riot-new/packages/kernel",
      "repo_url": "https://github.com/leostera/riot-new",
      "subdir": "packages/kernel",
      "sha": "2aef0372bf5b6687db05bda80cde55f960cbfd9d",
      "manifest_key": "packages/github.com/leostera/riot-new/packages/kernel/2aef0372bf5b6687db05bda80cde55f960cbfd9d.manifest.json",
      "source_key": "sources/github.com/leostera/riot-new/2aef0372bf5b6687db05bda80cde55f960cbfd9d.tar.gz",
      "dependencies": [
        {
          "name": "std",
          "requirement": "*"
        }
      ]
    }
  ]
}
```

Rules:

- `releases` are sorted descending by semver
- `latest` is the highest non-yanked version
- keys, not full repeated URLs, are stored in release rows
- each `name + version` appears at most once
- package documents are mutable derived files and may be rewritten in place

Optional metadata may be added later, such as:

- `description`
- `license`
- `homepage`
- `repository`
- `root_module`

Those are not required for v1 solver behavior.

## 7. Source of truth

The publish path is the trigger, not the ultimate source of truth.

When Riot indexes a published package, it should:

1. read the published release record from:
   `releases/<package>/<version>.json`
2. read the immutable source manifest referenced by that release
3. read the current package document, if any
4. upsert the release row
5. recompute `latest`
6. rewrite the package document atomically
7. emit `package.indexed`

This keeps the index derived from durable registry records instead of trusting
only transient in-memory publish state.

For disaster recovery or initial backfill, the index can be rebuilt by walking
the `releases/` namespace and re-materializing package documents.

## 8. Update semantics

Publishing is forever.
Once `kernel@0.0.1` is published, Riot must not later publish another
`kernel@0.0.1` with different contents.

The indexer therefore applies simple update rules:

- if the release row does not exist yet, add it
- if the release row already exists with the same canonical locator and SHA,
  keep it and treat the update as idempotent
- if the release row already exists with different contents, reject it as a
  corruption or upstream invariant breach

No yanking is part of v1.
If Riot adds yanking later, it will require a schema extension and another RFD.

`package.indexed` is only emitted when the package document actually changes.
Reprocessing an identical `package.published` event should be idempotent and
should not fan out duplicate downstream work.

## 9. Client flow

For `tusk add kernel`, the intended client flow is:

1. read and cache `config.json`
2. compute the shard path for `kernel`
3. fetch `kernel.json`
4. solve locally using the release list and dependency requirements
5. choose the latest compatible release
6. fetch the immutable source archive using `source_key`
7. record the canonical locator and SHA in the lockfile

This means the install identity is the published package name, while the
provenance identity remains the canonical source locator plus SHA.

## 10. JSON first

The network format should start as JSON.

The biggest wins come from:

- sparse per-package files
- CDN hosting
- `ETag`/`304` reuse
- local `tusk` metadata caching
- immutable source archives by SHA

Those wins do not require a binary wire format.

If JSON parsing or transfer size later shows up as a real bottleneck, Riot may
introduce a binary package document format in a future schema version.
That would be an optimization of the same sparse-index model, not a different
architecture.

## 11. Search and checks stay separate

This RFD does not define:

- `services/package-search`
- `services/package-checks`
- a global search catalog

Those services may consume the same publish events and even reuse some of the
same package metadata, but they are separate read models with different
latency, ranking, and storage concerns.

The index should remain solver-oriented.

## Drawbacks
[drawbacks]: #drawbacks

- the index introduces another derived data system that must stay in sync with
  registry publish records
- per-package JSON means very large packages with long version histories may
  eventually want pagination or a binary representation
- package-name restrictions need to be enforced carefully in the publish path,
  or index paths and lookup semantics become messy

## Rationale and alternatives
[rationale-and-alternatives]: #rationale-and-alternatives

### Why not one global `index.json`?

One monolithic index file makes every install and every update heavier.
It also turns cache invalidation into a global problem.
Sparse per-package files keep updates localized and let `tusk` fetch only what
it needs.

### Why not key the index by canonical source locator?

Because Riot now has explicit authenticated publishing and globally unique
package names.
That means normal installs can use package names cleanly:

```text
tusk add kernel
```

while each indexed release still preserves the canonical source locator for
provenance and artifact fetching.

### Why not binary first?

Binary is an optimization, not the fundamental design.
Sparse static JSON plus local caching gets Riot most of the speed win without
locking the ecosystem into an early opaque format.

## Prior art
[prior-art]: #prior-art

- Cargo sparse index
  - sharded per-package metadata files
  - root `config.json`
  - localized updates instead of one giant registry catalog
- Bun/npm package metadata fetch
  - fetch package metadata on demand
  - cache it aggressively on the client

Riot should copy the sparse serving idea from Cargo and the client cache
mindset from Bun, while keeping source-backed immutable artifacts underneath
every release.
