# RFD0026 - Riot Package Management

- Feature Name: `riot_package_management`
- Start Date: `2026-03-30`
- Status: `presented`
- RFD PR: [leostera/riot#0000](https://github.com/leostera/riot/pull/0000)
- Riot Issue: [leostera/riot#0000](https://github.com/leostera/riot/issues/0000)

## Summary
[summary]: #summary

This RFD defines Riot's package-management model on top of Riot's existing
registry, sparse index, and search infrastructure.

It introduces:

- a package-name-first dependency model
- manifest syntax for registry, source, path, and builtin dependencies
- a mandatory `riot.lock` file that stores the resolved graph
- a PubGrub-based solver contract for runtime, build, and dev universes
- the user-facing flows for `riot add`, `riot rm`, `riot update`, and
  `riot publish`

The registry and sparse index are treated as existing infrastructure.
This RFD defines how `riot` should consume them.

## Motivation
[motivation]: #motivation

Riot now has the backend pieces for packages:

- explicit publication through `api.pkgs.ml`
- a sparse named-package index under `cdn.pkgs.ml/index/v1/...`
- search for package discovery
- immutable package provenance through canonical source locators and SHAs

What is still missing is the client-side model that makes those pieces usable
from Riot.

That model must solve several concrete problems at once:

1. users should be able to add named packages quickly:
   - `riot add std`
   - `riot add std@0.0.1`
2. users should be able to add packages directly from source:
   - `riot add github.com/owner/repo`
   - `riot add github.com/owner/repo/path/to/pkg`
   - `riot add https://github.com/owner/repo#main`
3. local workspace development should remain ergonomic with path dependencies
4. published packages must remain installable by others, even if local path
   dependencies were used during development
5. every dependency operation should produce a reproducible lockfile
6. publish must batch workspace packages in dependency order and validate that
   what is being published is already public on GitHub

The main design pressure is to avoid splitting package identity in two.
The package manager should not sometimes think a package is called
`github.com/owner/repo/path` and other times think it is called `pkg_name`.

The chosen model is:

- package name is the dependency identity
- source, path, and registry metadata are resolution inputs and provenance
- `riot.toml` stores user intent
- `riot.lock` stores the exact resolved graph

That gives Riot a model that is simple enough to teach and strong enough to
support deterministic solving and reproducible publishing.

## Guide-level explanation
[guide-level-explanation]: #guide-level-explanation

### Dependency identity

The identity of a dependency is always its package name.

That means all of these ultimately resolve to a package named, for example,
`minttea`:

```toml
[dependencies]
minttea = "^0.4.0"
minttea = { github = "leostera/minttea" }
minttea = { source = "https://github.com/leostera/minttea" }
minttea = { path = "../minttea" }
```

The different forms mean different things operationally, but they all point at
the same conceptual package identity: `minttea`.

The authoritative package name comes from the target package's `riot.toml`.

### Dependency kinds

Riot recognizes four dependency kinds:

1. registry dependencies
2. source dependencies
3. path dependencies
4. builtin/system dependencies

#### Registry dependency

This is a normal named package from the Riot registry:

```toml
[dependencies]
std = "^0.1.0"
```

Riot resolves this through the sparse index.

#### Source dependency

This uses a remote source locator directly:

```toml
[dependencies]
minttea = { github = "leostera/minttea" }
minttea = { source = "https://github.com/leostera/minttea", ref = "main" }
widgets = { source = "https://github.com/owner/repo/packages/widgets" }
```

`github = "owner/repo"` is shorthand.
The canonical form is `source = "https://github.com/..."`

`ref` is optional.
If omitted, the default is `main`.

A source dependency may also include a version requirement:

```toml
[dependencies]
minttea = { version = "^0.4.0", github = "leostera/minttea" }
```

This means:

- materialize this source package
- read its declared package name and version
- require the declared version to satisfy the given constraint

#### Path dependency

This points at a local path:

```toml
[dependencies]
b = { path = "../b" }
```

This is useful for local development.

Path dependencies may also declare a fallback non-local identity:

```toml
[dependencies]
b = { path = "../b", version = "*" }
b = { path = "../b", source = "https://github.com/owner/repo/packages/b" }
```

These are important for publishing.

The rule is:

- `path` alone is local-only and not publishable
- `path + version` is publishable because consumers can fall back to registry
  resolution
- `path + source` is publishable because consumers can fall back to cached
  source provenance

#### Builtin/system dependency

Some dependencies are shipped with the OCaml toolchain and are not published in
the Riot registry.

Examples currently include:

- `stdlib`
- `unix`
- `dynlink`

These are treated as builtin/system dependencies.

### Manifest scopes

Riot keeps dependency scopes separate:

- `[dependencies]` for runtime
- `[build-dependencies]` for build-time
- `[dev-dependencies]` for development and test-only use

`riot add` chooses the target section based on flags:

- default: `[dependencies]`
- `--build`: `[build-dependencies]`
- `--dev`: `[dev-dependencies]`

It also chooses the target manifest based on flags:

- default: current package manifest
- `--workspace`: workspace manifest
- `-p <pkg>` / `--package <pkg>`: that package manifest only

### What `riot add` does

#### Named add

```text
riot add std
riot add std@0.0.1
```

This is exact package-name resolution, not a search query.

The rough flow is:

1. resolve the workspace and target manifest
2. fetch sparse index metadata for the named package
3. solve the graph with PubGrub
4. update `riot.toml`
5. write the exact resolved graph to `riot.lock`

If the package does not exist, Riot should fail clearly and may optionally show
close matches from package search.

#### Source add

```text
riot add github.com/leostera/minttea
riot add github.com/owner/repo/path/to/pkg
riot add https://github.com/owner/repo#main
```

The flow is:

1. normalize the source locator
2. materialize it through the registry
3. inspect the fetched package manifest
4. discover the real package name and declared version
5. write the dependency entry under the real package name
6. solve the graph
7. write `riot.toml`
8. write `riot.lock`

If the package discovered from source is actually named `awesome-utils`, then:

```text
riot add github.com/leostera/fartass
```

may write:

```toml
[dependencies]
awesome-utils = { github = "leostera/fartass" }
```

That normalization is intentional.
Package name remains the identity.

`riot add` should print useful progress while doing this:

- that it is discovering the source package
- which ref it resolved
- which commit SHA it selected
- which package name it discovered
- what it wrote to `riot.toml`

### What `riot rm` does

```text
riot rm std
```

This removes a dependency from the targeted manifest section only, then
re-solves and rewrites `riot.lock`.

It does not remove from every section automatically.
There is no implicit `--all`.

### What `riot update` does

```text
riot update
```

This updates the whole workspace graph while preserving manifest requirements.

It should:

1. keep `riot.toml` unchanged
2. fetch newer package metadata as needed
3. solve again against the current constraints
4. rewrite `riot.lock`

`riot update` updates the graph, not the user requirements.

### What `riot publish` does

`riot publish` publishes the workspace batch by default.

It should:

1. enumerate workspace packages
2. skip packages that are `private = true` or `public = false`
3. sort publishable packages in dependency order
4. verify that the current code is already on GitHub
5. run mandatory local verification
6. publish each package through the registry

This means Riot, not the registry, owns workspace publish serialization.

The registry remains the authority on:

- package-name claims
- immutable versions
- publish auth
- sparse index updates

Riot owns:

- workspace traversal
- publish ordering
- local verification
- operator UX

## Reference-level explanation
[reference-level-explanation]: #reference-level-explanation

## 1. Manifest model

### 1.1 Canonical package identity

Every dependency key in `riot.toml` is a package name.

The dependency payload may constrain how that package is resolved, but it does
not replace the package identity with a locator string.

### 1.2 Dependency grammar

The dependency grammar is:

```toml
[dependencies]
pkg = "<semver requirement>"
pkg = { version = "<semver requirement>" }
pkg = { source = "<absolute source url>", ref = "<selector>" }
pkg = { github = "<owner/repo[/path]>", ref = "<selector>" }
pkg = { path = "<relative path>" }
pkg = { path = "<relative path>", version = "<semver requirement>" }
pkg = { path = "<relative path>", source = "<absolute source url>" }
pkg = { path = "<relative path>", github = "<owner/repo[/path]>" }
pkg = { version = "<semver requirement>", source = "<absolute source url>" }
pkg = { version = "<semver requirement>", github = "<owner/repo[/path]>" }
```

Rules:

- `github` is shorthand for a GitHub `source`
- `ref` applies to source dependencies
- `ref` defaults to `main`
- `path` must be relative
- `source` must be absolute
- `version` must be a valid semver requirement
- handwritten dependency keys that disagree with the actual package manifest
  should be rejected during validation

### 1.3 Publishability rules for dependency declarations

For a package to be publishable:

- `path` alone is not enough
- `path + version` is allowed
- `path + source` is allowed
- `source` alone is allowed
- `github` alone is allowed

This ensures every published dependency still has a non-local resolution path.

## 2. Lockfile model

### 2.1 `riot.lock` is mandatory

Every workspace using package management should maintain `riot.lock`.

This is true for:

- registry dependencies
- source dependencies
- path dependencies
- exact commit refs

### 2.2 Lockfile stores the resolved graph

`riot.lock` should be a resolved graph, not just a list of selected top-level
versions.

Each resolved node should include at least:

- package name
- exact version
- source kind
- canonical provenance
- resolved SHA where applicable
- checksum for immutable artifacts where applicable
- direct resolved dependencies
- scope participation as needed by the build graph

Cargo and Hex are useful prior art here:

- Cargo stores one resolved node per package with provenance and checksums
- Hex stores enough locked dependency data to avoid reloading the registry for
  normal operations

Riot should follow that general shape.

### 2.3 Provenance strings

The lockfile should preserve provenance explicitly.

Illustrative examples:

```text
registry+https://cdn.pkgs.ml/index/v1
source+https://github.com/leostera/minttea#main
path+../minttea
builtin+stdlib
```

The exact encoding can be implementation-defined, but provenance must be
preserved and round-trippable.

## 3. Solver model

### 3.1 PubGrub

Riot should use `packages/pubgrub` as the version solver.

PubGrub should operate on package names and version ranges, not on raw source
locators.

### 3.2 Three universes

Riot conceptually solves three dependency universes:

- runtime
- build
- dev

These are solved together as part of one overall resolution operation, but they
remain distinct in meaning.

Runtime is the important universe for published consumer installability.
Build and dev are for package authors and local workflows.

### 3.3 Published-package constraints

For published packages:

- runtime dependencies are part of the published install surface
- build dependencies matter for author-side builds, but not for downstream
  runtime dependency surfaces
- dev dependencies are ignored for published consumer solving

During publish validation:

- runtime must be publish-valid
- build and dev may remain local-author concerns unless a stricter policy is
  chosen later

### 3.4 Source dependencies as solver inputs

Source dependencies are not package-name ranges in the registry universe.

Instead:

1. materialize the source package
2. discover its package name and declared version
3. convert it into a concrete package input for solving

If a source dependency also declared `version = "^1.2.0"`, then the discovered
package version must satisfy that requirement.
Otherwise solving fails.

### 3.5 Sparse index contract

For registry packages, Riot should solve from the sparse index.

The sparse index contract from RFD0023 is sufficient if each release entry
provides:

- exact version
- dependency metadata
- immutable provenance
- artifact location

The solver loop should be lazy:

1. fetch `config.json`
2. fetch package shard(s) only for packages entering the frontier
3. feed release versions and dependency requirements into PubGrub
4. materialize the selected graph into `riot.lock`

Search is not on the solver hot path.

## 4. `riot add`

### 4.1 Exact package lookup

`riot add <pkg>` expects an exact package name.

If missing:

- return a direct error
- optionally perform a lightweight search query to suggest near matches

### 4.2 Source discovery

`riot add <source>` must resolve through the registry immediately.

It should not merely write a textual manifest entry without validation.

That means `riot add` is both:

- a manifest-editing command
- a resolution command

### 4.3 Automatic manifest edits

`riot add` edits `riot.toml` automatically.

If the actual package name differs from the repo name or path, Riot should use
the actual discovered package name when writing the dependency entry.

## 5. `riot rm`

`riot rm <pkg>` removes the dependency from the targeted manifest scope only and
then rewrites `riot.lock` from a fresh solve.

If the dependency does not exist in the targeted section, Riot should fail
clearly.

## 6. `riot update`

`riot update` performs a workspace-wide re-solve.

It should:

- keep dependency requirements unchanged
- update as much of the graph as the current requirements allow
- rewrite `riot.lock`

It should not silently widen or rewrite manifest requirements.

## 7. `riot publish`

### 7.1 Workspace-first publish

`riot publish` publishes the workspace batch by default.

That means it should:

1. discover workspace packages
2. filter to publishable packages
3. order them by dependency
4. publish them serially

### 7.2 Publish eligibility

A package is publishable only if:

- it is public
- its version is valid
- it passes local verification (builds correctly)
- the source commit already exists on GitHub
- its runtime dependencies are publish-valid

The registry remains the final authority, but Riot should fail locally first
where possible.

### 7.3 Credentials

Riot should store credentials in:

```text
~/.riot/config.toml
```

That file should hold the API token used for authenticated publish operations.

## Implementation

This section records the current implementation plan for the first rollout.

### Package boundaries

The package-management implementation should live in:

```text
packages/pkgs-ml
packages/riot-deps
```

`pkgs-ml` owns:

- sparse-index path computation and cache layout helpers
- registry document parsing
- registry cache reads and writes
- reusable registry access APIs that are not Riot-specific
- an in-memory registry implementation for tests that should not require
  network I/O

`riot-deps` owns:

- manifest-to-resolution orchestration
- registry/source metadata fetches
- package materialization
- lock refresh and unlock behavior
- projection of resolved packages into build-ready data

`riot-model` owns:

- the manifest-intent package model (`Package.t`)
- the resolved package model used by the builder (`Package.resolved`)
- the lockfile schema and TOML serialization (`riot.lock`)
- package-management and download event types

This split keeps package-management policy and network/materialization logic in
`riot-deps`, reusable registry mechanics in `pkgs-ml`, and the durable shared
data model in `riot-model`.

### Phase-1 ownership and responsibilities

For the first rollout, `riot-deps` should own four concrete responsibilities:

1. build dependency universes by traversing and connecting manifest
   dependencies
2. fetch all manifests needed for packages in those universes
3. run a naive solver per universe to establish exact package versions
4. present the final resolved package graph in `riot-model` terms for the
   builder

In other words, the operational flow should look like:

1. read `riot.toml` files into `Riot_model.Package.t`
2. feed those package roots into `Riot_deps.Dep_solver`
3. have `riot-deps`, using `pkgs-ml` for registry access, compute a `riot.lock`
   plus resolved package data
4. pass the resolved package data into the builder/planner

For phase 1 there should be no separate solver backend abstraction. A concrete
naive solver is enough. When PubGrub lands later, it should replace the solver
internals rather than forcing an early abstraction layer now.

### Global registry cache

Registry state should be split by configured registry name from day one. For a
registry named `pkgs.ml`, the global cache layout should be:

```text
~/.riot/registry/pkgs.ml/index/...
~/.riot/registry/pkgs.ml/archive/<package-name>/<exact-version>.tar
~/.riot/registry/pkgs.ml/src/<package-name>/<exact-version>/...
```

This keeps:

- sparse index metadata separate from package artifacts
- downloaded archives separate from extracted sources
- registry caches partitioned cleanly once multiple registries are supported

For build purposes, the canonical on-disk home of a resolved external package is
its extracted source directory under:

```text
~/.riot/registry/<registry-name>/src/<package-name>/<exact-version>/...
```

Phase 1 should use the configured registry name directly in the path, not a
generic placeholder and not a derived URL host.

### `pkgs-ml` testability

`pkgs-ml` should be configurable and usable in tests without network I/O.

That means the library should provide:

- a filesystem-backed registry rooted at the configured cache directories
- an in-memory registry that can serve sparse-index config and per-package
  documents directly from test data

`riot-deps` tests should prefer the in-memory registry whenever they are testing
solver and lockfile behavior rather than transport or on-disk cache behavior.

### Manifests stay name-based

Downloaded package manifests must not be rewritten into path-based manifests.

For example, a downloaded package may still contain:

```toml
[dependencies]
kernel = "^0.3.0"
```

The lockfile and resolved graph, not rewritten manifests, determine which exact
`kernel` node that name refers to.

This means there are three distinct layers:

1. manifest intent in `riot.toml`
2. the exact resolved graph in `riot.lock`
3. materialized package roots in `~/.riot/registry/<registry-name>/src/...`

The builder should consume the resolved graph, not re-resolve transitive
dependencies from downloaded manifests.

### End-to-end data flow

The intended data flow is:

1. `riot-model` parses local manifests into `Package.t`
2. `riot-deps` traverses dependency names and builds the runtime/build/dev
   universes
3. `riot-deps` fetches package metadata and manifests for the packages entering
   those universes
4. `riot-deps` computes exact package selections
5. `riot-deps` downloads package archives into the registry archive cache
6. `riot-deps` materializes those archives into the registry source cache
7. `riot-deps` writes `riot.lock`
8. `riot-deps` projects the resolved graph into `Riot_model.Package.resolved`
9. the builder consumes `Package.resolved`, not unresolved downloaded manifests

This means that for build purposes every external dependency is eventually a
materialized package root on disk, but it is still resolved through the lock
graph rather than by mutating manifests into path dependencies.

### Concrete example

Suppose a workspace package declares:

```toml
[dependencies]
std = "^0.1.0"
jsonrpc = "^0.2.0"
```

and the downloaded `std` manifest still says:

```toml
[dependencies]
kernel = "^0.3.0"
```

Phase 1 should work like this:

1. the workspace manifest is parsed into `Riot_model.Package.t`
2. `riot-deps` discovers the transitive universe:
   - `std`
   - `jsonrpc`
   - `kernel`
3. `riot-deps` fetches their manifests
4. the naive solver picks exact versions
5. their downloaded archives are cached at paths like:
   - `~/.riot/registry/pkgs.ml/archive/std/<version>.tar`
   - `~/.riot/registry/pkgs.ml/archive/jsonrpc/<version>.tar`
   - `~/.riot/registry/pkgs.ml/archive/kernel/<version>.tar`
6. their extracted sources are materialized at paths like:
   - `~/.riot/registry/pkgs.ml/src/std/<version>/...`
   - `~/.riot/registry/pkgs.ml/src/jsonrpc/<version>/...`
   - `~/.riot/registry/pkgs.ml/src/kernel/<version>/...`
7. `riot.lock` records that:
   - the workspace depends on exact `std`
   - exact `std` depends on exact `kernel`
   - exact `jsonrpc` depends on exact `std`
8. the builder consumes the resolved graph and the extracted source paths
   directly

At no point should `std`'s downloaded manifest be rewritten to say:

```toml
[dependencies]
kernel = { path = "..." }
```

The lockfile is what explains what `kernel` means in that context.

### Build-time flow

The build path should ensure that a build always uses the latest lock.

The flow is:

1. read workspace and package manifests into `Riot_model.Package.t`
2. check whether `riot.lock` exists
3. if `riot.lock` is missing, solve and write it
4. if any participating workspace `riot.toml` is newer than `riot.lock`,
   refresh the lock and rewrite it
5. otherwise read the existing lock
6. project the lock into `Riot_model.Package.resolved`
7. feed resolved packages into the builder/planner

The staleness check is against:

- the workspace `riot.toml`
- each workspace member `riot.toml`

It should not be driven by downloaded manifests in the global cache.

This guarantees that if a build runs, it is using the latest lock.

### Refresh vs unlock

There are two solve modes:

- lock refresh
- unlock

Lock refresh is used by:

- `riot build`
- `riot add`
- `riot rm`

Unlock is used by:

- `riot update`

Lock refresh must not behave like a full cold solve when an existing lock is
present. It should preserve the current locked selections whenever possible and
only reopen the affected frontier when required by changed manifests or missing
lock state.

Unlock is the mode that is allowed to reopen the whole graph intentionally.

The intended behavioral split is:

- `riot build`
  - solve only when the lock is missing or stale
  - otherwise trust the existing lock
- `riot add`
  - edit the manifest
  - refresh the lock conservatively around that new requirement
- `riot rm`
  - edit the manifest
  - refresh the lock conservatively after removal
- `riot update`
  - unlock and intentionally reopen the graph

Even in phase 1, this policy distinction matters, even if the actual solver is
still naive.

### Phase-1 solver

Phase 1 should use a deliberately naive solver so the operational system can be
built first.

The phase-1 solver should:

- ignore version constraints entirely
- always pick the latest available version of each package

This is intentionally temporary. PubGrub will replace this logic later, but the
rest of the package-management pipeline should already be real:

- manifest parsing
- graph construction
- metadata fetches
- lockfile writes
- materialization
- build integration

It should still be a real solver pass in the operational sense:

- it traverses the universes
- it fetches the manifests
- it picks exact versions
- it produces a lockfile

The only intentionally fake part is the selection policy.

### Builder integration

For the builder, resolved external packages are effectively packages with
materialized roots on disk.

The builder should therefore consume `Riot_model.Package.resolved` and emit a
download/materialization action for any non-workspace package whose cache root
is missing.

That means the planner can produce actions such as:

- `DownloadPackage`
- regular build/compile/link actions

The download action is responsible for ensuring the resolved package exists at
its expected cache path before normal build actions consume it.

This means a build can repair a missing cache entry lazily without forcing the
planner to rediscover dependency resolution from scratch.

### Events

Package management should emit explicit events so solving, locking, and
materialization are visible in both CLI and server flows.

These events should be part of the shared `riot-model` event surface so both
CLI and server/session flows can report them consistently.

Phase 1 should add explicit package-management event kinds such as:

#### Lockfile events

- `LockfileReadStarted` of `{ path: string }`
- `LockfileReadFinished` of `{ path: string; duration_ms: int }`
- `LockfileReadFailed` of `{ path: string; error: string }`
- `LockfileWriteStarted` of `{ path: string }`
- `LockfileWriteFinished` of `{ path: string; duration_ms: int }`
- `LockfileWriteFailed` of `{ path: string; error: string }`

#### Resolution lifecycle events

- `DependencyResolutionStarted` of `{ packages: string list; mode:
    [ `Refresh | `Unlock ] }`
- `DependencyResolutionUsingExistingLock` of `{ path: string }`
- `DependencyResolutionRefreshingLock` of `{ path: string }`
- `DependencyResolutionUnlocking` of `{ path: string option }`
- `DependencyResolutionFinished` of
    `{ duration_ms: int; resolved_packages: int; resolved_edges: int }`
- `DependencyResolutionFailed` of `{ error: string }`

#### Universe-building events

- `DependencyUniverseBuilding` of `{ packages: string list }`
- `DependencyUniverseBuilt` of
    `{ runtime_packages: int; build_packages: int; dev_packages: int; duration_ms: int }`

#### Registry / manifest metadata events

- `PackageMetadataFetchStarted` of `{ package: string }`
- `PackageMetadataFetchFinished` of
    `{ package: string; version: string option; duration_ms: int }`
- `PackageMetadataFetchFailed` of `{ package: string; error: string }`
- `PackageManifestFetchStarted` of `{ package: string; version: string }`
- `PackageManifestFetchFinished` of
    `{ package: string; version: string; duration_ms: int }`
- `PackageManifestFetchFailed` of
    `{ package: string; version: string option; error: string }`

#### Materialization / download events

- `PackageDownloadStarted` of `{ package: string; version: string; path: string }`
- `PackageDownloadFinished` of
    `{ package: string; version: string; path: string; duration_ms: int }`
- `PackageDownloadFailed` of
    `{ package: string; version: string; path: string; error: string }`
- `PackageDownloadSkipped` of `{ package: string; version: string; path: string; reason: string }`
- `PackageCacheHit` of `{ package: string; version: string; path: string }`
- `PackageMaterializationStarted` of `{ package: string; version: string; path: string }`
- `PackageMaterializationFinished` of
    `{ package: string; version: string; path: string; duration_ms: int }`
- `PackageMaterializationFailed` of
    `{ package: string; version: string; path: string; error: string }`

#### Build-integration events

- `PackageResolvedForBuild` of
    `{ package: string; version: string option; path: string; workspace: bool }`
- `PackageDownloadQueued` of `{ package: string; version: string; path: string }`

The CLI should surface these as progress while operations such as `riot add`,
`riot update`, `riot publish`, and stale-lock `riot build` are running.

## Drawbacks
[drawbacks]: #drawbacks

- this model is more explicit than a pure “dependency string only” package
  manager and therefore has more syntax to teach
- package-name identity plus source/path payloads means Riot must validate name
  mismatches carefully
- mandatory lockfiles create more file churn
- solving runtime, build, and dev together is more complex than solving only one
  dependency class

## Rationale and alternatives
[rationale-and-alternatives]: #rationale-and-alternatives

### Why package-name identity instead of locator identity?

Because locator identity makes normal package usage awkward:

```text
riot add github.com/leostera/minttea
```

should not permanently force every later tool to think the package is named by
that locator.

Using package names as identity keeps the graph coherent and lets both registry
and source installs converge on the same package model.

### Why allow `version + source` together?

Because it is useful and precise.

It expresses:

- where to get the package from
- what version the discovered package must satisfy

That is better than forcing users to choose only one axis.

### Why allow `path + version` and `path + source`?

Because local development and publishability are both important.

These forms let package authors develop locally while still expressing a
consumer-visible fallback identity for publication.

### Why solve from the sparse index instead of the search API?

Because search is for discovery.
The sparse index is the canonical fast path for exact named installs and has
the right shape for a solver.

### Why make `riot publish` publish the workspace batch by default?

Because Riot workspaces commonly contain multiple related packages that must be
published in dependency order.
Making batch publish the default removes a lot of tedious operator work and
matches how the repository is actually structured.

## Prior art
[prior-art]: #prior-art

- Cargo
  - Cargo reinforces that package-name identity should be stable even when the
    actual source of a package comes from a registry or a git repository.
  - Cargo's sparse index model shows that exact named-package resolution should
    go through a small per-package metadata document rather than a search API or
    giant central catalog.
  - Cargo's lockfile shape shows the value of recording one resolved node per
    package, with provenance and checksums attached to the selected artifact.
- Hex
  - Hex shows that a lockfile should preserve enough dependency metadata that
    normal operations do not need to go back to the registry just to understand
    the graph.
  - Hex also reinforces the distinction between dependency requirements in the
    manifest and exact locked selections in the lockfile.
  - Hex's treatment of the lockfile as a self-contained resolution artifact is
    especially relevant for `riot update` and reproducible workspace builds.
- Go modules
  - Go demonstrates the ergonomics of source-first package acquisition.
    A user can point at a public source location and expect the tool to figure
    out the package boundary and usable version information.
  - The Riot source-dependency flow borrows that feeling, but keeps it separate
    from package-name claiming and publication.
- Bun and npm
  - Bun and npm reinforce that package installation by name should feel direct
    and fast, with local lockfile state doing most of the reproducibility work.
  - They also show the UX value of commands like `add`, `remove`, and `update`
    directly editing manifests and lockfiles as one coherent action.

Riot intentionally combines:

- Go's source ergonomics
- Cargo's package-name identity, sparse index, and provenance-oriented lockfile
- Hex's lockfile completeness and offline-friendly graph metadata
- Bun/npm's direct command UX around manifest and lockfile mutation

## Unresolved questions
[unresolved-questions]: #unresolved-questions

- What exact on-disk format should `riot.lock` use?
- How much build- and dev-universe detail should be preserved in the lockfile?
- Should `riot update <pkg>` and other more granular update commands be part of
  the first rollout or follow later?
- How should Riot represent builtin/system package compatibility against OCaml
  compiler versions in the solver?

## Future possibilities
[future-possibilities]: #future-possibilities

- add `riot search <query>` as a discovery companion to exact-match
  `riot add <pkg>`
- add yanked, deprecated, or compatibility flags to the sparse index and teach
  the solver how to treat them
- add more source providers beyond GitHub without changing the package-identity
  model
- add partial graph updates and more explicit workspace publish targeting
- add richer publish-time verification such as registry-side builds,
  documentation generation, or compatibility checks
