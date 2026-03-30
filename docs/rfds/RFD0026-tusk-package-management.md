# RFD0026 - Tusk Package Management

- Feature Name: `tusk_package_management`
- Start Date: `2026-03-30`
- Status: `presented`
- RFD PR: [leostera/riot#0000](https://github.com/leostera/riot/pull/0000)
- Riot Issue: [leostera/riot#0000](https://github.com/leostera/riot/issues/0000)

## Summary
[summary]: #summary

This RFD defines Tusk's package-management model on top of Riot's existing
registry, sparse index, and search infrastructure.

It introduces:

- a package-name-first dependency model
- manifest syntax for registry, source, path, and builtin dependencies
- a mandatory `tusk.lock` file that stores the resolved graph
- a PubGrub-based solver contract for runtime, build, and dev universes
- the user-facing flows for `tusk add`, `tusk rm`, `tusk update`, and
  `tusk publish`

The registry and sparse index are treated as existing infrastructure.
This RFD defines how `tusk` should consume them.

## Motivation
[motivation]: #motivation

Riot now has the backend pieces for packages:

- explicit publication through `api.pkgs.ml`
- a sparse named-package index under `cdn.pkgs.ml/index/v1/...`
- search for package discovery
- immutable package provenance through canonical source locators and SHAs

What is still missing is the client-side model that makes those pieces usable
from Tusk.

That model must solve several concrete problems at once:

1. users should be able to add named packages quickly:
   - `tusk add std`
   - `tusk add std@0.0.1`
2. users should be able to add packages directly from source:
   - `tusk add github.com/owner/repo`
   - `tusk add github.com/owner/repo/path/to/pkg`
   - `tusk add https://github.com/owner/repo#main`
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
- `tusk.toml` stores user intent
- `tusk.lock` stores the exact resolved graph

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

The authoritative package name comes from the target package's `tusk.toml`.

### Dependency kinds

Tusk recognizes four dependency kinds:

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

Tusk resolves this through the sparse index.

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

Tusk keeps dependency scopes separate:

- `[dependencies]` for runtime
- `[build-dependencies]` for build-time
- `[dev-dependencies]` for development and test-only use

`tusk add` chooses the target section based on flags:

- default: `[dependencies]`
- `--build`: `[build-dependencies]`
- `--dev`: `[dev-dependencies]`

It also chooses the target manifest based on flags:

- default: current package manifest
- `--workspace`: workspace manifest
- `-p <pkg>` / `--package <pkg>`: that package manifest only

### What `tusk add` does

#### Named add

```text
tusk add std
tusk add std@0.0.1
```

This is exact package-name resolution, not a search query.

The rough flow is:

1. resolve the workspace and target manifest
2. fetch sparse index metadata for the named package
3. solve the graph with PubGrub
4. update `tusk.toml`
5. write the exact resolved graph to `tusk.lock`

If the package does not exist, Tusk should fail clearly and may optionally show
close matches from package search.

#### Source add

```text
tusk add github.com/leostera/minttea
tusk add github.com/owner/repo/path/to/pkg
tusk add https://github.com/owner/repo#main
```

The flow is:

1. normalize the source locator
2. materialize it through the registry
3. inspect the fetched package manifest
4. discover the real package name and declared version
5. write the dependency entry under the real package name
6. solve the graph
7. write `tusk.toml`
8. write `tusk.lock`

If the package discovered from source is actually named `awesome-utils`, then:

```text
tusk add github.com/leostera/fartass
```

may write:

```toml
[dependencies]
awesome-utils = { github = "leostera/fartass" }
```

That normalization is intentional.
Package name remains the identity.

`tusk add` should print useful progress while doing this:

- that it is discovering the source package
- which ref it resolved
- which commit SHA it selected
- which package name it discovered
- what it wrote to `tusk.toml`

### What `tusk rm` does

```text
tusk rm std
```

This removes a dependency from the targeted manifest section only, then
re-solves and rewrites `tusk.lock`.

It does not remove from every section automatically.
There is no implicit `--all`.

### What `tusk update` does

```text
tusk update
```

This updates the whole workspace graph while preserving manifest requirements.

It should:

1. keep `tusk.toml` unchanged
2. fetch newer package metadata as needed
3. solve again against the current constraints
4. rewrite `tusk.lock`

`tusk update` updates the graph, not the user requirements.

### What `tusk publish` does

`tusk publish` publishes the workspace batch by default.

It should:

1. enumerate workspace packages
2. skip packages that are `private = true` or `public = false`
3. sort publishable packages in dependency order
4. verify that the current code is already on GitHub
5. run mandatory local verification
6. publish each package through the registry

This means Tusk, not the registry, owns workspace publish serialization.

The registry remains the authority on:

- package-name claims
- immutable versions
- publish auth
- sparse index updates

Tusk owns:

- workspace traversal
- publish ordering
- local verification
- operator UX

## Reference-level explanation
[reference-level-explanation]: #reference-level-explanation

## 1. Manifest model

### 1.1 Canonical package identity

Every dependency key in `tusk.toml` is a package name.

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

### 2.1 `tusk.lock` is mandatory

Every workspace using package management should maintain `tusk.lock`.

This is true for:

- registry dependencies
- source dependencies
- path dependencies
- exact commit refs

### 2.2 Lockfile stores the resolved graph

`tusk.lock` should be a resolved graph, not just a list of selected top-level
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

Tusk should follow that general shape.

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

Tusk should use `packages/pubgrub` as the version solver.

PubGrub should operate on package names and version ranges, not on raw source
locators.

### 3.2 Three universes

Tusk conceptually solves three dependency universes:

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

For registry packages, Tusk should solve from the sparse index.

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
4. materialize the selected graph into `tusk.lock`

Search is not on the solver hot path.

## 4. `tusk add`

### 4.1 Exact package lookup

`tusk add <pkg>` expects an exact package name.

If missing:

- return a direct error
- optionally perform a lightweight search query to suggest near matches

### 4.2 Source discovery

`tusk add <source>` must resolve through the registry immediately.

It should not merely write a textual manifest entry without validation.

That means `tusk add` is both:

- a manifest-editing command
- a resolution command

### 4.3 Automatic manifest edits

`tusk add` edits `tusk.toml` automatically.

If the actual package name differs from the repo name or path, Tusk should use
the actual discovered package name when writing the dependency entry.

## 5. `tusk rm`

`tusk rm <pkg>` removes the dependency from the targeted manifest scope only and
then rewrites `tusk.lock` from a fresh solve.

If the dependency does not exist in the targeted section, Tusk should fail
clearly.

## 6. `tusk update`

`tusk update` performs a workspace-wide re-solve.

It should:

- keep dependency requirements unchanged
- update as much of the graph as the current requirements allow
- rewrite `tusk.lock`

It should not silently widen or rewrite manifest requirements.

## 7. `tusk publish`

### 7.1 Workspace-first publish

`tusk publish` publishes the workspace batch by default.

That means it should:

1. discover workspace packages
2. filter to publishable packages
3. order them by dependency
4. publish them serially

### 7.2 Publish eligibility

A package is publishable only if:

- it is public
- its version is valid
- it passes local verification
- the source commit already exists on GitHub
- its runtime dependencies are publish-valid

The registry remains the final authority, but Tusk should fail locally first
where possible.

### 7.3 Credentials

Tusk should store credentials in:

```text
~/.tusk/config.toml
```

That file should hold the API token used for authenticated publish operations.

## Drawbacks
[drawbacks]: #drawbacks

- this model is more explicit than a pure “dependency string only” package
  manager and therefore has more syntax to teach
- package-name identity plus source/path payloads means Tusk must validate name
  mismatches carefully
- mandatory lockfiles create more file churn
- solving runtime, build, and dev together is more complex than solving only one
  dependency class

## Rationale and alternatives
[rationale-and-alternatives]: #rationale-and-alternatives

### Why package-name identity instead of locator identity?

Because locator identity makes normal package usage awkward:

```text
tusk add github.com/leostera/minttea
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

### Why make `tusk publish` publish the workspace batch by default?

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
    especially relevant for `tusk update` and reproducible workspace builds.
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

- What exact on-disk format should `tusk.lock` use?
- How much build- and dev-universe detail should be preserved in the lockfile?
- Should `tusk update <pkg>` and other more granular update commands be part of
  the first rollout or follow later?
- How should Tusk represent builtin/system package compatibility against OCaml
  compiler versions in the solver?

## Future possibilities
[future-possibilities]: #future-possibilities

- add `tusk search <query>` as a discovery companion to exact-match
  `tusk add <pkg>`
- add yanked, deprecated, or compatibility flags to the sparse index and teach
  the solver how to treat them
- add more source providers beyond GitHub without changing the package-identity
  model
- add partial graph updates and more explicit workspace publish targeting
- add richer publish-time verification such as registry-side builds,
  documentation generation, or compatibility checks
