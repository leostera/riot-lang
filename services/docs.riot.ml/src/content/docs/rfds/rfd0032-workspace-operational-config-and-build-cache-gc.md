---
title: "RFD0032 - Workspace Operational Config and Build Cache GC"
description: "Riot Request for Discussion · presented"
---

> Canonical source: `docs/rfds/RFD0032-workspace-operational-config-and-build-cache-gc.md`

> Status: **Presented**

- Feature Name: `riot_workspace_operational_config`
- Start Date: `2026-04-03`
- Status: `presented`
- RFD PR: [leostera/riot#0000](https://github.com/leostera/riot/pull/0000)
- Riot Issue: [leostera/riot#0000](https://github.com/leostera/riot/issues/0000)

## Summary
[summary]: #summary

This RFD introduces a workspace-local Riot operational config file:

```text
.riot/config.toml
```

Its purpose is to configure how Riot behaves around a project, without turning
`riot.toml` into a catch-all tool-behavior file and without overloading the
machine-local `~/.riot/config.toml`.

The first concrete feature owned by this config is build cache garbage
collection for `_build`.

The proposed default behavior is:

- enabled by default
- generational, not LRU
- configured per repository
- with good built-in defaults even when `.riot/config.toml` is absent

The initial default policy is:

```toml
[cache]
keep_generations = 10
max_size = "50 GiB"
```

This RFD does **not** propose putting registry credentials or user-machine
settings into `.riot/config.toml`. Those remain in `~/.riot/config.toml`.

## Motivation
[motivation]: #motivation

Riot's build cache currently lives under the workspace build root, typically:

```text
_build/<profile>/<target>/cache/...
```

That cache is valuable:

- it makes warm local builds fast
- it makes warm CI builds possible if cache state is restored between jobs

But it also grows over time because the content-addressed store is effectively
append-only. Every changed source/config/toolchain input produces new hash
entries, while older ones remain until the entire build tree is deleted.

This creates an awkward tradeoff today:

- if CI does not cache `_build`, many builds are effectively cold
- if CI caches `_build` wholesale, it will grow without bound

There is also a configuration-boundary problem.

`riot.toml` should primarily describe the project itself:

- packages
- dependency intent
- build profiles
- compiler flags
- publish metadata

It should not become a growing bucket for every piece of Riot tool behavior.

At the same time, `~/.riot/config.toml` already has a clear role:

- user- and machine-local state
- registries
- API tokens
- local machine defaults

Cache retention for a repo is neither of those things. It is not part of the
published project model, and it is not a personal machine secret. It is an
operational choice for how Riot should behave in one repository.

That is why this RFD proposes a third boundary:

- `riot.toml`: project semantics
- `.riot/config.toml`: workspace-local Riot behavior
- `~/.riot/config.toml`: user/machine config

## Guide-level explanation
[guide-level-explanation]: #guide-level-explanation

### Mental model

Contributors should think of `.riot/config.toml` as the place for Riot
operational policy that is shared by the repository but does not affect the
meaning of the package graph itself.

Examples of the right kind of setting:

- build cache retention
- formatter or fixer tool behavior
- other repository-wide Riot tool defaults

Examples of the wrong kind of setting:

- package dependencies
- publish metadata
- compiler/link flags that change produced artifacts
- registry API tokens

### Cache GC behavior

Riot should keep warm builds fast without requiring developers or CI to delete
`_build` manually all the time.

With this RFD, Riot gets a built-in cache retention policy:

```toml
[cache]
keep_generations = 10
max_size = "50 GiB"
```

If `.riot/config.toml` is missing, Riot still uses those defaults.

If the repo wants different retention, it can commit:

```toml
[cache]
keep_generations = 5
max_size = "20 GiB"
```

### Why generational instead of LRU?

Because LRU is the wrong model for Riot's cache shape.

In a content-addressed build cache:

- recent churn is often noisy and low-value
- older stable artifacts are often the most reusable

An LRU policy tends to retain fresh branch churn and evict the stable cache
that was actually helping repeated builds.

A generational policy is simpler and more honest:

- keep the last `N` successful build generations
- delete cache entries no longer referenced by those generations
- if the cache still exceeds `max_size`, drop the oldest retained generations
  until it fits

This keeps the cache useful without requiring hot-path timestamp updates or
cache-hit bookkeeping.

### Scope of the first rollout

This RFD only requires `.riot/config.toml` for cache GC.

It intentionally leaves formatter/fixer settings as a future follow-up, but the
same config boundary is expected to be a better home for repo-local
development-time Riot tooling behavior than `riot.toml`.

## Reference-level explanation
[reference-level-explanation]: #reference-level-explanation

### Config split

Riot should recognize three config layers:

1. `riot.toml`
2. `.riot/config.toml`
3. `~/.riot/config.toml`

Their intended ownership is:

#### `riot.toml`

Project/package semantics:

- dependency intent
- package/workspace metadata
- build profiles
- artifact-affecting compiler settings

#### `.riot/config.toml`

Workspace-local operational behavior:

- cache GC
- future formatter/fixer repo policy
- future Riot command defaults that should travel with the repo

#### `~/.riot/config.toml`

User- and machine-local state:

- registry definitions
- API tokens
- machine-local overrides

There should be no ambiguity between `.riot/config.toml` and `~/.riot/config.toml`:

- the former is repository state
- the latter is user state

### Cache GC model

The first feature defined by this RFD is GC for the hash-addressed build cache
under a build lane:

```text
_build/<profile>/<target>/cache/...
```

The important distinction is:

- `sandbox/` is already temporary and cleaned
- `out/` is promoted convenience output, not the primary historical cache
- `cache/` is the persistent build artifact store and the thing that grows over
  time

GC should therefore operate primarily on the lane-local `cache/` tree.

### Generations

Each successful build should write a small lane-local receipt describing the
cache entries reachable from that build.

For example:

```text
_build/<profile>/<target>/cache-generations/<timestamp>.json
```

Each generation receipt should record the hashes required to reproduce the warm
state for that build, including:

- package artifact hashes
- action artifact hashes
- other lane-local cache roots that must be retained for that build to remain
  warm

GC then works in two phases:

1. keep the newest `keep_generations` receipts
2. compute the live set as the union of hashes referenced by those receipts
3. delete lane-local cache entries not in the live set
4. if the remaining cache still exceeds `max_size`, drop the oldest retained
   generations and repeat

This gives Riot:

- deterministic retention
- no per-hit mutation of cache metadata
- no LRU timestamp churn in the hot path
- stable CI behavior when the build cache is restored between runs

### Default policy

The built-in defaults should be:

```toml
[cache]
keep_generations = 10
max_size = "50 GiB"
```

This policy should be active even if `.riot/config.toml` does not exist.

The reason is practical:

- Riot should manage its build cache reasonably out of the box
- requiring an explicit opt-in means most repos will never benefit
- keeping `_build` bounded is part of good default operational behavior

### Deliberate non-goals

This RFD deliberately does **not** include:

- LRU policy selection in config
- cache-hit timestamps or other hot-path mutation
- user-global cache GC policy in `~/.riot/config.toml`
- migration of `riot.fmt` / `riot.fix` settings as part of the first cache-GC
  rollout

If generational GC is the chosen model, Riot should just implement that model
instead of exposing a `gc = "lru" | "generational"` selector immediately.

## Drawbacks
[drawbacks]: #drawbacks

- Riot gains another config file, which increases conceptual surface area
- generation receipts add some additional bookkeeping
- the chosen defaults may not fit every repository perfectly
- cache GC introduces more lifecycle behavior around successful builds

There is also a small cost to teaching the config split:

- `riot.toml` for project semantics
- `.riot/config.toml` for repo-local Riot behavior
- `~/.riot/config.toml` for user/machine config

That split is intentional, but it is still one more thing to explain.

## Rationale and alternatives
[rationale-and-alternatives]: #rationale-and-alternatives

### Why not keep using `riot.toml` for this?

Because cache retention is not part of the project model.

It should not affect publication, dependency identity, or artifact semantics.
It is operational behavior of Riot around the repo.

Keeping it out of `riot.toml` helps preserve the meaning of that file.

### Why not put this in `~/.riot/config.toml`?

Because cache retention is often repository policy, especially on CI.

Different repositories should be free to carry different retention behavior
without depending on one developer's local machine config.

### Why not disable GC by default?

Because the whole point is to keep `_build` from becoming an unbounded cache
while preserving warm builds.

If Riot knows a reasonable default policy, it should use it.

### Why not use LRU?

Because LRU optimizes the wrong thing for this cache.

Riot's content-addressed build cache benefits more from retaining a bounded set
of recent successful build generations than from retaining "whatever was touched
most recently."

### Why not cache all of `_build` and leave cleanup to CI?

Because `_build` contains both:

- valuable persistent cache state
- disposable per-build state

Riot should understand that distinction itself instead of requiring every CI
system to discover it by accident.

## Prior art
[prior-art]: #prior-art

- Generational garbage collectors
  - The core idea maps well here: recent successful build generations are
    better retention units than individual per-hit timestamps.
- Build systems with separate local caches and disposable workdirs
  - Many systems distinguish reusable artifact stores from temporary execution
    directories. Riot already trends this way with `cache/`, `out/`, and
    `sandbox/`.
- Cargo / tool-local config patterns
  - Cargo distinguishes project manifests from local operational config. Riot
    can benefit from a similar separation even though the exact file layout will
    differ.

## Unresolved questions
[unresolved-questions]: #unresolved-questions

- Should generation receipts live under `cache/` itself or in a sibling
  directory such as `cache-generations/`?
- Should GC run automatically after every successful build, or only when a size
  threshold is exceeded?
- Should Riot also expose a manual `riot cache gc` command for CI and operator
  use, even if automatic GC exists?
- When formatter/fixer settings move out of `riot.toml`, should they move all
  at once or feature-by-feature?

## Future possibilities
[future-possibilities]: #future-possibilities

- move repo-local `riot fmt` and `riot fix` behavior into `.riot/config.toml`
- add explicit `riot cache gc` and `riot cache stats` commands
- teach CI docs to cache only the valuable persistent cache portions instead of
  `_build` wholesale
- eventually separate persistent build cache and disposable build state more
  explicitly on disk if that simplifies retention behavior further
