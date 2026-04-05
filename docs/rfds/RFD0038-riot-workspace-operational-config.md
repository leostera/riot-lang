# RFD0038 - Riot Workspace Operational Config

- Feature Name: `riot_workspace_operational_config`
- Start Date: `2026-04-05`
- Status: `presented`
- RFD PR: [leostera/riot#0000](https://github.com/leostera/riot/pull/0000)
- Riot Issue: [leostera/riot#0000](https://github.com/leostera/riot/issues/0000)

## Summary
[summary]: #summary

This RFD introduces a repository-local Riot operational config file:

```text
.riot/config.toml
```

It is a configuration-boundary RFD, not a cache RFD and not a project-manifest
change. Its job is to give Riot a place for repository-shared operational
behavior that does not belong in `riot.toml` and does not belong in
`~/.riot/config.toml`.

- `riot.toml` remains the project semantics file
- `.riot/config.toml` becomes the repository-local Riot behavior file
- `~/.riot/config.toml` remains the user- and machine-local config file
- the first concrete consumer is cache policy from `RFD0032`
- this rollout does not move registry credentials, package semantics, or other
  artifact-affecting settings into `.riot/config.toml`

## Motivation
[motivation]: #motivation

Riot currently has two obvious configuration homes:

- `riot.toml`
- `~/.riot/config.toml`

But repository-shared operational behavior fits neither one well.

`riot.toml` should primarily describe the project itself:

- package metadata
- dependency intent
- build profiles
- artifact-affecting settings

If Riot keeps putting every operational toggle there, `riot.toml` becomes a
catch-all tool-behavior file instead of a clear project manifest.

`~/.riot/config.toml` has the opposite problem. It is the right place for:

- user tokens
- registry definitions
- machine-local defaults

But it is the wrong place for repository policy that should travel with the
repository, be reviewed in commits, and apply consistently in CI.

This gap becomes obvious as soon as Riot wants repository-level operational
settings such as:

- cache retention policy
- future formatter defaults
- future fixer defaults

Without a third config boundary, Riot ends up paying one of two costs:

1. pollute `riot.toml` with non-semantic tool behavior
2. hide repository behavior in machine-local config that does not travel with
   the repo

This RFD removes that ambiguity by introducing a separate repository-local Riot
config file.

The first concrete use case is `RFD0032`, which needs a place to store cache GC
policy. But the need is broader than cache GC alone. Riot needs a durable
answer to "where does repository-local tool behavior live?"

## Guide-level explanation
[guide-level-explanation]: #guide-level-explanation

Contributors should think about Riot config in three buckets:

1. project semantics
2. repository-local Riot behavior
3. user- and machine-local Riot behavior

Those map to:

```text
riot.toml
.riot/config.toml
~/.riot/config.toml
```

Suppose a repository wants to keep a smaller cache than Riot's defaults.

That should look like:

```toml
# .riot/config.toml
[riot.cache]
keep_generations = 5
max_size = "20 GiB"
```

That file is:

- committed with the repo
- shared by contributors
- visible to CI

Now suppose a developer needs a registry token. That should still live in:

```text
~/.riot/config.toml
```

And if the repository changes its package metadata, dependencies, or build
profiles, that still belongs in:

```text
riot.toml
```

So the contributor mental model becomes:

- if it changes what the project *is*, it belongs in `riot.toml`
- if it changes how Riot should *behave around this repo*, it belongs in
  `.riot/config.toml`
- if it is personal, secret, or machine-local, it belongs in
  `~/.riot/config.toml`

That keeps the config story teachable and avoids making one file carry three
different kinds of meaning.

## Reference-level explanation
[reference-level-explanation]: #reference-level-explanation

### Config layers

Riot should recognize three distinct config layers:

1. `riot.toml`
2. `.riot/config.toml`
3. `~/.riot/config.toml`

Their intended ownership is:

#### `riot.toml`

Project/package semantics:

- dependency intent
- package and workspace metadata
- build profiles
- artifact-affecting compiler settings

#### `.riot/config.toml`

Repository-local Riot operational behavior:

- cache policy
- future repository-shared Riot command defaults
- future formatter and fixer behavior that should travel with the repo

#### `~/.riot/config.toml`

User- and machine-local state:

- registry definitions
- API tokens
- machine-local overrides

### Initial scope

The first consumer of `.riot/config.toml` is the cache policy described in
`RFD0032`.

That means the first rollout only needs enough surface to support repository
operational behavior for cache retention under a `riot.cache` section.

This RFD does not require Riot to move every operational setting at once. It is
acceptable to move feature-by-feature as those settings gain clearer ownership.

### Non-goals

This RFD does **not** move:

- package dependencies into `.riot/config.toml`
- compiler flags that change produced artifacts
- registry credentials or tokens out of `~/.riot/config.toml`
- all future tool settings in one immediate migration

## Drawbacks
[drawbacks]: #drawbacks

- Riot gains another config file, which increases conceptual surface area
- contributors must learn the distinction between project semantics and
  repository-local operational behavior
- some settings may still be ambiguous at first until more of Riot's command
  surface is classified

## Rationale and alternatives
[rationale-and-alternatives]: #rationale-and-alternatives

### Why not keep using `riot.toml`?

Because repository-local operational behavior is not project semantics.

Putting everything in `riot.toml` makes it harder to understand what that file
means and harder to tell which changes affect published or built artifacts.

### Why not keep using `~/.riot/config.toml`?

Because repository policy is not personal machine state.

If cache retention or other operational defaults should travel with the repo
and apply in CI, they cannot live only in the developer's home directory.

### Why not wait until several features need this boundary?

Because cache GC already needs it, and once that need exists Riot should define
the boundary cleanly instead of introducing one-off special cases.

### What if Riot does nothing?

Then Riot will keep paying for an unclear config story:

- `riot.toml` gradually becomes a catch-all
- repository-shared operational behavior has no proper home
- machine-local config gets used for things that should really belong to the
  repository

## Prior art
[prior-art]: #prior-art

- Cargo-style distinctions between project manifests and local tool config
  - The exact files differ, but the boundary lesson is useful.
- Riot's own existing split between `riot.toml` and `~/.riot/config.toml`
  - This RFD extends that split rather than replacing it.
- Repository-local tool config in other developer tools
  - Many tools end up needing a repository-local operational file once project
    semantics and machine-local secrets stop fitting in the same place.

## Unresolved questions
[unresolved-questions]: #unresolved-questions

- Which future Riot settings should migrate next after cache policy?
- Should `.riot/config.toml` eventually support repository-local command
  defaults, or should it stay limited to explicit operational sections?
- How much merging or precedence logic will Riot eventually want between
  built-in defaults, `.riot/config.toml`, and `~/.riot/config.toml` once more
  settings exist?

## Future possibilities
[future-possibilities]: #future-possibilities

- move repository-local `riot fmt` behavior into `.riot/config.toml`
- move repository-local `riot fix` behavior into `.riot/config.toml`
- add more repository-scoped Riot operational defaults as their ownership
  becomes clear
- eventually document the full config model in contributor docs once more than
  one feature uses this boundary
