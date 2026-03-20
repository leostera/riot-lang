# RFD0013 - Tusk Fix Package-Provided Rules

- Feature Name: `tusk_fix_package_rules`
- Start Date: `2026-03-20`
- RFD PR: [leostera/riot#0000](https://github.com/leostera/riot/pull/0000)
- Riot Issue: [leostera/riot#0000](https://github.com/leostera/riot/issues/0000)

## Summary
[summary]: #summary

This RFD proposes extending `tusk-fix` so packages can ship their own lint
rules and fix explanations, with the rules fused at build time into a
workspace-specific synthetic `tusk-fix` runtime.

The important design point is that package rules should not be executed as one
binary per rule or one binary per package at runtime. That model would be too
slow for Riot-sized workspaces.

Instead:

- packages declare fix-provider source files in `tusk.toml`
- `tusk fix` discovers those provider sources from the workspace
- `tusk fix` generates a synthetic registry package that depends on the owning
  packages
- that generated package links all discovered rules into one fused runtime
- the fused runtime parses each file once and runs all enabled rules in-process

That gives Riot the ownership we want:

- `std` can ship `no-stdlib`
- `suri` can ship framework migration rules
- `sqlx` can ship data-access rules
- any package can add rules by joining the workspace

without turning the checked-in `packages/tusk-fix` package into a giant static
central registry.

## Motivation
[motivation]: #motivation

`tusk-fix` already has the local building blocks:

- a parser-backed pipeline
- a rule abstraction
- typed diagnostics
- diagnostic explanations
- a worker/coordinator execution model

What it does not have yet is package ownership.

Right now, rules live in `packages/tusk-fix/src/rules/`. That is fine for
bootstrapping, but it is the wrong long-term boundary for package-specific
knowledge.

`no-stdlib` is the obvious example. It is not really a generic `tusk-fix`
opinion. It is a `std` opinion about how Riot code should interact with the
standard library boundary.

The same will be true elsewhere:

- `suri` should own web-framework migrations and conventions
- `http` should own protocol-surface transitions
- `sqlx` should own query and pool usage rules
- `minttea` should own update/view/command conventions

So the requirement is not just “support more rules”. The requirement is:

`tusk-fix` must let packages own their own rules.

There is also a performance requirement.

Riot already has roughly:

- ~100k lines of OCaml
- ~2300 source files

If package rules were executed as tiny subprocesses, runtime overhead would
explode quickly:

- one provider invocation per file per rule is already too much
- even “fast” subprocesses add startup, scheduling, and I/O overhead
- per-provider parsing would repeat the same parse work unnecessarily

The correct runtime shape is therefore:

- one fused runtime
- one parse pass per file
- many rules executed in-process

That is the central decision in this RFD.

## Guide-level explanation
[guide-level-explanation]: #guide-level-explanation

A package that wants to ship `tusk-fix` rules declares a provider source file in its
manifest.

For example, `std` could eventually declare:

```toml
[[tusk.fix.provider]]
name = "std"
path = "fix/no_stdlib_provider.ml"
rules = ["pkg:no-stdlib"]
```

That source file is not part of the package's normal build. Instead, `tusk fix`
copies it into the generated fused runtime and compiles it there.

From the user side, nothing changes:

```text
tusk fix
tusk fix --check
tusk fix --explain F0001
```

The difference is in how `tusk fix` runs internally.

Instead of directly using only the rules compiled into `packages/tusk-fix`, it
does this:

```mermaid
flowchart TD
  A[tusk fix] --> B[load workspace]
  B --> C[discover package rule sources]
  C --> D[generate fused runtime sources]
  D --> E[build synthetic tusk-fix runtime]
  E --> F[run one in-process fix pipeline]
  F --> G[stream results]
```

The user should think about it this way:

- packages own rule definitions
- `tusk fix` owns orchestration
- build-time fusion gives the user one runtime instead of hundreds of tiny ones

## Reference-level explanation
[reference-level-explanation]: #reference-level-explanation

## 1. Current state

Today:

- `tusk-fix` owns the rule abstraction
- `Pipeline.default_rules` returns built-in rules
- `Fix_config` resolves workspace and package-local enable/disable state
- the runtime executes rules in one process

That current pipeline is good.

What is missing is a way for rules to come from packages other than
`packages/tusk-fix`.

The current discovery shape is:

```mermaid
flowchart TD
  A[Pipeline.default_rules] --> B[built-in factories]
  B --> C[Rule.t list]
  C --> D[Pipeline.run]
```

That is too closed for the next phase.

## 2. Goals

This proposal is trying to achieve all of the following:

1. let any workspace package ship `tusk-fix` rules
2. keep package-specific lint logic near the package that owns it
3. preserve one in-process rule runtime
4. preserve one parse pass per file
5. keep `tusk fix --explain CODE` working for package-owned codes
6. preserve the current workspace/package config override model
7. avoid forcing checked-in `packages/tusk-fix` to depend on arbitrary packages

## 3. Non-goals

This proposal is not trying to do these things yet:

- third-party plugin loading outside the current workspace
- cross-file refactors
- formatter plugins
- dynlink-based OCaml plugins
- solving normal vs dev vs build/tool dependency classes in the same change

For the first implementation, providers are source files compiled only inside
the generated fused runtime. That keeps provider-owned packages out of the
normal `tusk-fix` dependency graph and avoids cycles like `std -> tusk-fix`.

## 4. Manifest shape

Packages expose rule providers through `tusk.toml`.

The proposed manifest shape is:

```toml
[[tusk.fix.provider]]
name = "std"
path = "fix/no_stdlib_provider.ml"
rules = ["pkg:no-stdlib"]
```

Fields:

- `name`: provider name for debugging and reporting
- `path`: source file relative to the owning package root
- `rules`: rule ids served by that provider

Package rule ids are prefixed as `pkg:<rule>`.

This uses provider source files, not one manifest entry per rule, because packages
will often want to ship a family of related rules and share helpers.

## 5. Provider authoring API

Provider sources should implement the `Tusk_fix.Provider.S` signature.

The authoring story should feel like:

```ocaml
open Std

let name = "std"

let rules () =
  [ No_stdlib_provider.make () ]

let diagnostic_codes () =
  No_stdlib_provider.codes
```

Because the source file is compiled only inside the fused runtime:

- the owning package does not need to link `tusk-fix` in its normal build
- providers can still reference `Tusk_fix`, `Syn`, and the owning package's
  public modules from the fused runtime

The important boundary is:

- diagnostics stay typed inside `tusk-fix`
- built-in codes remain strongly typed
- package-provided codes use a typed `PackageProvided` variant carrying
  provider-owned metadata

## 6. Fusion model

`tusk fix` should generate a workspace-specific synthetic package that links:

- `tusk-fix`
- `std`
- `syn`
- every provider-owning package
- a generated library/binary pair for `tusk-fix-fused`

Conceptually:

```mermaid
flowchart TD
  A[workspace packages] --> B[discover provider sources]
  B --> C[generate tusk-fix-fused workspace]
  C --> D[build synthetic tusk-fix-fused package]
  D --> E[run fused binary]
```

The generated fused runtime should:

- embed the provider source files as modules in generated source
- register all providers before CLI execution
- rebuild like any other `tusk` package
- rely on normal `tusk` caching to avoid unnecessary rebuilds

That synthetic runtime is what the CLI should execute.

## 7. Runtime model

Once the fused runtime exists, execution stays simple.

Per file:

- parse once with `syn`
- run all enabled rules in-process
- merge diagnostics and optional fixes
- stream results through the existing coordinator/reporter path

Conceptually:

```mermaid
flowchart TD
  A[file] --> B[syn parse]
  B --> C[red tree]
  C --> D[all enabled built-in and package rules]
  D --> E[diagnostics and fixes]
  E --> F[streamed output]
```

This is the key reason not to use provider subprocesses.

## 8. Config interaction

The current config model is already close to what we need:

- workspace `[tusk.fix].rules`
- package-local `[tusk.fix].rules`
- package-local overrides applying on top of workspace defaults

The only change is that the available rule set now comes from:

- built-in rules
- fused provider rules discovered from the workspace

So the effective-rule algorithm becomes:

1. discover all built-in and package-provided rule ids
2. establish default enablement
3. apply workspace overrides
4. apply package-local overrides for the file’s package
5. run only the resulting enabled rules

## 9. Explain flow

`tusk fix --explain F0001` should search:

1. built-in diagnostic codes
2. package-owned diagnostic codes fused into the runtime
3. if no match exists, return the usual unknown-code error

That means package-provided rules get first-class explanation text with no
special user syntax.

## 10. Why not one provider binary per package

That model is easy to imagine, but it is the wrong runtime shape.

Problems:

- one file can trigger many provider invocations
- the same file gets reparsed repeatedly
- process startup and transport overhead add up quickly
- large workspaces become “call 100 tiny binaries”

At scale, that is not acceptable.

## 11. Why not statically link all rules into checked-in tusk-fix

That creates the wrong ownership pattern.

Problems:

- `packages/tusk-fix` would need to depend on arbitrary packages
- package-specific lint logic would still be centralized
- package authors would need to edit `packages/tusk-fix` to add rules
- the checked-in core package would keep growing as a bottleneck

That defeats the point of package-provided rules.

## 12. Why not dynlink

Dynamic OCaml plugins are the wrong tradeoff here.

Problems:

- native plugin loading is platform-sensitive
- ABI boundaries are fragile
- debugging is worse
- the build/runtime model becomes harder to reason about

Generated fusion is more explicit and more reliable.

## 13. Rollout plan

The rollout should happen in stages.

### Stage 1

- add provider metadata to `tusk-model`
- discover providers from the workspace
- generate the fused workspace and runtime
- build and run one fused `tusk-fix` runtime

### Stage 2

- move `no-stdlib` into a package-owned provider
- most likely `std` first
- keep the built-in copy temporarily only for migration safety

### Stage 3

- remove migrated built-ins from `packages/tusk-fix/src/rules/`
- make package-owned rules the normal extension mechanism

## Drawbacks
[drawbacks]: #drawbacks

- fusion introduces generated workspace build artifacts
- provider discovery changes the `tusk fix` build path
- provider source files are compiled in a synthetic runtime, not in their
  owning package's normal build
- the synthetic runtime must be rebuilt when the provider set changes

These are acceptable costs for getting ownership and runtime shape right.

## Rationale and alternatives
[rationale-and-alternatives]: #rationale-and-alternatives

The chosen design optimizes for:

- package ownership
- one parse pass per file
- one runtime process
- runtime performance
- a stable user model

The strongest alternative is “one provider binary per package”.
That is simpler mechanically, but it is much worse operationally.

The fused design keeps the public story clean:

- packages declare providers
- `tusk fix` discovers them
- `tusk` generates a fused runtime
- the user still runs one command

That is the right foundation.

## Prior art
[prior-art]: #prior-art

Relevant patterns:

- workspace-discovered extension points already exist in `tusk`
- other language toolchains generate synthetic registries at build time
- Riot already uses generated build artifacts when a checked-in package would be
  the wrong boundary

This proposal follows the same general idea:

- package-owned implementation
- generated fusion at build time
- one coherent runtime at execution time

## Unresolved questions
[unresolved-questions]: #unresolved-questions

- How far should provider sources be allowed to reach into their owning package:
  only public modules, or some future friend/internal surface?
- Should provider-owned diagnostic ids follow a recommended naming convention
  beyond uniqueness?
- When built-ins migrate to package providers, should the built-in copies linger
  for one release window or be removed immediately after verification?
