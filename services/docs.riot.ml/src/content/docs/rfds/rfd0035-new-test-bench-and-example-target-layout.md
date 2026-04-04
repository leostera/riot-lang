---
title: "RFD0035 - New Test, Bench, and Example Target Layout"
description: "Riot Request for Discussion · presented"
---

> Canonical source: `docs/rfds/RFD0035-new-test-bench-and-example-target-layout.md`

> Status: **Presented**

- Feature Name: `new_target_layout`
- Start Date: `2026-04-03`
- Status: `presented`
- RFD PR: [leostera/riot#0000](https://github.com/leostera/riot/pull/0000)
- Riot Issue: [leostera/riot#0000](https://github.com/leostera/riot/issues/0000)

## Summary
[summary]: #summary

This RFD proposes changing Riot's autodiscovery rules for runnable targets
under `tests/`, `bench/`, and `examples/` to follow a new directory-based
target layout.

The new rule is:

- top-level `tests/*.ml` files are test suites
- top-level `bench/*.ml` files are benchmark suites
- top-level `examples/*.ml` files are binaries
- `tests/<name>/main.ml` is also a test suite named `<name>`
- `bench/<name>/main.ml` is also a benchmark suite named `<name>`
- `examples/<name>/main.ml` is also a binary named `<name>`
- all other nested `.ml` files under those trees are support modules, not
  runnable targets

In other words, Riot should stop inferring runnable status from suffixes like
`_tests.ml` and `_bench.ml`, and instead infer it from directory position.

This proposal keeps source scanning recursive so support modules remain
available to the planner. The change is only about which files become runnable
targets by default.

## Motivation
[motivation]: #motivation

Riot currently treats `examples/` more ergonomically than `tests/` and
`bench/`.

Today:

- `examples/*.ml` are autodiscovered as binaries
- `tests/*.ml` are only autodiscovered when the filename ends in `_tests.ml` or
  `-tests.ml`
- `bench/*.ml` are only autodiscovered when the filename ends in `_bench.ml`

That asymmetry creates unnecessary naming ceremony:

- `tests/parser_tests.ml`
- `bench/warm_build_bench.ml`

instead of simply:

- `tests/parser.ml`
- `bench/warm_build.ml`

It also makes multi-file tests and benchmarks awkward. Riot already scans those
trees recursively, so nested support modules exist, but there is no first-class
"directory target" shape equivalent to Cargo's `tests/foo/main.rs` or
`examples/bar/main.rs`.

The proposed model is better for three reasons:

1. Directory position is a clearer signal than filename suffix.
2. It gives Riot a uniform story across `tests/`, `bench/`, and `examples/`.
3. It supports both single-file and multi-file targets without forcing helper
   code into a special support naming convention.

## Guide-level explanation
[guide-level-explanation]: #guide-level-explanation

### Mental model

Contributors should think of these directories like this:

- `tests/` contains test targets and their support modules
- `bench/` contains benchmark targets and their support modules
- `examples/` contains example binaries and their support modules

There are two runnable target shapes:

1. Single-file target
2. Multi-file directory target

### Single-file targets

These become runnable automatically:

```text
tests/parser.ml
bench/warm_build.ml
examples/hello_world.ml
```

Their target names are the basename without `.ml`:

- `tests/parser.ml` -> `parser`
- `bench/warm_build.ml` -> `warm_build`
- `examples/hello_world.ml` -> `hello_world`

### Multi-file directory targets

These also become runnable automatically:

```text
tests/parser/main.ml
bench/warm_build/main.ml
examples/http_client/main.ml
```

Their target names are the directory name:

- `tests/parser/main.ml` -> `parser`
- `bench/warm_build/main.ml` -> `warm_build`
- `examples/http_client/main.ml` -> `http_client`

Sibling and nested files under that directory are support modules for that
target:

```text
tests/parser/main.ml
tests/parser/helpers.ml
tests/parser/fixtures/tokens.ml
```

Only `main.ml` is the runnable entrypoint. `helpers.ml` and `tokens.ml` are not
separate suites.

### Support modules

Nested files that are not `main.ml` are support code:

```text
tests/support/assertions.ml
tests/http/helpers/request.ml
bench/shared/data.ml
examples/http_client/codec.ml
```

These files remain part of the source tree for planning and compilation, but
they are not directly runnable targets.

### Important consequence

Top-level files remain special.

This means:

```text
tests/support.ml
```

is a test suite named `support`, not a helper file.

If a contributor wants shared helper code, it must live under a subdirectory:

```text
tests/support/assertions.ml
```

This mirrors Cargo's distinction between top-level files and nested support
modules.

## Reference-level explanation
[reference-level-explanation]: #reference-level-explanation

## 1. Discovery rules

Riot should classify autodiscovered targets by relative `Path.t`, not by target
name suffix.

For each source bucket:

### `tests/`

Accepted test suite entrypoints:

- `tests/<name>.ml`
- `tests/<name>/main.ml`

Ignored as entrypoints, but kept as sources:

- `tests/<name>.mli`
- `tests/<name>/<other>.ml`
- `tests/<name>/<nested>/<other>.ml`

### `bench/`

Accepted benchmark suite entrypoints:

- `bench/<name>.ml`
- `bench/<name>/main.ml`

Ignored as entrypoints, but kept as sources:

- `bench/<name>.mli`
- `bench/<name>/<other>.ml`
- `bench/<name>/<nested>/<other>.ml`

### `examples/`

Accepted binary entrypoints:

- `examples/<name>.ml`
- `examples/<name>/main.ml`

Ignored as entrypoints, but kept as sources:

- `examples/<name>.mli`
- `examples/<name>/<other>.ml`
- `examples/<name>/<nested>/<other>.ml`

## 2. Target naming

Name derivation is path-based:

- `<dir>/<name>.ml` -> `<name>`
- `<dir>/<name>/main.ml` -> `<name>`

Examples:

```text
tests/parser.ml            -> parser
tests/parser/main.ml       -> parser
bench/warm_build.ml        -> warm_build
examples/http_client/main.ml -> http_client
```

This immediately creates a possible collision:

```text
tests/parser.ml
tests/parser/main.ml
```

Both would try to define the target `parser`.

Riot should treat duplicate autodiscovered target names within one package as a
discovery-time error. The same should apply when an explicit manifest-declared
binary collides with an autodiscovered target name.

In particular, these should fail during target discovery:

```text
tests/foo.ml
tests/foo/main.ml
```

and likewise:

```text
bench/foo.ml
bench/foo/main.ml
```

```text
examples/foo.ml
examples/foo/main.ml
```

The goal is to reject ambiguous layouts before they become `Package.binaries`,
not to let them survive until build selection time.

## 3. Source scanning stays recursive

This RFD does **not** propose changing `Package.sources`.

Riot should keep recursively scanning:

- `tests/`
- `bench/`
- `examples/`

so support modules continue to participate in module planning.

The change is only in autodiscovery:

- which paths become entries in `Package.binaries`
- which entries are considered tests or benchmarks by the build/runtime layers

## 4. Centralize target-role classification in `riot-model`

Today, multiple packages infer test and benchmark behavior from name suffixes:

- `riot-model` autodiscovery
- `riot-build` suite collection
- `riot-cli` completions

That should be replaced by shared path-based helpers in `riot-model`, for
example:

```ocaml
type binary_role =
  | Normal
  | Test
  | Bench
  | Example

val binary_role: binary -> binary_role
val is_test_binary: binary -> bool
val is_bench_binary: binary -> bool
val is_example_binary: binary -> bool
```

The exact helper surface can vary, but the important point is:

- suffix parsing should stop living in `riot-build` and `riot-cli`
- path classification should be shared and deterministic

## 5. Runtime and CLI changes

Once target roles are path-based:

- `riot test` should collect suites by `binary_role = Test`
- `riot bench` should collect suites by `binary_role = Bench`
- `riot completions --tests` should list only `binary_role = Test`
- `riot completions --benchmarks` should list only `binary_role = Bench`
- normal binary listings should exclude tests and benchmarks, but may include
  examples if Riot continues to treat examples as runnable binaries

No user-facing command syntax needs to change. The visible effect is simply
that these files start working without suffixes:

```text
tests/parser.ml
bench/warm_build.ml
```

## 6. Compatibility and migration

### Existing suffix-based files

Files like these continue to work:

```text
tests/parser_tests.ml
bench/warm_build_bench.ml
```

They are still top-level `.ml` entrypoints, so they remain autodiscovered.
Riot just stops *requiring* those suffixes.

### Existing top-level non-suite files under `tests/`

This is the main compatibility cost.

Packages that currently store fixture inputs or helper modules directly under
`tests/` will need to move those files under a nested directory such as:

```text
tests/fixtures/
tests/generated/
tests/diagnostics/
tests/support/
tests/cases/
```

For example, a file like:

```text
tests/0002_nostdlib_module_path.ml
```

would become a suite under the new rules, so it should move to something like:

```text
tests/fixtures/0002_nostdlib_module_path.ml
```

This is an intentional tradeoff. The proposal chooses a simple, predictable
layout rule over hidden heuristics for "probably a helper file".

## 7. Implementation sketch

### Step 1: change autodiscovery in `riot-model`

Update `Package.autodiscover_test_binaries`,
`Package.autodiscover_bench_binaries`, and
`Package.autodiscover_example_binaries` so they:

- accept top-level `*.ml`
- accept nested `*/main.ml`
- ignore other nested `.ml` files as entrypoints

### Step 2: add shared path-based role helpers

Add binary classification helpers to `riot-model` and use them everywhere
target kind matters.

### Step 3: update `riot-build`

Replace suffix checks in:

- `packages/riot-build/src/test_runtime.ml`
- `packages/riot-build/src/bench_runtime.ml`

with the shared path-based helpers.

### Step 4: update `riot-cli`

Replace suffix checks in:

- `packages/riot-cli/src/shell_completions/shell_completions.ml`

with the shared path-based helpers.

### Step 5: migrate top-level non-suite fixture inputs

Move any fixture/support files currently living directly under `tests/` into
nested directories before flipping the rule globally.

## Drawbacks
[drawbacks]: #drawbacks

The biggest drawback is migration churn for packages that currently keep
top-level test fixtures in `tests/`.

This proposal also makes top-level helper files impossible by convention:

```text
tests/support.ml
```

becomes a suite. Contributors must instead use:

```text
tests/support/assertions.ml
```

That is slightly stricter than Riot's current layout, but it keeps the rule
simple and aligns with Cargo's target model.

## Rationale and alternatives
[rationale-and-alternatives]: #rationale-and-alternatives

## Keep suffix-based discovery

Rejected because it preserves unnecessary naming ceremony and still does not
provide a good multi-file target story.

## Make every recursive `.ml` file under `tests/` and `bench/` runnable

Rejected because it leaves no obvious place for support modules. Contributors
would be forced into special-case ignored directories just to write helpers.

## Support only top-level files, not `dir/main.ml`

Rejected because it makes multi-file tests, benchmarks, and examples clumsy.
`dir/main.ml` is the natural structured target shape and mirrors Cargo.

## Add explicit `[test]`, `[bench]`, and `[example]` manifest sections instead

Rejected for now because the point of this change is to make the conventional
layout work without extra manifest boilerplate.

## Unresolved questions
[unresolved-questions]: #unresolved-questions

1. Should Riot surface duplicate target-name collisions as package load errors
   during autodiscovery itself, or during `Package.from_toml` validation after
   autodiscovery has returned candidate targets?
2. Should `riot init` eventually scaffold this layout explicitly for examples,
   tests, and benches?
3. Should Riot eventually add explicit manifest sections for tests/benches as a
   strictly opt-in advanced override, or is conventional layout sufficient?
