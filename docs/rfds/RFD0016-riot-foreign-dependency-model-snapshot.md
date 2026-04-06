# RFD0016 - Riot Foreign Dependency Model Snapshot

- Feature Name: `riot_foreign_dependency_model_snapshot`
- Start Date: `2026-03-22`
- Status: `implemented`

## Summary
[summary]: #summary

This RFD documents the current foreign dependency model in `riot`. A foreign
dependency is a package-local build hook declared in `riot.toml` under
`[foreign-dependencies.<name>]`. `riot-model` parses that declaration into a
structured `Package.foreign_dependency`, `riot-planner` injects one
`BuildForeignDependency` action node per foreign dependency into the package
action graph, and `riot-executor` runs the declared command directly in the
foreign dependency directory before the rest of the package actions proceed.

The current model is intentionally small. It gives Riot packages a way to build
native artifacts such as static libraries and then link them into OCaml
executables or shared libraries. It does not yet make foreign dependencies a
first-class published package kind, a sandboxed action kind, or an integrated
test/clean lifecycle.

## Motivation
[motivation]: #motivation

Foreign dependency support already exists in the repository, but the behavior is
spread across several packages:

- `riot-model` defines the manifest shape and hashes foreign inputs
- `riot-planner` injects foreign build actions and threads foreign outputs into
  linker inputs
- `riot-executor` runs the foreign command and validates the declared outputs

Some of that behavior is mentioned in broader `riot` snapshots, but there is no
single document that explains what a foreign dependency currently is and is not.
That matters because foreign dependencies sit at an awkward seam:

- they are part of package metadata
- they behave like build actions
- they run outside the normal sandbox model
- they influence linking and package invalidation
- they carry dormant hooks such as `clean_cmd` and `test_cmd` that are not yet
  wired into the main CLI flows

This RFD exists to make the current baseline explicit before Riot evolves it.
That baseline is important for future work such as:

- first-class expect and snapshot testing
- richer foreign build/test integration
- improved cache semantics for native artifacts
- package publishing and registry work

## Guide-level explanation
[guide-level-explanation]: #guide-level-explanation

The current foreign dependency model is easiest to understand as "a package can
ask `riot` to run an external build command before linking the package's OCaml
artifacts."

The in-repo `hello-foreign` example looks like this:

```toml
[foreign-dependencies.hello-rust]
path = "../../native/hello-rust"
build_cmd = ["cargo", "build", "--release"]
outputs = ["../../target/release/libhello_rust.a"]
```

When Riot builds that package today:

1. the package manifest is parsed into a `foreign_dependency` record
2. the foreign dependency directory is scanned for input files
3. those scanned files contribute to the package hash
4. the planner injects a `BuildForeignDependency` action node
5. all normal package action nodes are made to depend on that foreign node
6. the executor runs `cargo build --release` in the foreign dependency's own
   directory
7. the executor checks that the declared outputs now exist
8. later link actions receive the declared outputs as absolute linker inputs

The most important contributor-facing properties are:

- foreign dependencies are package-local
- they are built before the rest of the package actions run
- they run in their own directory, not in the package sandbox
- their declared outputs are treated as linkable native artifacts
- they are not currently a separate test or clean lifecycle

In other words, the current model is a practical bridge from `riot` into an
external native build system, not a full subsystem for foreign package
management.

### What contributors should expect

Today, contributors should think about foreign dependencies as a coarse package
precondition.

If a package declares two foreign dependencies, `riot` does not currently try to
discover which OCaml actions need which native outputs. It simply builds both
foreign dependencies first, then allows the rest of the package action graph to
run.

That is simple and predictable, but intentionally blunt.

### What contributors should not assume

The current implementation does not imply several stronger guarantees:

- foreign dependencies are not independently published or resolved
- foreign inputs are not copied into the `riot` sandbox
- foreign outputs are not modeled as a standalone immutable artifact family
- `clean_cmd` and `test_cmd` are not part of standard `riot clean` or `riot test`
- there is no dedicated foreign dependency test runner or promotion flow

## Reference-level explanation
[reference-level-explanation]: #reference-level-explanation

### 1. Manifest shape

`packages/riot-model/src/package.ml` parses foreign dependencies from either:

- nested tables under `[foreign-dependencies.<name>]`
- dotted keys beginning with `foreign-dependencies.`

Each foreign dependency currently has this model shape:

- `name`
- `path`
- `inputs`
- `build_cmd`
- `clean_cmd`
- `test_cmd`
- `outputs`
- `env`

The required manifest fields are:

- `path : string`
- `build_cmd : string list`
- `outputs : string list`

The optional fields are:

- `clean_cmd : string list option`
- `test_cmd : string list option`
- `env : string -> string table`

`path` is resolved relative to the owning package directory.
`outputs` are stored as declared relative paths, not canonicalized absolute
paths.

### 2. Input discovery

Foreign dependencies currently do not declare `inputs` explicitly in the
manifest. Instead, `riot-model` scans the foreign dependency directory and
builds the `inputs` list automatically.

The scanner currently:

- recurses from the foreign dependency root
- skips hidden files and directories
- skips common build artifact directories such as `target`, `_build`, `build`,
  `dist`, and `node_modules`
- includes source-like files such as `.rs`, `.c`, `.h`, `.cpp`, `.hpp`
- includes common native build config files such as `Cargo.toml`, `Cargo.lock`,
  `build.rs`, `CMakeLists.txt`, and `Makefile`

This means the foreign dependency hash boundary is partly policy embedded in the
scanner rather than explicitly declared by the package author.

### 3. Package hashing

Foreign dependencies participate in the package hash through
`Package.hash`.

For each foreign dependency, the package hash currently includes:

- the dependency name
- the resolved foreign path
- the `build_cmd`
- the relative input paths discovered by the scanner
- the contents of those input files when readable

Notably, the package hash does not currently add:

- `outputs`
- `env`
- `clean_cmd`
- `test_cmd`

So the current invalidation model is "foreign input content plus build command"
more than "entire foreign dependency declaration."

### 4. Planner behavior

`packages/riot-planner/src/package_planner.ml` injects foreign dependency build
actions after module planning has already produced the package action graph.

For each `Package.foreign_dependency`, the planner creates one
`Action.BuildForeignDependency` node carrying:

- `name`
- `path`
- `build_cmd`
- `outputs`
- `env`

Those action nodes currently:

- have no declared `srcs`
- have no dependency edges between each other
- are added directly to the package action graph

After creating the foreign nodes, the planner makes every existing non-foreign
action node depend on every foreign node in the package.

So the dependency policy today is coarse:

- foreign builds run before normal package actions
- there is no finer-grained dependency modeling between individual OCaml actions
  and individual foreign outputs

### 5. Link integration

`packages/riot-planner/src/action_graph.ml` threads foreign outputs into link
actions by turning each declared foreign output into an absolute path:

`Path.normalize (Path.join fdep.path output)`

Those absolute paths are then supplied as `cclibs` for executable and
shared-library link actions.

This is the main user-visible effect of foreign dependencies today: they are a
way to produce native library files and then feed them into Riot link actions.

### 6. Executor behavior

`packages/riot-executor/src/action_executor.ml` treats
`BuildForeignDependency` differently from normal sandboxed actions.

For a foreign build action:

1. `build_cmd` is split into a program plus arguments
2. the command runs with `cwd` set to the normalized foreign dependency path
3. the provided `env` pairs are passed to the subprocess
4. on success, the executor checks that each declared output exists at
   `Path.normalize (Path.join path output)`

This action does not use the package sandbox as its working directory.
It executes directly against the foreign dependency checkout.

### 7. Sandbox interaction

Normal package inputs are copied into a fresh sandbox directory.
Foreign dependency inputs are not.

The planner explicitly creates foreign action nodes with `srcs = []`, and the
executor skips sandbox output verification for any node whose actions are only
`BuildForeignDependency`.

So foreign builds currently sit outside the main sandbox model:

- normal OCaml sources are copied into sandbox
- dependency `.o` files are copied into sandbox
- foreign build commands run in place in their own directory
- foreign outputs are checked in place, not in sandbox

### 8. Store and artifact behavior

The package build remains package-cache-oriented.
After successful execution, `Package_builder.build` still tries to save the
package outputs into `riot-store`.

That package-store path is sandbox-centric. Foreign dependency outputs are not
currently modeled as an independently cached artifact class with their own
promotion rules.

The practical consequence is that foreign dependencies are primarily used as
"build this native thing before linking" rather than as fully managed immutable
artifacts owned by `riot-store`.

### 9. Dormant lifecycle hooks

`clean_cmd` and `test_cmd` are real fields in `Package.foreign_dependency`, and
they are parsed from the manifest.

But in the current implementation:

- `build_cmd` is used
- `clean_cmd` is not wired into `riot clean`
- `test_cmd` is not wired into `riot test`

So the current model already reserves space for a broader lifecycle, but only
the build hook is live.

### 10. JSON surface

`Package.to_json` and `Package.from_json` do not currently round-trip foreign
dependencies. The current JSON shape drops `foreign_dependencies` entirely and
reconstructs packages with `foreign_dependencies = []`.

That is an implementation detail of the current model, but it matters because it
shows that foreign dependencies are fully supported in manifest parsing and
direct planning, while secondary serialization surfaces have not yet caught up.

## Drawbacks
[drawbacks]: #drawbacks

The current model is useful, but it has clear limitations:

- dependency edges are coarse and package-wide
- foreign builds execute outside the normal sandbox
- input discovery is implicit scanner policy, not explicit manifest data
- only `build_cmd` participates in the live CLI lifecycle
- `env`, `outputs`, `clean_cmd`, and `test_cmd` are not fully reflected in the
  package hash boundary
- package JSON serialization drops foreign dependencies
- there do not appear to be dedicated foreign dependency tests in the current
  `riot` test suites
- foreign outputs are treated more like in-place build prerequisites than like a
  first-class immutable artifact family

None of those are arguments against the feature existing. They are simply the
current baseline.

## Rationale and alternatives
[rationale-and-alternatives]: #rationale-and-alternatives

The current design appears to favor a pragmatic integration point over a heavy
foreign build subsystem.

That choice makes sense for the current Riot repository:

- it keeps manifest syntax small
- it avoids inventing a separate external toolchain framework up front
- it gives packages like `hello-foreign` a direct path to native linkage
- it keeps planner and executor changes localized

But there are obvious alternative designs Riot could pursue later:

- explicit declared foreign inputs instead of directory scanning
- sandboxed foreign builds with copied inputs and promoted outputs
- fine-grained action dependencies instead of "all package actions depend on all
  foreign nodes"
- dedicated foreign artifact caching in `riot-store`
- integrated `clean_cmd` and `test_cmd` support
- workspace-level or registry-level foreign package resolution

This RFD does not choose among those alternatives. It records the current
design they would extend or replace.

## Prior art
[prior-art]: #prior-art

The current foreign dependency model resembles a few common build-system shapes:

- Cargo build scripts and native library linkage, where a Rust build can produce
  native outputs consumed later in the build
- Bazel/Buck-style external actions, but without their stronger sandbox and
  artifact modeling
- ad hoc `make`, `cargo`, or `cmake` subprocess hooks embedded in higher-level
  language build systems

Riot currently chooses the simpler end of that spectrum. It models foreign
dependencies explicitly in package metadata and the action graph, but it does
not yet elevate them into a full external dependency subsystem.

## Unresolved questions
[unresolved-questions]: #unresolved-questions

- Should foreign dependency inputs remain auto-discovered, or become explicitly
  declared in manifests?
- Should `env`, `outputs`, `clean_cmd`, and `test_cmd` affect package hashing?
- Should foreign build outputs become first-class cached artifacts in
  `riot-store`?
- Should foreign dependency nodes stay coarse package prerequisites, or become
  more targeted graph dependencies?
- Should `clean_cmd` and `test_cmd` become part of the normal top-level CLI
  flows?
- Should package JSON serialization grow a full foreign dependency round-trip?

## Future possibilities
[future-possibilities]: #future-possibilities

The current model leaves several natural follow-on directions open.

### 1. Integrated foreign test hooks

The parsed-but-unused `test_cmd` field is an obvious extension point for future
test-system work.

### 2. Better artifact ownership

Foreign outputs could move from "verified in place" to "declared, promoted, and
cached as first-class artifacts."

### 3. Explicit input models

If Riot wants stronger reproducibility guarantees, the manifest could stop
inferring foreign inputs from directory scans and make them explicit.

### 4. Richer registry and publishing work

If Riot later introduces a service or registry layer, the current foreign model
could become one of the stepping stones toward packaged external native
artifacts.
