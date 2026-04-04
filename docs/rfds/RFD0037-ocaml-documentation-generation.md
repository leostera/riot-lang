# RFD0037 - Code-Driven OCaml Documentation Generator

- Feature Name: `ocaml_code_driven_doc_generation`
- Start Date: `2026-04-04`
- Status: `presented`
- RFD PR: [leostera/riot#0000](https://github.com/leostera/riot/pull/0000)
- Riot Issue: [leostera/riot#0000](https://github.com/leostera/riot/issues/0000)

## Summary
[summary]: #summary

This RFD proposes a new `riot doc` command and a `riot-doc` code-generation pipeline that build modern, static documentation sites for OCaml packages from source AST/types, then uploads them to
`docs.pkgs.ml/p/<package>/<version>/` in the shape expected by the docs service.

The proposal mirrors docs.rs assumptions for isolation and reproducibility:

1. `riot build` can assume its dependency artifacts are available.
2. `riot doc` can assume dependency docs are available when linking to other packages.
3. A package at `docs.pkgs.ml/p/<name>/<version>/` has docs for package `<name>@<version>`.
4. `riot doc` must be source-cacheable: unchanged source hashes and dependency hashes should yield cache hits and reuse docs from `Riot_store` without rework.

## Motivation
[motivation]: #motivation

Riot does not currently have a native, first-party API documentation generator. Existing package publication already expects a docs phase (`services/docs.pkgs.ml` stores a `DocsBuildRequest` and currently expects `riot doc`), but there is no concrete implementation path.

This leaves a gap in:

- discoverability: users cannot browse OCaml package docs from the registry web route contract,
- correctness: current docs for external packages are impossible without external tooling assumptions,
- consistency: no shared rendering model that is aligned with Riot’s parser/typechecker and formatter pipeline.

Riot already has strong foundations (`syn`, `typ`, `krasny`, `riot-cli`, `riot-publish`), so this is the missing link between package publication and user-facing package docs.

## Guide-level explanation
[guide-level-explanation]: #guide-level-explanation

Roughly:

- `riot doc` becomes a first-class command.
- It discovers a target package and its public surface.
- It renders a static docs site from parsed docs/signatures and type information.
- It writes HTML/CSS/JS to `target/docs/<package>/` locally.
- CI/registry runners upload that output under
  `docs/<package>/<version>/` so `docs.pkgs.ml` can serve it at
  `/p/<package>/<version>/`.

### Default usage

```sh
riot doc
riot doc --package std
riot doc --open
riot doc --all
riot doc --format html --output ./dist/docs
```

### Contract by assumption

- If package `foo` depends on `bar@0.0.1`, generated docs for `foo` may link to `bar` using:
  `https://docs.pkgs.ml/p/bar/0.0.1/`.
- If that dependency docs payload is not available yet, links can remain as dead links temporarily. This is explicit and acceptable for now.
- If package sources and lockfile hashes are unchanged, docs and cross-package links should be re-used from cache for a zero-work rebuild.
- This enables **isolated package docs generation** and **later stitching** when dependencies become available.

### Why this mirrors docs.rs

`3rdparty/docs.rs` already demonstrates a very useful operational model:

- docs pipeline stages a request with `command: ["riot", "doc"]`.
- docs service serves keys under `docs/<package>/<version>/` and path-resolves `/` or missing suffixes to `index.html`.
- the runner flow is expectation-driven and container-safe.

Riot can adopt this model without waiting for a monolithic external toolchain.

## Reference-level explanation
[reference-level-explanation]: #reference-level-explanation

### 1) CLI contract

`packages/riot-cli` currently exposes `doc` in the command table but has no command module.

`riot doc` should be wired to:

- parse flags
- resolve workspace/package context
- call a new `Riot_doc` API
- return progress on `stderr` and generated artifact paths on `stdout`.

Suggested flags:

- `--package <name>`: select root package
- `--all`: document all publishable packages in workspace
- `--output <dir>`: override output directory (default from target dir)
- `--open`: open index file locally
- `--no-deps`: skip docs-link map generation for dependencies
- `--format html` (initially only HTML is required)
- `--clean`: clear previous docs output before generation
- `--force`: force rebuild and write through cache even if key exists
- `--no-cache`: skip cache read/write for this invocation

### 2) New package split

Add a docs-generation package (for example `packages/riot-doc`) and separate it from CLI parsing:

- `packages/riot-doc/src/doc.ml` (public request API)
- `packages/riot-doc/src/discovery.ml` (package resolution + entrypoints)
- `packages/riot-doc/src/api_graph.ml` (dependency graph + version map)
- `packages/riot-doc/src/ast_surface.ml` (public signature extraction from `syn` + `typ`)
- `packages/riot-doc/src/render.ml` (page templates, markdown rendering, search index)
- `packages/riot-doc/src/assets/` (Tailwind-first styling + modern layout)

Use `syn` as the source of structural truth (including docstrings/comments) and `typ` for stable public API signatures.

### 3) Rendering model and output contract

Use a deterministic output tree under package docs root:

```text
docs-root/
  index.html
  search.json
  modules/
    Mod1.html
    Mod2.html
  types/
    Type.html
  values/
    f.html
  assets/
    app.css
    app.js
```

For package publish staging, command output should be uploaded under
`docs/<package>/<version>/` so route resolution in `services/docs.pkgs.ml` remains unchanged:

- `docs/<name>/<version>/index.html`
- `docs/<name>/<version>/...` for all generated assets

### 4) Dependency link mapping

In each generated page, dependency links should use either:

- public docs URL map:
  `https://docs.pkgs.ml/p/<dep_name>/<dep_version>/`
- local fallback map when docs are generated in same run and locally available.

The link map is derived from resolved dependency metadata (`package` + locked version).

### 5) Registry/docs pipeline integration

- `services/docs.pkgs.ml` already stages a docs request with:
  - `run_kind: "docs"`
  - `command: ["riot", "doc"]`
  - `output_prefix: docs/<package>/<version>/`
- Implement `riot doc` so this stage becomes real:
  - it must read workspace/package context from unpacked publish artifact
  - it must write docs to a directory discoverable by a runner uploader
  - it must fail fast and clearly if package declaration is incomplete.

No docs router behavior changes are needed immediately in `services/docs.pkgs.ml`.

### 6) HTML UI and “modern” look

Riot needs docs that feel contemporary:

- use a distinct color system and type hierarchy,
- responsive layout with sidebar navigation,
- fast search (`search.json`),
- syntax highlighting,
- explicit module/type/value navigation,
- sectioned docs and jump links.

A lightweight tailwind-first component style can be generated into
`packages/riot-doc/src/assets` and bundled into outputs.

### 7) Execution order and failure behavior

A practical first implementation should run in this order:

1. Resolve package graph and public roots.
2. Build/collect signatures via existing planner/type pipeline.
3. Convert to doc model (items, signatures, docs, metadata).
4. Render pages and write assets.
5. Emit `index.html` and `search.json`.
6. Exit nonzero if any package fails critical rendering.

### 8) Future hardening (post-MVP)

- split-module/page streaming render,
- search indexing at package index level,
- richer cross-target docs,
- example rendering and doctest integration,
- diagnostics surfaces (broken symbol references, dead external links).

## Detailed implementation plan
[implementation-plan]: #implementation-plan

### Phase 0 - Scope lock and build contracts

Define and pin behavior before code generation:

1. confirm docs output root and URL contract is exactly `docs/<package>/<version>/...` and that `index.html` is default at package root.
2. define `riot doc` exit semantics: nonzero on any root package hard failure, zero with warnings only if all required packages rendered.
3. define one supported format initially: HTML with optional `--format html` alias.
4. define dependency link policy: docs links may point to `https://docs.pkgs.ml/p/<dep>/<version>/` even when dependency docs are absent; this is not a hard failure in MVP.
5. define cache key material:
   - lane (`profile` + `target`) from `Riot_store.Store.create_for_lane`,
   - package-level input hash from planner context (`Package.hash`, build ctx, toolchain hash),
   - type/signature snapshot hash from docs surface extraction pass,
   - dependency version+doc-key snapshot from lockfile,
   - docs renderer template version.
6. define cache reuse semantics:
   - if cache key exists, promote all docs outputs from `Riot_store` and emit cached artifacts quickly,
   - if any key part changed, re-run extract/render and overwrite cache entry.

### Phase 1 - CLI and service-facing API

1. add `packages/riot-doc` crate/package with a single public entrypoint module `Doc` that accepts a normalized request object.
2. implement `type cli_options`, `type request`, and `type result_summary` in `packages/riot-doc/src/doc.mli`.
3. update `packages/riot-cli` command registry to wire `riot doc` to `Riot_doc`.
4. implement `packages/riot-cli/src/doc.ml` parsing for `--package`, `--all`, `--open`, `--output`, `--format`, `--no-deps`, `--clean`, `--force`, and `--no-cache`.
5. keep command output deterministic by emitting a structured line per package, printing final generated paths on stdout, and printing diagnostics on stderr.
6. emit cache trace metadata (`cache_key`, `hit`/`miss`) in summary output and optionally emit debug details when cache is disabled.

### Phase 2 - Package and dependency graph extraction

1. add `packages/riot-doc/src/discovery.ml` that resolves one package by CLI selection or all publishable workspace packages.
2. add `packages/riot-doc/src/api_graph.ml` to build a direct dependency manifest map from existing lock/dependency metadata.
3. define `type dep_doc_target = { dep_name : string; dep_version : string; url_base : string }`.
4. add `packages/riot-doc/src/cache.ml` for cache key synthesis, dependency snapshotting, lookup, and promotion into local output directories.
5. define failure modes: missing root package fails hard; missing optional dependency entries map to local placeholder but generation continues; unresolved links produce warning diagnostics.
6. return a stable package processing order by topological sort using declared dependency edges.
7. consume dependency manifests from `Riot_store` during graph hydration so docs run can reuse source/type outputs already produced by `riot build`.

### Phase 3 - Public surface extraction

1. add `packages/riot-doc/src/ast_surface.ml` for parse-level extraction of modules, values, types, constructors, fields, exceptions, class items, and doc comments.
2. add `packages/riot-doc/src/type_surface.ml` for signature canonicalization using `typ`.
3. merge syntactic and typed views into a single immutable `doc_model` that is stable-by-design: ordered by source order where deterministic, then canonical sort by names, private items omitted unless exported from interfaces, and unresolved refs retained for later warnings.
4. support module/interface asymmetry: source `mli` preferred for public surface, `ml` used when no interface exists.
5. persist extracted model hash and optional model cache artifacts so unchanged source can skip re-parse and re-type.

### Phase 4 - Model-to-page compiler pass

1. add `packages/riot-doc/src/model.ml` with explicit `DocPage`, `DocSection`, and `DocSymbol` nodes.
2. add `packages/riot-doc/src/render.ml` for page-level emit order: package index first, then modules alphabetical, then symbols with stable anchors.
3. generate one deterministic link format with module landing at `/modules/<module>.html` and symbol anchors at `/modules/<module>.html#<symbol>`.
4. add `search.json` index with at least package metadata, symbol name, kind, URL, module, doc snippet, and signature text.
5. when cache key is valid, skip full emit and promote `search.json` + html assets from store.

### Phase 5 - Dependency hyperlink map and import link behavior

1. implement local resolver in `packages/riot-doc/src/linking.ml` that maps any external path to docs base URL from dependency map.
2. generate fallback link text when a dependency docs target is missing.
3. keep link generation deterministic so page-by-page regeneration does not reshuffle URL ordering.
4. add warning class for unresolved links and include in summary output.
5. keep dependency-url mapping in cache metadata so unchanged lock state produces byte-stable cross-package links.

### Phase 6 - Static assets + modern styling

1. add `packages/riot-doc/src/templates/` and `packages/riot-doc/src/assets/`.
2. build layout tokens without introducing a framework runtime; the template should include sidebar + content column, module/type/value hierarchy, search UI driven by `search.json`, and explicit typography + responsive breakpoints.
3. generate at least `assets/app.css`, `assets/search.js`, and `assets/logo.svg` (or equivalent brand mark).
4. keep everything self-contained in generated output with relative asset references.
5. package assets as cache artifacts and re-use them while doc hash is stable.

### Phase 7 - Docs pipeline integration

1. ensure command writes output at path consumed by `services/docs.pkgs.ml` runner (`docs/<package>/<version>/` for staging).
2. preserve existing `DocsBuildRequest` schema and do not require service-side contract changes.
3. update `services/docs.pkgs.ml/src/main.ts` docs service docs to include concrete `riot doc` requirements (artifact name, root file, exit expectations).
4. add an execution contract for runner behavior: command working directory points at unpacked package source, generated output lives under configured `output_root`, and runner uploads only successful `index.html` plus tracked generated assets.

### Phase 8 - Operational polish and release hardening

1. add `--open` behavior with cross-platform opener fallback and clear message when unavailable.
2. add `--clean` behavior with safe deletion rules (only target docs dir, no recursive workspace deletion).
3. add docs cache invalidation knobs (`--force` and `--no-cache`) already in MVP for deterministic operation.
4. add richer broken-link diagnostics (source path, symbol name, and dependency URL).

### Concrete file map for MVP

Proposed implementation touch list:

1. `packages/riot-doc/dune` (+ opam/metadata as required by package conventions),
2. `packages/riot-doc/src/doc.mli`,
3. `packages/riot-doc/src/doc.ml`,
4. `packages/riot-doc/src/discovery.ml`,
5. `packages/riot-doc/src/api_graph.ml`,
6. `packages/riot-doc/src/ast_surface.ml`,
7. `packages/riot-doc/src/type_surface.ml`,
8. `packages/riot-doc/src/model.ml`,
9. `packages/riot-doc/src/linking.ml`,
10. `packages/riot-doc/src/cache.ml`,
11. `packages/riot-doc/src/render.ml`,
12. `packages/riot-doc/src/templates/*.ml`,
13. `packages/riot-doc/src/assets/*`,
14. `packages/riot-cli/src/doc.ml`,
15. `packages/riot-cli/src/cli.ml` (route to new command and docs command docs text where needed),
16. `services/docs.pkgs.ml/src/main.ts` (contract comments and docs notes only).

### Acceptance criteria for MVP

1. `riot doc` works on a simple package and prints generated root path.
2. docs are emitted under a deterministic path and include `index.html` and `search.json`.
3. public pages link dependencies to `docs.pkgs.ml/p/<dep>/<version>/`.
4. generated docs are publishable by docs service using existing `output_prefix` rule.
5. when dependency docs are absent, build still succeeds and link placeholders are visible.
6. service-side docs generation path and artifact shape match current resolver tests in docs service.
7. unchanged source + unchanged dependency lock snapshot should report `Riot_store` cache hit and re-use docs outputs.

## Drawbacks
[drawbacks]: #drawbacks

- First-pass link stitching is allowed to be temporarily incomplete if dependency docs are not yet available.
- Full fidelity docs from all OCaml syntax corners may initially lag behind parser completeness.
- `riot doc` will likely be slower than `fmt`/`check` and will need stronger caching.
- HTML rendering is a larger surface to maintain (templates, assets, mobile layout).

## Rationale and alternatives
[rationale-and-alternatives]: #rationale-and-alternatives

Why this route:

- It reuses Riot-owned language infrastructure (`syn`, `typ`) instead of reintroducing external generator assumptions.
- It makes the existing docs pipeline concrete with minimal service-side changes.
- It aligns with the current docs contract (`docs/<pkg>/<ver>/`) already implemented in services.

Alternatives considered:

- shell out to external odoc/magic generator:
  - simpler upfront, but creates external distribution and versioning drift.
  - weaker control over dependency-link assumptions in our own publish pipeline.
- postpone `riot doc` until `typ` is production-ready:
  - safer short-term, but blocks service contracts already staged.
- pure markdown-only docs:
  - too little structure for API browsers at this stage.

Why not docs.rs integration directly:

- It is Rust-centric and does not match Riot’s OCaml compiler pipeline.
- The import path from `riot.doc` metadata and lockfile semantics differs significantly.

## Prior art
[prior-art]: #prior-art

- `3rdparty/docs.rs` pipeline contract (pipeline request shape, staging, and output bucket path expectations).
- `3rdparty/docs.rs` builder command wiring (explicit assumptions that a package’s docs can be built independently and linked via deterministic base URLs).
- existing OCaml tooling patterns where symbol/URL mapping is injected to keep cross-crate links deterministic.

Riot-specific distinction:

- this proposal is a Riot-owned generator, but with the same publish-time isolation mindset used by docs.rs.

## Unresolved questions
[unresolved-questions]: #unresolved-questions

- Should `riot doc` support a JSON/markdown export in phase 1, or HTML-only is enough?
- Should docs generation honor target selection (native/binary profiles), or always document library-like entries only?
- Should `--all` include package groups, examples, tests, and benches, or be library-only initially?

## Future possibilities
[future-possibilities]: #future-possibilities

- add a docs daemon for local server + live-rebuild,
- add a `riot docs` alias for interactive browser workflows,
- make dependency doc-link repair part of a post-processor,
- include doctest/examples from docs comments and show rendered source snippets,
- integrate docs artifacts with package search index for discoverability,
- version-switch controls on docs pages (`@0.0.1`, `@latest`, etc).
