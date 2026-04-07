# Riot Dependency Specs

This directory contains readable TLA+ models for `riot-deps` semantics that are
subtle enough to deserve an executable design artifact.

The current scope is intentionally small: graph-level feature propagation,
ambient default behavior, and `default_features = false` cuts.

## Layout

- `PackageFeaturePropagation.tla`: package-feature propagation semantics for
  explicit requests, ambient defaults, and graph cuts.
- `PackageFeaturePropagation.cfg`: smoke config for the cut-and-override
  scenario discussed for the package-features RFD.
- `PackageFeaturePropagationAlternatePath.cfg`: defaults still apply when at
  least one reachable path allows them.
- `PackageFeaturePropagationStickyCut.cfg`: once a path crosses
  `default_features = false`, lower edges on that path cannot re-enable ambient
  defaults.
- `PackageFeaturePropagationMultipleRoots.cfg`: multiple roots contribute to one
  shared effective feature set.
- `PackageFeaturePropagationRootPatch.cfg`: a root can patch an underspecified
  transitive dependency by adding explicit features on the shared package.
- `PackageFeaturePropagationExplicitPlusDefaults.cfg`: explicit requests and
  ambient defaults can coexist on the same edge.
- `PackageFeaturePropagationUnreachable.cfg`: unreachable subgraphs do not
  contribute features or defaults.
- `PackageFeaturePropagationDuplicate.cfg`: duplicate explicit requests are
  idempotent.
- `PackageFeaturePropagationTransitiveDefaults.cfg`: defaults can still
  propagate transitively through libraries when no cut is present.

## How To Work On The Spec

- Keep the model readable first.
- Prefer one slice per semantic concern.
- Keep the graph small and purpose-built in the configs.
- Comment every important divergence from Cargo-like semantics.

## Validation Commands

Run these from the repo root:

```sh
timeout 60 java -cp "/Applications/TLA+ Toolbox.app/Contents/Eclipse/tla2tools.jar" \
  tlc2.TLC specs/riot-deps/PackageFeaturePropagation.tla \
  -config specs/riot-deps/PackageFeaturePropagation.cfg
```
