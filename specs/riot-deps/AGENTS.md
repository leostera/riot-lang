# riot-deps spec AGENTS

This directory contains readable formal models for `packages/riot-deps`
semantics.

## Source Map

- `packages/riot-model/src/package.mli`: dependency edge vocabulary
- `packages/riot-model/src/lockfile.mli`: lockfile/package identity surface
- `packages/riot-deps/src/dep_solver.ml`: dependency graph projection and
  package-resolution rules
- `packages/pubgrub/src/*.ml`: version-solving substrate

## Rules

1. Keep these specs focused on package-management and dependency-graph
   semantics, not generic executor or cache behavior.
2. Model the contract Riot wants to expose to users first, then map that back
   to the current implementation shape in comments.
3. Prefer small, bounded feature-resolution slices over one full package
   manager model.
4. Be explicit about what is out of scope, especially when a model covers
   graph semantics but not version solving or source rewriting.
5. Update `README.md` when the modeled package-management behavior or validation
   commands change.

## Validate

From the repo root:

```sh
timeout 60 java -cp "/Applications/TLA+ Toolbox.app/Contents/Eclipse/tla2tools.jar" \
  tlc2.TLC specs/riot-deps/<Slice>.tla -config specs/riot-deps/<Slice>.cfg
```
