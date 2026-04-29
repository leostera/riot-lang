# Routing

Use root `AGENTS.md` as the source of truth for package ownership and routing. This file only summarizes common areas so you know what to read next.

## Core Libraries

- `packages/std/AGENTS.md`: shared standard library APIs, test/snapshot/bench runners, IO, data structures, runtime helpers.
- `packages/kernel/AGENTS.md`: platform shims, FFI, file descriptors, event-loop primitives.
- `packages/colors/AGENTS.md`, `packages/ignore/AGENTS.md`, `packages/http/AGENTS.md`, `packages/blink/AGENTS.md`, `packages/tty/AGENTS.md`: package-specific library contracts.

## Syntax, Formatting, Linting, Typing

- `packages/syn/AGENTS.md`: lexer, streaming parser, syntax tree, AST views, diagnostics, dependency extraction.
- `packages/krasny/AGENTS.md`: OCaml formatting, stream rendering, layout policy, formatter fixtures.
- `packages/fixme/AGENTS.md`: shared rule-authoring APIs for fixers.
- `packages/riot-fix/AGENTS.md`: lint/fix orchestration and generated rule runner behavior.
- `packages/typ/AGENTS.md`: experimental type-analysis work. Confirm current branch expectations before treating failures as blockers.

## Riot Build System And CLI

- `bootstrap.py` and `packages/miniriot`: first-build path before the normal `riot` binary exists. Read [bootstrap and miniriot](bootstrap.md) before touching this area.
- `packages/riot-model/AGENTS.md`: shared workspace, profile, package, and target model.
- `packages/riot-deps/AGENTS.md`, `packages/pubgrub/AGENTS.md`, `packages/pkgs-ml/AGENTS.md`: dependency solving and registry behavior.
- `packages/riot-planner/AGENTS.md`, `packages/riot-build/AGENTS.md`, `packages/riot-store/AGENTS.md`, `packages/riot-toolchain/AGENTS.md`: planning, build runtime, caches, toolchain invocation.
- `packages/riot-cli/AGENTS.md`: user-facing CLI flows and event rendering.
- `packages/riot-fmt/AGENTS.md`, `packages/riot-run/AGENTS.md`, `packages/riot-install/AGENTS.md`, `packages/riot-publish/AGENTS.md`, `packages/riot-init/AGENTS.md`, `packages/riot-bench/AGENTS.md`: command-specific wrappers.

## Compilers, Editors, Specs, Docs

- `compiler/*/AGENTS.md`: RAML compiler frontend/backend/facade ownership.
- `editors/*/AGENTS.md`: editor integrations; keep editor UX thin and consume Riot JSON surfaces where available.
- `specs/*/AGENTS.md`: formal models and their executable expectations.
- `docs/AGENTS.md`: living docs and RFD handling.

## Rule

If a change touches more than one routed area, read each relevant `AGENTS.md`. If package boundaries or behavior contracts move, update the affected AGENTS files.
