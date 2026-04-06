# riot-model AGENTS

`riot-model` defines the shared types for the build system: workspaces, packages, modules, actions, events, targets, and errors.

## Rules

1. Keep this package free of execution policy. It is the shared vocabulary for the rest of riot.
2. Prefer structured variants and records over loosely typed payloads.
3. Scoped package phases (`Build`, `Runtime`, `Dev`) live here; changes to that shape usually require follow-up in planner, executor, server, and CLI code.
4. Be conservative about breaking public type shapes.
5. Workspace build-path configuration lives in the root `riot.toml` under `[riot].target_dir`; treat that as the source of truth for `_build`-style paths.
6. Formatter ignore configuration lives under `[riot.fmt]` (`ignore = ["substring", ...]`) on both workspace and package manifests. Bare `[fmt]` is only a compatibility fallback.
7. The default `debug` profile is the debugger-friendly baseline: native code with debug symbols and minimal optimization (currently `-inline 0` plus `-g`). Do not silently drift it back toward bytecode or optimized native output.
8. `Ocaml_compiler` owns the shared OCaml warning/flag vocabulary and its string codec. Do not duplicate warning/flag parsing in planner or toolchain packages.
9. `User_config` is the shared `~/.riot/config.toml` vocabulary. Registry entries carry `api_url`, `cdn_url`, and `api_token`; preserve those fields across parse/save/update paths.
10. Test support trees under `tests/fixtures/`, `tests/generated/`, and `tests/diagnostics/` are non-compilable inputs. Keep them out of `Package.sources.tests` and test-binary autodiscovery.
11. Lockfile dependency entries should stay flat and exact for registry packages: render `name`, `version`, and `sha256` directly in the dependency table instead of nesting a second `package = { ... }` object.
12. `Lockfile.t` carries a required `dependency_hash` derived from the raw `[dependencies]`, `[build-dependencies]`, and `[dev-dependencies]` sections of workspace manifests. Treat it as the staleness contract for `riot.lock`, not as optional metadata. Empty lockfiles should still serialize a concrete `packages = []` field so bootstrapped workspaces round-trip cleanly.
13. Member package manifest decode failures are workspace load errors, not silent drops. `Workspace_manager.scan` should preserve those failures in `load_error list` so CLI commands can fail honestly instead of pretending the member disappeared.
14. `Package.t` is a private record. Read fields freely, but construct synthetic packages through `Package.synthetic` and keep new package creation logic inside `Package`.
15. Keep `Package` values canonicalized at construction time. Hash-relevant lists such as dependencies, binaries, source buckets, fix providers, and override tables should be sorted or deduped when a `Package.t` is created or scope-adjusted, not re-sorted inside `Package.hash`.
16. Package-management progress belongs on the shared `Event.kind` surface. When `riot add` / `riot rm` / `riot update` need new lifecycle reporting, add structured PM events here instead of inventing a parallel event type in `riot-deps` or `riot-cli`.
17. `Workspace_manager.scan` should still surface real member and local-package manifest failures, but it must not raise load errors for missing external `path` dependencies when that dependency also carries a publishable fallback (`version` or `source`). Those are resolved later by `riot-deps`.
18. Repository-local operational policy belongs in `.riot/config.toml`, modeled by `Workspace_operational_config`. Keep it separate from `riot.toml` build/package semantics and `~/.riot/config.toml` user config. Test-runner policy such as `[riot.test].small_test_timeout` and `[riot.test].flaky_max_retries` belongs here too.
19. `Workspace_manager.scan` should resolve the build root in one upward pass: prefer the nearest enclosing workspace manifest with `[workspace]`, but if none exists, fall back to the first package manifest with `[package]` and synthesize a one-package workspace so detached package roots still build.
20. `Package.from_toml` should autodiscover a default runtime binary named after the package when `src/main.ml` exists and no explicit `[[bin]]` entries are declared. Keep that fallback minimal; tests/examples/bench autodiscovery remains separate.

## Validate

`timeout 30 riot build riot-model`
