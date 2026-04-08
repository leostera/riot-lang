# VS Code Extension TODO

This document compares `editors/vscode-riot-ml` against:

- `3rdparty/rust-analyzer/editors/code`
- `3rdparty/vscode-ocaml-platform`

The goal is not to copy those extensions blindly. The goal is to identify the
feature gaps, architecture gaps, and quality-of-life details that make them feel
finished, and decide what is worth porting into Riot's thinner CLI/LSP-first
model.

## Highest Leverage

- [x] Add explicit `Riot: Restart Language Server`, `Riot: Start Language Server`, and `Riot: Stop Language Server` commands.
  `rust-analyzer` exposes lifecycle control directly. Our extension currently tries LSP first and silently falls back to CLI behavior, but it gives the user no operational control when the LSP wedges, needs a restart, or should be kept stopped during debugging.

- [x] Add `Riot: Show Riot LSP Output` and `Riot: Show Riot Extension Output`.
  Both reference extensions expose their output channels as first-class commands. Right now our logs exist, but discovering them depends on VS Code internals rather than an explicit command surface.

- [x] Add a visible server/workspace status indicator.
  `rust-analyzer` exposes status and health much more clearly. We should surface states like `starting`, `ready`, `formatting`, `checking`, `waiting on lock`, and `server stopped` in a status bar item instead of forcing users to infer that from missing diagnostics or stalled commands.

- [x] Make LSP availability and fallback state explicit in the UI.
  Today the extension silently flips between `riot lsp stdio` and CLI fallback. That keeps the extension resilient, but it hides an important behavioral shift from the user.

- [x] Add `Riot: Run Check` and task integration for `riot check`.
  `rust-analyzer` has an explicit flycheck/check workflow. We now have `riot check`; the VS Code extension should expose it directly instead of limiting the command surface to `build` and `test`.

- [x] Add `Riot: Run` for runnable binaries.
  Riot already has a real `riot run` CLI surface. The extension should expose that directly so users can run the current package's main binary from the UI instead of dropping to the terminal.

- [ ] Add proper command palette coverage for the things users actually need every day.
  The current command surface is very small compared with both reference extensions. At minimum we should expose restart server, show logs, run check, run current binary, run tests, format current file, fix current file, add dependency, remove dependency, and refresh diagnostics as discoverable commands.

## Editor Features We Still Lack

- [ ] Add `codeActionKind`-aware command palette commands such as `Riot: Fix Current File` and `Riot: Apply Available Quick Fixes`.
  We already have LSP quick fixes, but the extension still lacks higher-level editor commands that make those fixes easy to invoke without learning the editor's built-in code action UX.

- [ ] Add `source.fixAll` entry points in the extension UI.
  The LSP can expose file-level fix-all, but the extension should also make it obvious through command palette commands and menus.

- [ ] Add hover/documentation affordances for Riot diagnostics.
  `rust-analyzer` invests heavily in hover UX. For Riot, a smaller but valuable version would be commands like `Riot: Explain Diagnostic Under Cursor` that call `riot fmt --explain`, `riot fix --explain`, or `riot check --explain`.

- [ ] Add type-centric commands once the LSP supports them.
  `vscode-ocaml-platform` exposes commands like copy type under cursor, search by type, hole navigation, and type selection. Riot should grow the equivalent surface gradually as `riot-lsp` grows: type under cursor, hover explain, go to interface/implementation, typed hole navigation, and search-by-type once the server can answer them.

- [ ] Add implementation/interface switching.
  OCaml Platform exposes `switch implementation/interface`; that is a very natural fit for `*.ml` and `*.mli` in Riot workspaces.

- [ ] Add nearest package/workspace aware commands.
  The extension already detects the nearest `riot.toml` for add/remove dependency. That same package-awareness should drive build, test, check, and future commands so the command palette acts on the right package by default.

- [ ] Add runnable/package discovery for `riot run`.
  If a package has exactly one runnable binary, the extension should offer a direct "Run Current Package" action. If there are multiple runnable binaries, it should show a picker instead of forcing the user to know names and flags.

- [ ] Add first-class test execution from the UI.
  The extension already has workspace-level `test`, but we should support at least:
  - run tests for nearest package
  - rerun last test selection
  - run tests matching a query
  - later, run test at cursor if Riot can map source locations or names reliably

- [ ] Add debug entrypoints once the CLI/runtime contract is stable.
  `rust-analyzer` treats run/debug as siblings. Riot should eventually expose `Run` and `Debug` for runnable binaries and test binaries, but only once the CLI surface for debugger-friendly execution is stable.

- [ ] Add editor title/context menu entries for Riot commands.
  Both reference extensions use menus and `when` clauses to put the right commands in the right places. Our extension currently relies almost entirely on command palette discovery.

- [ ] Add targeted `when` clauses for command visibility.
  `vscode-ocaml-platform` and `rust-analyzer` both hide commands aggressively when they are not relevant. We should only show package commands in Riot workspaces, only show OCaml editor actions for `*.ml`/`*.mli`, and only show server-control commands when the LSP is running or stoppable.

- [ ] Add "show generated/failing command" affordances for build, test, check, fix, and format.
  OCaml Platform and rust-analyzer are better at making the operational command visible. Riot users should be able to inspect the exact invocation and cwd when something fails.

## Diagnostics And Feedback

- [ ] Improve diagnostics presentation instead of only publishing raw LSP/CLI diagnostics.
  `rust-analyzer` customizes diagnostic handling to make compiler output easier to inspect. We should do a Riot version of that: cleaner messages, explicit source labels, and commands to open full diagnostic details when the inline message is insufficient.

- [ ] Add better progress reporting for long-running commands.
  We already know Riot emits build/check/fix/fmt JSON events. The extension should parse those events and reflect progress like `Building fix runner`, `Waiting on Riot lock`, or `Checking package foo` instead of showing a generic spinner.

- [ ] Add cancellation for long-running extension-driven commands.
  `rust-analyzer` exposes cancel/clear/run flows for check-style work. Riot commands like `check`, `fix`, and perhaps package-scoped `build` should be cancellable when launched from the extension.

- [ ] Separate parser, lint, and type diagnostics more cleanly in the UI.
  Users should be able to distinguish syntax errors, lint findings, and type errors immediately. The extension should preserve source identity and avoid collapsing all Riot diagnostics into one undifferentiated bucket.

- [ ] Add commands to clear or refresh diagnostics intentionally.
  `Riot: Refresh Diagnostics` exists, but we should likely also have explicit clear/recheck flows once `riot check` becomes a heavier background capability.

## Workspace And Project UX

- [ ] Add stronger project detection and activation behavior.
  `rust-analyzer` and OCaml Platform activate from workspace signals like `Cargo.toml`, `dune`, `opam`, and file languages. Riot should likely activate on `riot.toml` and relevant OCaml file types, not just startup and language open.

- [ ] Add context keys like `inRiotProject`, `riotLspRunning`, and `riotManagedInstallAvailable`.
  `rust-analyzer` uses context keys heavily to gate commands and menus. We should do the same so irrelevant commands do not appear outside Riot workspaces.

- [ ] Add workspace-folder aware behavior for multi-root workspaces.
  The current extension mostly assumes a single nearest root lookup. The reference extensions are more deliberate about workspace scoping.

- [ ] Add safer handling for files outside the active Riot graph.
  `rust-analyzer` warns about unlinked files. Riot should decide what to do when a file is opened outside a usable `riot.toml` root or outside the package the LSP/checker can reason about.

- [ ] Add explicit "Riot not installed" and "Riot version mismatch" states rather than relying on startup notifications.
  The current startup messaging is too noisy for happy paths and too weak for failure paths.

- [ ] Replace eager startup popups with quieter, stateful surfaces.
  The current extension shows startup notifications for "found Riot" and "new version available". The reference extensions lean more on status bars, commands, and deliberate prompts. Riot should reserve notifications for actionable problems, not normal startup.

- [ ] Consider an activity-bar Riot view once the command surface grows.
  OCaml Platform ships an `OCaml` explorer container with dedicated views for sandbox state, commands, and help. Riot does not need that immediately, but once we have more workflows it may be worth adding a lightweight `Riot` activity view with commands, package actions, logs, and install/status affordances.

## Commands And Tasks

- [ ] Expand task support beyond `build` and `test`.
  `riot check`, package-scoped build/test/check, and possibly fix/format checks should be available as tasks where that makes sense.

- [ ] Add better task/problem matcher integration if Riot emits stable machine-readable diagnostics for those flows.
  Right now tasks are process wrappers. The reference extensions feel more integrated because the command/task layer is richer.

- [ ] Add separate commands for workspace-wide vs package-scoped actions.
  `Riot: Build Workspace` is too coarse for large repos. The extension should let users run commands on the nearest package or the whole workspace explicitly. The same applies to `run`, `test`, and `check`.

- [ ] Assign task groups and labels more deliberately.
  `rust-analyzer` assigns build/test/clean groups so the task runner feels native. Riot tasks should distinguish build, test, check, and clean-ish flows properly instead of just exposing generic process wrappers.

- [ ] Add runnable and test tasks where the target is stable enough to serialize in `tasks.json`.
  Build/test/check are obvious. If we can define a stable task model for `riot run <name>` and package-scoped `riot test`, those should be task-provider entries too.

## Architecture Improvements

- [ ] Split extension state into explicit subsystems.
  `rust-analyzer` has clearer separation between activation, config, client lifecycle, tasks, diagnostics, and commands. `vscode-riot-ml` is still small, but it is already mixing lifecycle, install/update flow, tasks, commands, diagnostics fallback, and formatting concerns.

- [ ] Introduce a dedicated client/session manager module.
  `editor_features.ts` currently owns client startup, fallback activation, and disposal in one place. We should move toward a `riot_lsp_client.ts` or `server_manager.ts` that owns lifecycle, restart policy, output channels, and status reporting.

- [ ] Introduce a dedicated configuration module.
  `rust-analyzer` has a serious configuration layer because configuration changes have operational consequences. Riot should at least centralize config reading, validation, change handling, and which changes require server restart versus lightweight refresh.

- [ ] Define which configuration changes require restart, refresh, or no-op.
  `rust-analyzer` explicitly distinguishes settings that require server restart from those that only require a window reload or lightweight reconfiguration. Riot should do the same for settings like `riot.path`, diagnostics toggles, install URLs, and future check/lsp settings.

- [ ] Introduce an explicit command registry instead of registering everything inline in `extension.ts`.
  Both reference extensions are easier to reason about because commands are modeled as a surface, not scattered inline callbacks.

- [ ] Add lazy output-channel construction.
  `rust-analyzer` does not eagerly create every output channel. Riot should likely create the extension log, Riot command log, and Riot LSP log lazily so the extension starts quietly and stays cheap when users only need formatting.

- [ ] Separate extension output from Riot command output from Riot LSP output.
  OCaml Platform exposes multiple output channels. Riot should likely have:
  - extension/runtime log
  - Riot LSP log
  - Riot command/task log

- [ ] Normalize all extension-driven Riot invocations through a single command runner with cancellation, cwd resolution, env control, JSON event parsing, and output routing.
  Right now `runRiot` is a thin spawn wrapper. That was enough to bootstrap, but it is too primitive for a serious editor integration.

- [ ] Separate "server lifecycle state" from "editor feature availability".
  `editor_features.ts` currently couples "can I start the LSP?" with "should I register fallback formatting/diagnostics?" We should model those as separate states so commands, menus, and status surfaces can explain what is happening instead of just silently flipping behavior.

- [ ] Make fallback behavior a deliberate strategy object, not an implicit branch.
  LSP-first with CLI fallback is correct for Riot, but the boundary should be explicit so commands can declare whether they require LSP, can fall back to CLI, or should fail visibly.

- [ ] Add stronger tests around activation, root detection, CLI fallback, and command routing.
  The reference extensions are more mature partly because the architecture is easier to test in isolation.

- [ ] Add extension tests around config changes and restart semantics.
  Once the extension has a real config layer, we should test things like "changing Riot binary path prompts restart", "disabling diagnostics refreshes collections", and "server-control commands mutate status correctly".

## Small Details That Make Good Extensions Feel Good

- [ ] Contribute more menus with `when` clauses instead of making users remember command names.
- [ ] Add icons for user-facing commands where that improves discoverability.
- [ ] Add better output-channel naming and documentation.
- [ ] Add walkthrough or onboarding content once the extension surface is larger.
- [ ] Add conflict detection with other OCaml extensions if we find known bad combinations.
- [ ] Add clearer release/version reporting for the Riot binary and the Riot LSP server.
- [ ] Add first-class log toggles and "open logs" commands.
- [ ] Add richer README screenshots and feature documentation once the extension surface is stable.
- [ ] Add one-shot actionable install/upgrade prompts instead of repeated generic messaging.
- [ ] Add a "copy generated command line" command for failed build/test/check/fix runs.
- [ ] Add explicit commands to open the Riot binary path and version info the extension resolved.

## Concrete Reference Patterns Worth Stealing

- [ ] `rust-analyzer`: status bar as a real state machine.
  It distinguishes healthy, warning, error, and stopped states, changes command targets, and uses the tooltip as a compact operational dashboard. Riot should do a smaller version of this instead of using transient notifications.

- [ ] `rust-analyzer`: "runnables" as a first-class concept.
  One of the biggest qualitative differences is that it treats executable things as discoverable editor objects. Riot should eventually do the same for binaries and tests instead of only exposing generic workspace commands.

- [x] `rust-analyzer`: explicit context keys.
  Commands and views are driven by context like `inRustProject`. Riot should add `inRiotProject`, `riotLspRunning`, `riotCliFallbackActive`, and probably `riotManagedInstallPresent`.

- [x] `rust-analyzer`: separate trace output and server output.
  Riot should likely split "extension/runtime log", "Riot LSP", and "Riot command log" instead of funneling everything into one place.

- [x] `rust-analyzer`: central `Ctx`/manager object.
  We do not need a direct clone, but we do need one place that owns status bar state, output channels, client lifecycle, context keys, and restart behavior.

- [ ] `vscode-ocaml-platform`: explicit command registry.
  Commands are registered as a real API surface rather than ad hoc inline callbacks. That scales much better once the extension starts adding type, check, fix, and project-management commands.

- [x] `vscode-ocaml-platform`: dedicated output channels by concern.
  It has separate output channels for language server, extension, and commands. Riot should copy that pattern.

- [x] `vscode-ocaml-platform`: status bar items for project state.
  OCaml Platform exposes sandbox/documentation state in the status bar. Riot should expose package/workspace identity and LSP/check state there.

- [ ] `vscode-ocaml-platform`: activity-bar command surfaces.
  It uses a dedicated explorer container for commands/help/project state. Riot does not need this immediately, but it is a good model once command count grows.

- [ ] `vscode-ocaml-platform`: project-state-aware prompts.
  It does a better job of turning missing prerequisites into actionable prompts like install/select/generate. Riot should do the same for missing `riot`, missing `riot.toml`, missing lockfiles, or unsupported workspace states.

- [ ] `vscode-ocaml-platform`: OCaml-native command surfaces.
  Even when the backend is doing the real work, the extension still offers language-native UX like switch impl/intf and type-aware actions. Riot should aim for the same level of language-native affordance around running, testing, and inspecting code.

## Things The Reference Extensions Do Better Today

- [ ] `rust-analyzer` treats server lifecycle as a first-class UX concern.
- [ ] `rust-analyzer` invests in context-aware menus instead of a command-only surface.
- [ ] `rust-analyzer` has much better diagnostic and progress plumbing.
- [ ] `rust-analyzer` has a richer configuration model and clearer restart semantics.
- [ ] `vscode-ocaml-platform` exposes more OCaml-native workflows directly in the editor.
- [ ] `vscode-ocaml-platform` has better output-channel discoverability.
- [ ] `vscode-ocaml-platform` models workspace/sandbox/project state much more explicitly than Riot currently does.

## What We Should Probably Not Copy

- [ ] Do not move core Riot behavior into TypeScript just because the reference extensions do more on the client side.
  Riot should keep formatting, diagnostics, fixes, and type analysis inside Riot CLI/LSP surfaces.

- [ ] Do not build heavy extension-side project modeling if Riot CLI/LSP can answer the question.

- [ ] Do not add a huge command surface before the corresponding Riot backend capability is stable.

## Suggested Order

- [x] Phase 1: lifecycle and observability
  Add restart/start/stop server commands, output channel commands, status bar item, context keys, and explicit fallback state.

- [ ] Phase 2: command surface and package-aware workflows
  Add `check`, `run`, package-scoped build/test/check/run, fix current file, explain diagnostic, and menu integration.

- [ ] Phase 3: richer diagnostics and progress
  Parse Riot JSON events for progress, cancellation, and richer diagnostic detail commands.

- [ ] Phase 4: OCaml-native workflows on top of Riot LSP
  Add implementation/interface switching, type under cursor, and future type/hole/search commands as the server grows.

- [ ] Phase 5: runnable and test UX
  Add runnable discovery, run/debug entrypoints, rerun-last flows, and possibly VS Code Testing API integration once Riot can expose stable enough test identities.
