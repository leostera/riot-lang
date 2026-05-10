# Changelog

All notable changes to `dotenv` are documented here.

## 0.0.1 - 2026-05-01

This release turns `dotenv` from the earlier single-file, minimal loader into a
small but complete package with a documented public API, profile-aware loading,
a parser that matches common dotenv behavior, and tests that cover the cases we
expect users to rely on.

The main adoption idea is: keep calling `Dotenv.*` from application code, but
start choosing explicit policies for existing environment variables and missing
files. Paths are now `Std.Path.t`, and the loader has separate entry points for
"this dotenv file is required" and "load it only if it exists".

### Added

- Added `Dotenv.parse` as the pure parsing entry point. Use this when you want
  to inspect dotenv content, validate it, or transform it without changing the
  process environment. It returns parsed bindings with keys, values, and source
  line numbers, so callers can build diagnostics or tooling on top of the same
  parser used by the loader.

- Added `Dotenv.load_string` for tests and generated dotenv content. This is the
  quickest way to apply a string of dotenv assignments to the current process
  environment without writing a temporary file first.

- Added `Dotenv.parse_files` for reading dotenv files without applying them.
  This is useful for config validation, previews, editor tooling, or tests where
  mutating `Env` would make assertions harder to isolate.

- Added `Dotenv.load_files` for explicit layered loading. Pass the paths in
  priority order, with the most important file first. The loader preserves that
  first-file precedence even when it has to apply lower-priority files before
  higher-priority files internally.

- Added `Dotenv.env_paths` so applications can ask the package which files it
  will consider for a base path and an optional environment profile. This makes
  it easy to show users the exact load order, write tests for profile behavior,
  or plug the same path derivation into custom loading flows.

- Added `Dotenv.load` as the required-file entry point. Use `Dotenv.load ()`
  when the application expects `.env` to exist and should fail clearly when no
  matching dotenv file is present.

- Added `Dotenv.load_if_exists` as the optional-file entry point. Use
  `Dotenv.load_if_exists ()` when `.env` is a local developer convenience and
  the application should keep booting if no dotenv file is present.

- Added profile-aware loading for `.env.{ENV}` files. For example,
  `Dotenv.load ~env:"test" ()` considers `.env.test` and `.env`, giving the
  profile file higher precedence than the base file. This makes test, dev, and
  local overlays first-class instead of requiring every application to assemble
  the path list itself.

- Added support for custom base paths with profiles. For example,
  `Dotenv.load ~path:(Std.Path.v "config/.env") ~env:"local" ()` derives
  `config/.env.local` and `config/.env`. This keeps profile loading useful when
  a project stores configuration outside the repository root.

- Added `KEY=value` assignment parsing with optional whitespace around the
  separator. This covers the normal dotenv shape most users expect.

- Added YAML-style `KEY: value` assignment parsing. This makes the parser more
  compatible with existing dotenv files that use colon separators.

- Added `export KEY=value` parsing. Existing files copied from shell snippets or
  other dotenv implementations can keep their `export` prefix without requiring
  preprocessing.

- Added `export KEY` parsing. When a key is already defined earlier in the file
  or already present in the process environment, `export KEY` reuses that value.
  This matches the common dotenv behavior used by the reference Ruby and Rust
  implementations.

- Added single-quoted value parsing. Single quotes are literal, so values like
  `'${HOST}'` stay exactly `${HOST}` instead of being substituted.

- Added double-quoted value parsing. Double quotes support escape handling and
  variable substitution, making them the right choice for values that need
  spaces, escaped characters, or `${NAME}` interpolation.

- Added unquoted value parsing. Unquoted values support comments after
  whitespace and variable substitution, which keeps simple dotenv files concise.

- Added multi-line quoted value parsing. Quoted values can span lines, so
  certificates, private keys, and other block-like values can be represented
  without custom escaping by the caller.

- Added inline comment handling. A `#` after whitespace starts a comment in
  unquoted values, while `#` inside quoted content remains part of the value.
  This lets users annotate dotenv files without corrupting passwords, URLs, or
  literals that legitimately contain `#`.

- Added `$KEY` substitution. Simple shell-style variable references now work in
  unquoted and double-quoted values.

- Added `${KEY}` substitution. Braced references make it possible to separate a
  variable name from adjacent text, such as `${HOST}:${PORT}`.

- Added deterministic substitution precedence. Process environment values take
  precedence over values parsed earlier from the same dotenv source, and earlier
  parsed values are available to later bindings when the process environment
  does not already define the name.

- Added UTF-8 BOM stripping. Files written by tools that include a BOM at the
  beginning of the file can be parsed without treating the BOM as part of the
  first key.

- Added CRLF and CR line-ending normalization. Dotenv files created on Windows
  or older systems parse the same way as LF-only files.

- Added structured read and parse errors. `Dotenv.error` now distinguishes file
  read failures from parse failures, and `Dotenv.error_to_string` gives callers
  a stable human-readable message for logging or CLI output.

- Added `Dotenv.Events` telemetry. Parsing and loading now emit events for
  successful parses, parse failures, load starts, successful loads, skipped
  missing files, and load failures. Applications with telemetry enabled can
  observe dotenv behavior without wrapping every call site.

- Added runnable examples for the main adoption paths. `parse_string` shows pure
  parsing, `load_if_exists` shows optional local loading, and `profile_loading`
  shows `.env` plus `.env.test` precedence.

- Added package benchmarks. The benchmark suite gives us a baseline for small
  files, substituted values, multi-line values, large files, and in-memory
  loading so parser changes can be checked against performance regressions.

- Added a focused test suite based on behavior covered by dotenv.rb and
  dotenv-rs. The tests exercise syntax compatibility, substitution, profile
  loading, missing-file policy, existing-environment policy, telemetry, and the
  no-mutation parsing APIs.

- Added Markdown API docs in the `.mli` files. `riot doc` now has public
  package docs that explain the supported syntax, loading policies, path model,
  and intended entry points.

### Changed

- Changed the package layout from one implementation file to focused modules.
  `Types`, `Events`, `Environment`, `Parser`, and `Loader` now each own one
  piece of behavior, while `Dotenv` remains the public facade. This makes the
  implementation easier to test and review without forcing users to import
  internal modules.

- Changed file arguments from raw strings to `Std.Path.t`. Callers should now
  write `Std.Path.v ".env"` or use other `Std.Path` helpers. This keeps the API
  consistent with the rest of the Riot/Std ecosystem and avoids passing plain
  strings where path operations are expected.

- Changed existing-environment behavior to be explicit through variants. The
  default is `Dotenv.PreserveExisting`, which keeps already-set process
  environment variables untouched. Use `~on_existing:Dotenv.OverwriteExisting`
  only when dotenv values should replace existing process values.

- Changed missing-file behavior to be explicit through variants. The default for
  multi-file helpers is `Dotenv.SkipMissing`, which is convenient for optional
  overlays. Use `~on_missing:Dotenv.FailMissing` when the first missing file
  should stop loading with a `ReadError`.

- Changed option names from policy nouns to event-style labels. `?existing`
  became `?on_existing`, and `?missing` became `?on_missing`. The new names read
  more clearly at call sites because they describe when the policy is applied.

- Changed profile precedence to be built into the loader. Callers no longer need
  to manually reverse or reorder `.env` and `.env.test`; `Dotenv.load ~env:"test"
  ()` derives the right order and preserves profile values over base values.

- Changed `load` and `load_if_exists` to communicate required versus optional
  behavior through separate functions. This removes the need for a boolean flag
  at call sites and makes boot behavior obvious when reading application code.

- Changed return values for loading functions to report the bindings that were
  actually applied. With `Dotenv.PreserveExisting`, skipped bindings are omitted
  from the returned list, which lets callers report what changed without
  rechecking the process environment.

- Changed multi-file overwrite handling so first-file precedence remains stable
  even with `Dotenv.OverwriteExisting`. Lower-priority files are applied first
  and higher-priority files overwrite them, so the user-facing precedence rule
  stays the same across policies.

- Changed telemetry constructor names to avoid repeating the module name.
  Pattern matches should now use `Dotenv.Events.Parsed`,
  `Dotenv.Events.ParseFailed`, `Dotenv.Events.LoadStarted`,
  `Dotenv.Events.Loaded`, `Dotenv.Events.LoadSkipped`, and
  `Dotenv.Events.LoadFailed`.

- Changed parser behavior to reject invalid input with line-aware parse errors
  instead of silently accepting malformed assignments. This is better for users
  because configuration mistakes fail close to the source of the problem.

- Changed docs and examples to use `Std.Path.t`, `?on_existing`, and
  `?on_missing` consistently, so examples can be copied into applications
  without relying on legacy names or stringly-typed paths.

### Not Supported

- Shell command substitution such as `$(command)` is intentionally not
  supported. The parser treats dotenv files as configuration data, not as shell
  programs. This keeps loading deterministic and avoids executing arbitrary
  commands while reading configuration.

### Migration Guide

- If existing code loaded a string path, wrap it in `Std.Path.v`:

  ```ocaml
  Dotenv.load ~path:(Std.Path.v ".env") ()
  ```

- If existing code used an overwrite boolean, switch to an explicit policy:

  ```ocaml
  Dotenv.load ~on_existing:Dotenv.OverwriteExisting ()
  ```

- If existing code skipped missing files through a boolean flag, use the
  optional entry point for the common case:

  ```ocaml
  Dotenv.load_if_exists ()
  ```

- If existing code needs to fail on a missing file inside a custom file list,
  use `~on_missing:Dotenv.FailMissing`:

  ```ocaml
  Dotenv.load_files ~on_missing:Dotenv.FailMissing [ Std.Path.v ".env" ]
  ```

- If existing code manually loaded `.env.test` and then `.env`, replace that
  custom layering with profile loading:

  ```ocaml
  Dotenv.load ~env:"test" ()
  ```

- If existing code only wants to validate dotenv files, use `Dotenv.parse_files`
  instead of loading them:

  ```ocaml
  Dotenv.parse_files [ Std.Path.v ".env"; Std.Path.v ".env.local" ]
  ```

- If existing telemetry handlers matched old prefixed constructor names, update
  them to the shorter names under `Dotenv.Events`:

  ```ocaml
  match event with
  | Dotenv.Events.Loaded { path; binding_count } ->
      (* record a successful dotenv load *)
      ()
  | _ -> ()
  ```
