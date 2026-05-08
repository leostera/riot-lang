# Changelog

## 0.0.32 - 2026-05-04

### riot
- Added `riot fuzz`, a coverage-guided fuzzing command for `Std.Test.fuzz` cases. Riot can now discover fuzz cases from test binaries, run campaigns with corpus/crash persistence, replay saved inputs, minimize coverage-redundant corpuses, emit JSON events, and serialize fuzzing through a workspace fuzz lock.
- `riot test` and generated test binaries now understand fuzz cases as first-class test cases. Seed inputs still replay through normal test runs, while `riot fuzz` can drive the same case with generated inputs and mutator/corpus metadata.
- `riot doc` now reuses a generated manifest to skip unchanged documentation builds, shares generated CSS and JavaScript assets across package docs, restores syntax highlighting for fenced code blocks, distinguishes values from functions, renders package overview metadata, and lists variant constructors without extra pipe markers.
- The installer now supports explicit version and install-directory selection. Use `curl -sSL https://get.riot.ml | sh -s -- -v 0.0.32` to install a specific Riot version, or pass `--riot-dir <dir>` to install outside `$HOME/.riot`.
- Release guidance now documents the normal dirty-worktree flow: releases may proceed with unrelated dirty files as long as `./packages`, real `riot.toml` manifests, and release inputs are committed.

### std
- `Std.Test.fuzz` adds the public test-case surface used by `riot fuzz`, including seed replay, corpus metadata, mutator hints, and `run-fuzz-case` execution support inside generated suite binaries.
- Conversion helpers continue moving to explicit `from_*` names, with unchecked or panic-capable conversions named accordingly. This keeps public APIs clearer about whether a value is parsed, converted, or assumed valid.

### suri
- Interface docs were cleaned up so generated documentation attaches summaries and details to the intended public items instead of carrying stale separator or misplaced doc comments.

## 0.0.31 - 2026-05-03

### riot
- Riot's build cache now keys package actions by the output hashes of dependency artifacts, not only by the inputs used to plan the action. This prevents stale library artifacts from being reused after an upstream dependency rebuilt to a different `.cmi`/`.cmx` shape, fixing inconsistent-assumption failures such as mismatched generated alias modules.
- `riot-store` now fails loudly when an action declares outputs that were not produced. Incomplete or broken action sandboxes are no longer saved as if they were valid cache entries, which makes cache corruption and cross-target output bugs visible at the point they happen.
- `riot-planner` rejects empty library plan bundles and bumps the planner artifact version, forcing old build-cache entries through the corrected dependency-output hashing path.
- Module dependency analysis now treats a reference from `Config.ml` to `Config` as an external or opened dependency when one is available, instead of always interpreting it as a circular self-reference. This lets modules such as application `Config.ml` use `Std.Config` after `open Std`.

### krasny
- Inline snapshot assertions now compile cleanly under warning-as-error release builds. Snapshot-heavy formatter tests no longer trigger warning 10 from a non-unit expression in generated test code.

## 0.0.30 - 2026-05-02

### riot
- `riot doc` now renders root modules as real detail pages, excludes executable entrypoints from documentation packages, and improves module-page structure so top-level items have working links, signatures, summaries, and detail sections.
- `riot doc` now extracts record field docstrings without duplicating raw comment syntax in rendered signatures, and fenced Markdown code blocks preserve their relative indentation after only the shared doc-comment padding is stripped. This keeps rendered examples readable, including nested `match` branches and indented error handling.
- The installer now tags Riot binary downloads with `X-Riot-Agent: riot-install@1`, so CDN download metrics can distinguish install-script traffic from CLI or other pipeline downloads. The 0.0.30 binary release re-uploads `install.sh` with this behavior.

### blink
- Removed Blink's built-in managed circuit breaker from the HTTP client surface. Applications should own circuit-breaker policy at their API boundary, where they have the context to choose failure thresholds and reset behavior.

### krasny
- Signature comments now stay attached to the value or type item that follows them. Formatting no longer inserts an unwanted blank line between a docstring/comment block and the signature item it documents.

### postgres
- PostgreSQL protocol parsing is more defensive and better documented, with broader coverage for invalid or partial wire messages. Driver behavior is clearer around malformed server input while preserving typed error reporting.

### sqlx
- SQLx is marked public again in the release manifest, so it is included in the published package set and remains available through the registry.

### std
- `Std.Log.start_link` now reads `RIOT_LOG` and configures the default log level automatically, removing the need for every application to reimplement the same environment parsing boilerplate.
- `Std.Test` suites now support optional `setup` and `teardown` hooks. Setup failures fail the suite before tests run, while teardown failures are reported after the suite completes.
- The Riot standard library no longer exposes the old `Char.chr` spelling. Use the explicit `from_int` / unchecked conversion APIs instead.

## 0.0.29 - 2026-05-01

### riot
- `riot build --all` now builds test, bench, and example artifacts only for workspace packages. Downstream dependencies are still built as normal libraries, but their development artifacts are no longer pulled into unrelated workspace builds.

### kernel
- Blocking file and socket IO now copies OCaml heap-backed buffers before entering blocking sections, and copies read data back after returning. This prevents the GC from moving heap buffers while native read/write calls are in progress.

### postgres
- PostgreSQL connection writes are now serialized through the driver, preventing concurrent operations from interleaving wire-protocol messages on the same connection.

### pubgrub
- Restored the published `Pubgrub.mli` interface as valid source text, so dependency analysis and downstream builds can parse the package interface normally.

### sqlx
- Added `Sqlx.Migrate`, a migration API for discovering, applying, and tracking database migrations from SQLx applications.
- Expanded SQLx documentation around connections, pools, transactions, and migrations so users have concrete guidance for wiring migration workflows into applications.

### std
- `Std.Fs.File.write_string`, `write_all`, and file-backed writers now route string data through `IO.Buffer` and vectored IO. High-level file writes use the same off-heap safe path as the rest of the IO stack while preserving writable retry behavior.

## 0.0.28 - 2026-05-01

### riot
- Fixed `riot-planner` dependency wiring for nested local modules in published packages. Downstream workspaces can now build `kernel` and `std` from the registry again when modules refer to sibling nested modules such as `Regex_stubs` or generated child roots such as `Algo` after an `open`.
- Fixed `riot build --all` in downstream workspaces that depend on packages with published `riot-fix` providers. Riot now builds fix provider runners only for workspace-member packages, so dependency-provided rules no longer create a synthetic `fixme-runner -> riot-fix` edge that the downstream workspace cannot satisfy.
- `riot publish` now accepts `--skip-fmt` for release operators that need to publish a known-good build while intentionally bypassing the `riot fmt --check` preflight. This mirrors `--skip-check` for the fix preflight while keeping the skipped stage explicit in the command line.

## 0.0.27 - 2026-05-01

### riot
- `riot-lsp` now exposes typed hover, inlay hints, semantic completion, and editor-facing diagnostic handling backed by the new Typ inference work. Editors can show richer type information and completion results without depending on the old syntax-view path.
- `riot.nvim` now wires completion and LSP logging through the Riot LSP integration, making editor debugging and completion behavior easier to inspect.
- `riot-planner` now suggests similar available module names when dependency graph verification finds a missing module such as a casing or underscore mismatch. The error path does a little extra work to make module-name mistakes actionable.
- `riot-fix` now reports nested match depth only after the third nested match, so shallow two- and three-level matches no longer trigger the lint.
- `riot-store` preserves cache generation recency more accurately, avoiding cache bookkeeping that can make recently used entries look stale.
- The workspace release set no longer includes the obsolete `parquet` and `pretext` packages. They were removed from the active workspace so release builds and package planning only cover maintained packages.

### krasny
- Formatter policy is more stable for field access, including qualified record fields such as `Module.record.field`, dereference field access, and constructor-like field expressions.
- Match-case comments are preserved before their cases, preventing formatter runs from dropping meaningful comments inside pattern matches.
- Pattern layout was tightened for multiline lists, constructor records, inline records, and complex syntax. Closing delimiters now align with their opening syntax, constructor record payloads align under the constructor, and small readable record/list patterns stay inline when they fit.
- Let right-hand-side layout now retries after width overflow and keeps value delimiters such as `{`, `[`, and `[|` attached to value bindings while function bodies still break after `->`.

### syn
- Syn now uses its own `Span.t` instead of depending on Ceibo for source spans, reducing parser dependencies and keeping source locations in the parser package.
- The semantic Ast views were tightened around explicit identifiers, required fields, local spans, and typed view modules. Downstream tools can rely on stronger `Syn.Ast` handles instead of token-list identifiers or generic nodes.
- The Ast implementation was split into a library directory so identifiers, tokens, nodes, type expressions, and related view helpers can be maintained in smaller focused modules.
- Dotted field access is parsed as field access rather than as a plain identifier, including qualified forms such as `Hello.record.field`.

### markdown
- Markdown no longer depends on Ceibo, which narrows the parser stack and leaves Ceibo out of the active release path.

### std
- Snapshot tests now recreate pending `.expected.new` files on every failing run, even when a pending file already exists. This keeps snapshot failures fresh while iterating.
- Snapshot diffs now prefer colored diff output, making review of expected/actual changes easier in terminals.
- Fixture discovery no longer sorts eagerly when ordering is not semantically relevant, reducing unnecessary latency before tests start running.

### typ
- Typ gained a source-backed diagnostic fixture runner, JSON diagnostics, and source-rendered diagnostic output so editor and CLI integrations can consume type-analysis errors more directly.
- The experimental inference engine now handles functions, lets, tuples, arrays, lists, records, record updates, field access, constructors, constructor payloads, pattern matching, modules, module aliases, includes, functors, polymorphic variants, GADTs, and inline record constructors across a broader fixture corpus.
- Typ now tracks expression types and query context for LSP features such as hover, completion, and stable inlay hints.
- Type rendering and inference environment internals were refactored around explicit scopes, constructor descriptions, record field inference, and source-backed Ast views, making future checker work less dependent on ad hoc syntax lowering.

## 0.0.26 - 2026-04-28

### riot
- `riot build --all` now builds package-provided `riot-fix` rule runners as part of the all-artifacts graph. Packages that ship custom lint rules now fail during the normal workspace build when their generated runner no longer compiles, instead of surprising users later during `riot fix`.
- Generated fix-rule runners now use the same binary entrypoint shape as regular Riot executables: `let main ~args = ...` plus `Runtime.run ~main ~args:Env.args`. This keeps provider binaries compatible with Riot's entrypoint validation.
- `riot fix --check` works with the new `Syn.Ast`-based rule pipeline, including generated package providers. Parse diagnostics and lint diagnostics stay separate, which makes fix output easier to interpret.
- Debug builds now treat OCaml warning 6, omitted labels in function application, as an error. This catches calls that accidentally drop required labels during development instead of letting them pass as warnings.
- `riot test` now honors repeated `-p` filters. Commands such as `riot test -p syn -p krasny` now run both selected packages instead of silently using only the last package flag.
- Human test output now shows per-test timings. Normal timings are subdued, slow small tests are highlighted, and failures render in bold red while JSON output remains machine-readable.
- Snapshot commands start reporting work as pending snapshots are found instead of waiting for a full repository scan. `snapshot accept`, `snapshot reject`, and `snapshot review` only scan supported snapshot locations, so large workspaces get interactive feedback much sooner.
- `snapshot review` now exits cleanly without printing action prompts when there are no pending snapshots.
- `riot publish` now emits an availability check event before querying the registry for an already-published version. Long registry lookups no longer leave human or JSON output completely silent before format/build checks begin.
- Obsolete checked-in Riot binary artifacts were removed from the repository so releases are built from the current release pipeline instead of stale local binaries.

### blink
- Blink now follows the stricter HTTP and WebSocket validation introduced in `http`. Client, transport, websocket, and error paths surface protocol errors more consistently instead of accepting malformed frames or request/response metadata.
- Fixed fixed-length HTTP responses on keep-alive connections. Blink now parses response headers separately from response bodies, so a response with `Content-Length` is not consumed and then waited for a second time.
- Managed HTTP, SSE, and websocket flows continue to work with the hardened protocol layer, including retry, budget, circuit-breaker, request rendering, and SSE parsing behavior.

### colors
- ANSI palette conversion has more stable edge behavior: out-of-range palette indices clamp to the closest valid entry, duplicate palette colors canonicalize predictably, and nearest-color lookup remains stable for off-palette inputs.
- RGB, linear RGB, XYZ, LUV, and UV conversions now have tighter numeric behavior around roundtrips, finite edge cases, and perceptual blending, which makes terminal color and gradient output more predictable.

### contentstore
- Object, tree, and named-object writes are safer under concurrency. Same-hash object writers and same-key named writers now converge on a readable value instead of risking corrupt partial state.
- Named-object overwrites preserve a valid old or new value for readers while the overwrite is in progress. This matters for cache and registry metadata paths that may be read while another process is updating them.
- Store operations now return structured errors for missing objects, permission failures, unwritable namespaces, and failed tree commits, so callers can distinguish absent content from real filesystem failures.
- Temporary files from failed object, named-object, and file saves are cleaned up more reliably, reducing stale cache debris after interrupted writes.

### fixme
- Rule authors now get `Syn.Ast`-based traversal and matching helpers for expressions, patterns, let bindings, parameters, match cases, applications, identifiers, and spans.
- Provider rules use the same typed traversal shape as built-in rules, which removes the old dependency on the removed CST API and makes custom rule providers easier to keep compatible with Syn.
- Rule helpers expose source spans directly from typed Ast handles, so diagnostics and fixes no longer need to recover locations by scanning raw syntax nodes.

### gooey
- Gooey layout and rendering behavior is more stable for nested layouts, clipping, borders, padding, margins, custom commands, unicode text width, and terminal scissor regions.
- Style and config helpers now reject invalid values more consistently and preserve rendering metadata such as text size, z-index, background, borders, and custom render commands.

### http
- HTTP/1 parsing is stricter and more complete. Request and response parsers now validate CRLF placement, request targets, response versions, malformed headers, incomplete status/request lines, ambiguous body framing, and fixed body framing.
- HTTP/1 chunked transfer support was expanded for requests and responses, including chunked body decoding, chunk delimiter validation, chunk-size overflow handling, trailer parsing, and line/header block byte limits.
- HTTP cookie parsing and rendering now return structured errors for invalid names, values, max-age fields, same-site values, content-length fields, and set-cookie payloads.
- HTTP/1 server-sent event parsing can now assemble complete SSE events.
- HTTP/2 frame parsing, serialization, and connection handling were hardened across stream id validation, settings validation, empty settings acknowledgements, frame payload sizes, metadata checks, and invalid serialized payloads.
- HTTP/2 stream-state validation now rejects peer protocol violations such as data before headers, data after stream end, headers after stream end, idle-stream control frames, new streams after GOAWAY, invalid stream ordering, unsupported push promises, and excessive concurrent streams.
- HTTP/2 flow control now tracks split windows, applies remote initial windows, rejects window-update overflow, and validates self-dependent priorities.
- HPACK handling now validates dynamic table size update order, resets reader state correctly, rejects unsupported Huffman strings, encodes custom literal header names, and guards integer overflow.
- WebSocket parsing and serialization now validate masking roles, invalid frame encodings, close payloads, extended payload lengths, parser payload limits, and remaining frame bytes.
- WebSocket message assembly was added so fragmented frames can be reconstructed into complete messages.

### kernel
- Kernel gained a queue surface for FIFO work management in lower-level runtime code.
- Async readiness handling is more complete across pipes, timers, UDP, TCP, processes, deregistration, duplicate registration, mixed event sources, closed sources, and invalid polling limits.
- File and filesystem operations now report more precise typed errors for missing paths, dangling symlinks, invalid file kinds, invalid read/write slices, directory removal failures, copy/rename behavior, and link metadata.
- IO buffer, IoVec, IoSlice, process, environment, monotonic time, and system time paths were tightened so low-level runtime APIs preserve byte ranges, timestamps, and OS error context more reliably.

### krasny
- Krasny is now centered on the streaming formatter path. The old document solver pipeline, old stream-doc intermediary, and old lower2-style naming were removed or renamed so the formatter has one primary architecture.
- Formatter internals were split and renamed around the actual streaming formatter responsibilities, with text helpers, formatter entrypoints, and layout policy surfaces separated more clearly.
- Layout decisions now route through a central policy layer for let right-hand sides, function bodies, applications, infix chains, records, lists, tuples, parenthesized expressions, if conditions, and type separators.
- Application formatting now uses explicit layout roles rather than ambient force flags, making nested applications and function bodies more predictable.
- Layout policy tracing was added and covered by tests, so future formatter policy changes can explain why a node chose inline, hanging, vertical, or block layout.
- The formatter preserves typed-expression parentheses correctly and avoids adding redundant parentheses around ascriptions in match scrutinees, constructor payloads, and parenthesized typed expressions.
- Formatter policy now keeps fitting constructor or-patterns inline, breaks nested constructor patterns when needed, breaks match bodies after multiline constructor patterns, and keeps paired `if` branch parentheses on the right line.
- Local let bindings now respect the configured width, including overflowing single-line bindings and multiline bodies that require `in` placement to stay stable.

### minttea
- Minttea FPS ticks are more regular, which makes time-driven terminal programs less dependent on uneven render-loop timing.
- Renderer, IO loop, text input, cursor, sprite, and program paths were tightened so Elm-style terminal apps behave more consistently with the updated Gooey and TTY rendering layers.

### pkgs-ml
- Registry materialization now returns regular result values for cached and downloaded release trees. Callers can distinguish successful reuse, fresh materialization, and transport/cache failures without relying on exceptions.
- Filesystem registry caches now handle stale config, corrupt cached archives, gzipped archives, missing package documents, and publish/yank routes more predictably.

### propane
- Generators, shrinkers, printers, and property runners have more consistent behavior for common standard-library containers such as lists, arrays, options, results, hash maps, hash sets, queues, deques, and heaps.
- Property failures now retain stable printed values and shrink toward smaller counterexamples more predictably, making failing property tests easier to debug.

### pubgrub
- Pubgrub range operations, term algebra, incompatibility explanations, partial-solution caching, backtracking, and deterministic solution ordering were tightened.
- Solver diagnostics now preserve dependency ranges and no-version explanations more clearly, which helps users understand why a package resolution failed.
- Solver code now avoids removed `List.reverse_append` usage, keeping the package compatible with the cleaned-up standard collection surface.

### std
- `Std.Crypto` now exposes HMAC-SHA256 helpers used by Suri session, CSRF, and LiveView signing paths.
- `Std.Http.Status` gained equality helpers for status comparisons.
- `Std.Test` output now reports per-test timings in the human runner while preserving JSON mode for automation.
- `Vector.concat` and `Vector.extend` support efficient vector concatenation without building temporary lists. `extend` mutates the left vector in place, which is useful in hot parser, formatter, and analysis paths.
- `Std.Collections.HashMap` now uses a SwissTable-style backing table for denser storage and faster lookup, insertion, removal, and traversal while preserving the existing public API.
- Queue, Deque, HashMap, HashSet, Heap, TypedKeyHashMap, iterator, mutable iterator, IO reader/writer, buffered reader, and Unicode helpers now have tighter semantics around order, mutation, borrowed slices, and invalid input.
- `Std.Command.output` remains safe around inherited stdout/stderr pipes and delayed output, preserving idle callbacks and streamed line callbacks for long-running commands.

### suri
- Suri gained a hardened server limits configuration covering request body limits, keep-alive request limits, websocket frame limits, and socket pool startup validation.
- HTTP request handling now validates host headers, request body framing, request ids, method overrides, forwarded client IPs, query parameters, accept headers, CORS configuration, basic auth, static file paths, router matching, websocket routes, and response serialization.
- Suri now returns typed errors for startup configuration, connection handling, protocol handling, static paths, body parsing, session cookie decoding, config environment lookup, CSRF runtime and token unmasking, CORS preflight, liveview protocol, liveview HTML attributes, HTTP/1 validation, and accept quality parsing.
- Sessions and CSRF handling were hardened with HMAC signing, session secret validation, mandatory sessions before CSRF, and structured liveview token/session validation.
- CORS behavior now handles preflight responses as no-content responses and merges `Vary` headers correctly.
- Static file handling now enforces directory roots, dotfile policy, mount boundaries, partial ranges, and no-body response ETag behavior.
- LiveView support now carries typed event payload errors and serializes LiveView errors structurally.
- WebSocket handshakes, frame limits, message flow, and connection writes are validated more carefully.
- App testing helpers, middleware test helpers, and core testing helpers are now exposed as APIs, while the older top-level testing facade was removed.
- Handler exceptions are recovered into structured responses, and fallback unsent response behavior is covered by tests.

### syn
- `Syn.Ast` continued the semantic-view cleanup: source files are now concrete implementation or interface views, and the old empty source-file state was removed from the public shape.
- Ast view handles are opaque at the public boundary and expose typed helpers such as `span`, `width`, `view`, `fold_*`, and count accessors instead of requiring downstream callers to unwrap arbitrary syntax nodes.
- Ast casts now use structured cast results, so callers can distinguish successful typed views, unknown recovery nodes, and true cast errors explicitly.
- Identifier handling was normalized around opaque `Ident` views instead of loose path vectors, making downstream dependency and lint logic less likely to accidentally traverse arbitrary token sequences.
- Module expression and module type views now expose structured bodies, including module declarations, module type declarations, module type constraints, and body items, instead of leaking parser-specific placeholder states.
- Parameter views were normalized into a more semantic shape, covering labeled and optional parameters without splitting optional-default syntax into unrelated variants.
- Pattern views were tightened to remove non-pattern constructs and expose constructor, record, alias, first-class module, and constraint structure more directly.
- Expression and type views were simplified around semantic constructs; parenthesized and syntactic-only wrappers were collapsed where possible so consumers see the expression or type they actually need to analyze.
- Destructuring `let` binding patterns and function parameter spines are parsed more precisely, including the distinction between multiple function parameters and parenthesized constructor patterns.
- Class syntax support was removed from the supported Syn subset, matching the language surface Riot wants to keep formatting and analyzing.
- `Syn.Deps` and other Ast consumers now use the new views and controlled folds, with less list churn in hot dependency-analysis paths.

### swisstable
- SwissTable behavior was tightened for insertion, removal, tombstone reuse, overwrite, clear, resize, entry APIs, collision handling, and iteration.
- Complex record, tuple, variant, and nested keys now behave consistently with the standard hash-map model across long operation sequences.

## 0.0.25 - 2026-04-27

### riot
- `riot publish` now supports `--json`, making publish flows easier to script and inspect in automation.
- `riot add` and `riot rm` now accept multiple package names in one command, so dependency edits can be batched without repeated solver runs.
- `riot update` now accepts one or more package names, allowing targeted dependency updates instead of always refreshing the whole dependency set.
- Riot commands run outside a workspace now print guidance that explains the missing workspace context and points users toward initialization, instead of silently doing nothing or failing without direction.
- CLI behavior is covered by additional `riot-e2e` tests for generated workspaces, package commands, publish flags, and command parsing.
- Generated Riot contributor skill files were refreshed with current CLI flags, module-system notes, testing guidance, and benchmark references.

### planner-build
- The planner now rejects direct use of modules from transitive dependencies. Package code may depend on its own modules and direct dependency roots, which keeps package manifests honest and avoids hidden dependency edges.
- Executable, example, bench, and test entry files now need a top-level `let main ~args = ...` entry point, giving binaries one consistent runtime shape before code generation grows macro support.
- Planning and build error rendering was tightened so innermost diagnostics can explain missing entry points and module-graph violations without extra wrapper noise.
- Workspace package labels now show artifact kind and target context for tests, examples, benches, and multi-architecture builds.

### syn-krasny
- `syn` completed the streaming parser migration, including the replacement CST builder, typed syntax views, broader diagnostic recovery, and parser-backed dependency analysis.
- `krasny` now routes formatting through the typed syntax views and streaming lowerer, improving formatter stability across modules, signatures, local opens, type declarations, attributes, and comments.
- Formatter policy was tightened for pipelines, tuples, function parameters, binding operators, branch layouts, docstrings, phrase separators, and parenthesized expressions.
- Snapshot and fixture coverage was expanded across real files and focused parser/formatter regressions, giving future formatter work a broader safety net.

### std
- Added `Std.Order.is_lt`, `is_lte`, `is_eq`, `is_gte`, and `is_gt`, so callers can work directly with `Order.t` compare results without converting through integers.
- Replaced remaining deprecated list helper usage in downstream packages after the standard collection cleanup.

### blink
- Added a managed HTTP client layer with request/response types, retry policy, connection and rate budgets, circuit breaker state, and telemetry hooks.
- Added managed HTTP, SSE, and WebSocket examples, plus property and unit tests for retry, budget, circuit breaker, request rendering, and SSE parsing behavior.

### postgres
- Added PostgreSQL password authentication support, including cleartext, MD5, and SCRAM-SHA-256 handshake handling.
- Extended protocol parsing and writing for SASL authentication messages while preserving structured PostgreSQL error rendering.

## 0.0.24 - 2026-04-24

### riot
- `riot build` now renders structured planning failures as targeted detail lines and keeps package status labels readable for versions, dev artifacts, and multi-target builds.
- Build output now distinguishes workspace dev artifacts such as tests and benches, including labels like `serde-json (test, aarch64-apple-darwin)` when multiple targets are active.
- `riot init` scaffolds workspace defaults for agents, development config, git hooks, Riot GC config, and starter `Std.Log` setup.

### package-management
- Reworked package, workspace, registry, lockfile, and publish/install/run error paths to carry structured typed errors through Riot internals and render strings only at the CLI edge.
- Improved lock refresh and registry-cache failure reporting so package-management flows preserve actionable error context.

### std
- Collapsed concurrent queue variants into a single lock-free queue surface.
- Migrated comparison APIs toward `Std.Order.t` return values across the stack.

## 0.0.23 - 2026-04-23

### riot
- Defaulted Riot to `OCaml 5.5.0-riot.4` across workspace/toolchain defaults, including generated workspaces, toolchain resolution, and bootstrap constants.
- Updated toolchain tests and release scripts to target `5.5.0-riot.4`.

### windows-toolchain
- Updated vendored OCaml to support MinGW cross toolchains reliably.
- Fixed Windows cross-compilation issues by:
  - setting the Windows API floor to `0x0600` for MinGW targets
  - fixing Win32 runtime/header guards
  - adding missing `errno.h` includes in Win32 Unix shims
  - fixing the `yacc/wstr.c` Windows build path
- This enabled shipping the full `5.5.0-riot.4` toolchain matrix, including the Linux-hosted MinGW targets.

## 0.0.22 - 2026-04-23

### riot
- Fixed generated `riot init --bin` workspaces so the starter package builds, runs, and tests correctly when the package has no library archive.
- Dev-scope planning for no-library packages now carries reachable `src/` helper modules into tests/examples/benches instead of linking a missing package `.cmxa`.
- Added Docker smoke fixtures for mounting the current locally built Riot binary into Arch Linux and Ubuntu containers and validating `riot init`, `riot build`, `riot run`, and `riot test --small`.

## 0.0.21 - 2026-04-23

### riot
- `riot init` now lowercases generated starter file stems, so dotted workspace names with normalized package names build and run consistently on case-sensitive systems.
- `Std.Command.output` no longer hangs when the direct child exits while another process inherited the captured stdout/stderr pipe, preserving idle callbacks and streamed stdout line callbacks for long-running commands.
- Release automation now supports force-republishing explicit Riot binary targets and strips release binaries before upload.

### kernel-toolchain
- Added the Linux implementation path for the new kernel async backend, including epoll/timerfd process, pipe, timer, TCP, and UDP readiness support.
- Added `Kernel.Thread.sleep_ns` for low-level blocking sleeps used by polling paths that must not depend on the actor scheduler.
- Updated Linux sysroot and OCaml cross-toolchain packaging so Riot-built Linux binaries run on common glibc distributions such as Ubuntu and Arch instead of relying on Ubuntu-specific assumptions.
- Published and validated the `5.5.0-riot.3` toolchains with Riot project smoke tests in Linux containers.

### release
- Published Riot 0.0.20 binaries for `aarch64-apple-darwin`, `aarch64-unknown-linux-gnu`, and `x86_64-unknown-linux-gnu` after validating the generated release artifacts and CDN metadata.

## 0.0.20 - 2026-04-22

### riot
- `riot new --lib` and `riot new --bin` now work both inside a workspace and as standalone package scaffolds.
- `riot new` now keeps `[workspace].members` in sync when adding packages into an existing workspace, and repeated `riot new` flows no longer leave generated packages unbuildable or unrunnable.
- Generated `--bin` scaffolds now use the correct runtime entrypoint shape, so newly created binaries run immediately after scaffolding.
- Test suites now receive structured suite context that includes the binaries built for the owning package, so package tests can execute the just-built artifact instead of relying on a globally installed `riot`.
- Riot now has initial `riot-e2e` generated-workspace coverage for:
  - `riot init`
  - `riot new --lib`
  - `riot new --bin`
  - repeated `riot new` flows inside a workspace

### planner-build
- Hardened package-layout validation so target code is rejected during planning, not later during compilation, when it reaches:
  - library-internal modules directly
  - namespaced internal modules like `Pkg__A`
  - another target's private root module
- Refreshed stale plan handling and planner artifact versioning so old cached plans are rebuilt instead of leaking invalid graph state forward.
- Tightened target ownership between `riot-model` and `riot-planner`: declared binaries in a source bucket now suppress autodiscovery in that same bucket, avoiding fake extra target roots during planning.
- Added real-package kernel planner oracles that pin public-root dependency retention before action planning, including:
  - `Kernel__Net__Addr__Unix` keeping `Result`, `System_error`, and `Socket_addr`
  - `Kernel__Process` depending on `Fs` through the public child root instead of leaking down to `Fs__File`
- Expanded planner regression coverage across:
  - `syn` dependency analysis for alias-open and public-root cases
  - planner package layout validation
  - real-kernel module graph and action graph behavior

### std-kernel
- Preserved `IoVec` module casing correctly across `kernel` / `std`, fixing the broken alias/module naming path that was surfacing in kernel builds and planner probes.

## 0.0.19 - 2026-04-22

### riot
- `riot test` now streams human output per test case, passes structured suite context through `--ctx`, exposes built runtime binaries to tests, and treats small tests as a tighter fast path with clearer small/large summaries.
- `riot bench` now records and compares benchmark history, streams benchmark progress/heartbeats, surfaces GC counters and variance, and supports top-level `--warmup`, `--iterations`, and `--compare` controls.
- `riot build` now uses explicit `-p/--package` selection and can build dev artifacts with `--tests`, `--benches`, `--examples`, and `--all`.
- `riot info` now reports workspace/package details more clearly, `riot clean` locks active build lanes before cleaning and reports lock waits, and `riot fmt` stays scoped to workspace sources.
- `riot init` now preserves dotted workspace names while still normalizing starter package names correctly.

### planner-build
- Riot planning now trims unreachable library modules and computes target-private module closures for binaries, tests, examples, and benches, so helper modules can stay target-local instead of being forced into the package library.
- Build caching is more reliable after dependency and manifest changes, including cache invalidation for build dependency path updates and stronger runtime/build dependency hashing.
- Build and test execution gained more stability through lane-lock coordination, generated-root dependency fixes, and tighter small/large test classification.

### std-kernel
- Added `Std.Collections.Proplist` for duplicate-friendly property-list workflows.
- Continued IO/runtime hardening and performance work across `std`, `kernel`, `http`, and `serde-json`, including vectored TLS fallback support and lower-overhead buffer/reader paths.

### docs
- Added an RFD for target-specific module reachability and clarified how that planner model composes with future conditional compilation.

## 0.0.18 - 2026-04-15

### riot
- Expanded Riot build orchestration and toolchain domains (`riot-build`, `riot-planner`, `riot-executor`) with typed command/request surfaces and stronger plan execution contracts.
- Refactored package-orchestration surfaces across `riot-model`, `riot-install`, `riot-run`, and CLI command paths for clearer package and runtime boundaries.
- Fixed planner/build regressions including dependency ordering, explicit-root planning, lazy realization, stale work-plan cache state, and workspace target overrides.
- Hardened artifact and runtime flows in `riot-store` with content-store work, warm-generation dedupe/indexing, and new artifact-store and planner benchmarks.
- Added/updated Riot-side tests and fixtures for planner, build, toolchain, publish, init, and runtime suites.

### raml
- Added and split the compiler stack into dedicated packages (`asm`, `raml-core`, `raml-native`, `raml-js`, `raml-wasm`, `raml-cli`).
- Added new MIR/LIR/WIR/native/JS back-end features: scheduling, legalization, cse/liveness/passes, stack/home allocation, entity/purity/jir simplifications, and runtime import/module support.
- Expanded JS backend lowering and passes (JIR/JST tooling, property/intrinsic/object/records/modules flows) and improved backend-specific test coverage and docs.
- Added artifact-store and codegen improvements for Wasm/native toolchains, including layout and decoding/encoding-path hardening.
- Added many compiler fixtures, snapshots, and regression cases across native, Wasm, and JS tool paths.

### serde
- Added schema-driven codec surface and format packages (`serde-cbor`, `serde-bson`, `serde-yaml`, `serde-urlencoded`) and evolved compact binary codec support.
- Expanded `serde-bin` with new codec variants, native fast paths, and additional benchmarked encode/decode scenarios.
- Reworked serde property/test suites and formatter updates for ongoing API and package migrations.

### kernel-std
- Expanded and standardized the new `std` API surface for common project-wide primitives (`path`, `read_dir`, IO, and core runtime entrypoints), with many call sites moved from legacy kernel usage.
- Refactored runtime ownership: moved tar and gzip engines out of kernel into std, and ported FS facade + core runtime/IO onto the smaller kernel model.
- Added a dedicated `std` events seam (`feat(kernel): add fs events seam for std`) and absorbed actors/runtime pieces into kernel/std ownership.
- Removed legacy runtime debt by dropping `kernel-old` and completing staged migration checkpoints (`kernel,std`, `workspace`, `typ`) across validation and bootstrap.
- Standardized behavior around bootstrap and runtime correctness (`self-host bootstrap`, sandbox path normalization, valid float-literal restoration, and warning/validation cleanups).

### tooling
- Added/updated docs and RFD material for scheduler/executor redesign and parallelism behavior.
- Added the new user-facing Riot skill (`riot-ml`) and its associated references/signature guidance.
- Delivered major package/domain updates for supporting ecosystems (`parquet`, `pretext`, `tty`, `contentstore`, `typ`, `suri`, `propane`, `ignore`) with migration, validation, and hardening work.

### performance
- Added and stabilized benchmark coverage across store/build/planner, serde-bin, and compiler/runtime paths.
- Improved internal performance through cache pruning, planning overhead reduction, module-typing caching, and targeted serialization micro-optimizations.

## 0.0.17 - 2026-04-10

### Fixed

- `riot init` now defaults to the current directory when no target path is provided.
- `riot` now gives clearer guidance when commands are run outside a workspace.
- `riot` surfaces a better hint when a package does not define a runnable binary.
- Repaired the `miniriot` bootstrap dependency graph.

## 0.0.16 - 2026-04-10

### Added

- Added hover request support across the LSP stack and editor integrations.
- Added more detailed `riot check` progress events in the CLI.
- Added a large new slice of experimental multicore runtime and `kernel-new` groundwork.

### Changed

- Workspace package-root handling is more consistent across `riot check`, docs generation, and editor flows.
- A broad round of interface and formatter refreshes landed across core packages.

### Fixed

- Fixed package-root scanning in `riot-check` so per-package runs stay scoped correctly.
- Fixed docs planning so package documentation resolves source roots correctly.
- Fixed several parser and formatter edge cases, including keyword-operator handling in `syn`.

## 0.0.15 - 2026-04-06

### Added

- Added cache-first external source workflows for `riot run` and `riot install`, with explicit `--update` refreshes and repo-name default binaries for remote sources.
- Added detached single-package workspace synthesis so Riot commands can build, run, and install from a package root without an enclosing workspace manifest.
- Added `riot yank` plus exact-version yank support in the `pkgs-ml` registry client.
- Added structured test and benchmark suite timing output in microseconds, including per-test durations, suite lifecycle timing, and escaped JSON payloads that pipe cleanly into tools like `jq`.
- Added aggregated case-level summaries in `riot test` and `riot bench`, including measured test time, slowest tests, and aggregated failed test lists in both human and JSON output.
- Added package admin views in the `pkgs.ml` services and migrated `docs.riot.ml` to the new Astro/Starlight content layout.

### Changed

- `riot test` now rebuilds the familiar human suite output from structured runner events, suppresses zero-match suites for filtered runs, and emits richer machine-readable summary events.
- `riot bench --json` and `riot test --json` now keep build events in the JSON stream instead of dropping compile progress when suites run in structured mode.
- Workspace and package resolution for `riot build`, `riot run`, and `riot install` now prefer the nearest enclosing workspace but can fall back to a single package manifest discovered during the same upward scan.
- RFD metadata and the docs content tree were cleaned up so implemented proposals and user-facing docs stay in sync with the current repo state.

### Fixed

- Fixed stale cached package archives and source materializations so corrupted archives are retried and warm cache flows stay usable.
- Fixed bootstrap/miniriot wrapper drift that was breaking `./bootstrap.py && ./miniriot`.
- Fixed invalid JSON serialization for control characters in suite stdout/stderr.
- Fixed multiple docs and web regressions, including sandbox reruns, mobile navigation, accessibility, and builtin release docs rendering.

## 0.0.12 - 2026-04-06

### Added

- Added `Actors.spawn_pinned`, `Actors.spawn_blocked`, and scheduler support for pinned and blocking actor placement.
- Added detached single-package build support so `riot build` now works from a standalone package root with only a package-level `riot.toml`.
- Added `./scripts/release.sh` to automate version bumps, changelog updates, tagging, and Riot release orchestration.

### Changed

- `riot` CLI workspace resolution now scans upward once, preferring an enclosing workspace manifest and otherwise synthesizing a one-package workspace from the nearest package manifest.
- Riot release automation now supports manifest-aware all-target releases from `./scripts/release/riot.sh all`.

### Docs

- Marked implemented RFDs as `implemented`, including the pinned/blocking actor runtime work.

## 0.0.10 - 2026-04-06

### Added

- Added the new `riot-doc` package for documentation generation, HTML rendering, doctree/source transforms, and workspace interface docs generation.
- Added manual cache GC and generation receipts in `riot-store`, plus workspace operational cache config via `.riot/config.toml`.
- Added published toolchain manifests and `riot toolchain list-available` / `riot toolchains list-available`.
- Added JSON event output for `riot doc`.
- Added rooted snapshot preparation and broader session-driven improvements in `typ`.
- Added richer analytics and registry dashboards across the `pkgs.ml` services.

### Changed

- `riot clean` now performs policy-aware cache cleanup, while successful builds record generation metadata without automatically reclaiming cache entries.
- `riot install` now fails when promotion fails instead of reporting a warning and continuing.
- `riot-doc`, `riot-cli`, and related command surfaces gained structured JSON output improvements.
- Package detail, activity, stats, and mobile layouts on `pkgs.ml` were substantially refined.
- The repository now carries a default `.riot/config.toml`.

### Fixed

- Fixed stale exported build artifacts after cache cleanup so warm rebuilds stay warm after `riot clean`.
- Fixed `pkgs.ml` desktop readme/package section behavior and removed the mobile theme toggle.
- Fixed `riot` agent propagation for access analytics and process stamping in the package services.
- Fixed a dead tail in Suri liveview process handling.
- Improved Neovim Riot diagnostic float rendering.

### Docs

- Reworked the `typ` RFD/spec stack with expanded algorithm notes, examples, diagnostics, and semantic slices.
- Split the cache and operational config RFDs into:
  - `RFD0032 - Riot Cache and GC`
  - `RFD0038 - Riot Workspace Operational Config`
- Refreshed workspace documentation around toolchains and typing internals.
