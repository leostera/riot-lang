# std AGENTS

`std` is the mandatory standard library surface for the rest of the repo.

## Rules

1. Favor small, composable APIs over one-off helpers.
2. Do not leak `Stdlib`, `Unix`, `Sys`, or `Obj` back through this surface unless the boundary is already intentional.
3. Changes here have wide blast radius. Prefer additive evolution and stable signatures.
4. If a utility is only useful for one package, keep it out of `std`.
5. `std` owns its package-provided `riot-fix` rules under `fix/`; keep those diagnostics aligned with the scheduler and `std` ownership rationale.
6. `Std.Test.Cli` owns the portable test-binary contract (`list-tests`, `run-tests`, `--json`, query filtering, and shared selectors such as `--small`, `--large`, and `--flaky`). Keep individual test binaries on that runner instead of inventing local CLIs.
7. Archive and compression APIs should compose with `IO.Reader` and `IO.Writer`. Keep path-based helpers as thin wrappers around the streaming APIs rather than making them the only surface.
8. Binary/text codecs belong under `Std.Encoding`. Keep `Std.Data` focused on structured data formats like JSON, TOML, CSV, XML, and S-expressions.
9. `Std.Test` owns the shared test-binary contract. Per-test callbacks now receive a `Std.Test.ctx`; future snapshot and fixture helpers should extend that context instead of inventing parallel identity plumbing.
10. `Std.Test` case metadata lives on `size` and `reliability`. Keep timeout, retry, and per-case actor isolation behavior inside the shared runner so packages do not grow their own flaky-test or slow-test harnesses. Small tests have a shared 500ms timeout budget by default; anything that legitimately runs longer must be marked `Large`.
11. Keep `Std.Test.FixtureRunner` path-typed. Fixture roots and discovery filters should use `Std.Path.t`, and mixed fixture directories should narrow discovery through the shared `~filter` hook instead of package-local file scanning.
12. Keep snapshot approval routing explicit in shared test helpers. Use fixture-provided `snapshot_path` when a suite already has a package-specific approved filename convention, render JSON snapshots through `Std.Data.Json.to_string_pretty` so approved files stay reviewable, and treat `.expected.new` files as visible review artifacts rather than ignored scratch output.
13. Keep `Std.Crypto.Hasher.Intf` string-first. Callers that need to mix existing digests into a hash state should use `write_hash` rather than passing mutable raw bytes through the public API.
14. Keep recursive filesystem walking in `Std.Fs.Walker`, with an iterator-first surface built around `Iterator.t`. Package-level ignore policy belongs in higher layers, not in `std`.
15. Keep `Std.Fs.Walker` on the cheap `ReadDir` path on the common case. Use directory-entry kind hints first and fall back to metadata only for unknown kinds or symlink-following semantics.
16. Keep `Std.Fs.Walker.FileItem` opaque. The walker hot path should stay string-first internally and only pay `Path.t` construction when a caller explicitly asks for it through accessors such as `FileItem.path`.
17. Keep `Std.Fs.ReadDir` split between raw and validated entry APIs. Hot recursive walkers should prefer raw names plus cheap kind hints; convenience path-typed helpers can stay layered above that.
18. Keep `Std.Regex` tree-shaped and pure. Rendering and compilation should flow through `Kernel.Regex`; higher-level pattern syntaxes should target the regex AST instead of emitting ad hoc kernel pattern strings directly.
19. Keep `Std.Glob` as a syntax-and-translation layer. Parse user glob strings into a glob AST first, then lower that AST into `Std.Regex` before compiling.
20. Keep `Std.Bench.Cli` aligned with `Std.Test.Cli` for machine output. `run-benchmarks --json` should emit one final JSON object that captures per-benchmark results plus the suite summary so higher layers can aggregate without scraping pretty text.
21. `Std.Test` JSON output should include per-test `duration_us`, `size`, `reliability`, and retry/timeout status, plus suite-level `started_at_us`, `completed_at_us`, and `duration_us`. Use `Std.Time.Instant` for these monotonic offsets and durations rather than wall-clock timestamps.
22. `Std.Bench` JSON output should include suite-level `started_at_us`, `completed_at_us`, and `duration_us` alongside the existing per-benchmark statistics, measured from `Std.Time.Instant`.
23. `Std.Test.Cli list-tests --json` and `Std.Bench.Cli list-benchmarks --json` are editor-facing discovery contracts. Keep them machine-readable, selector-aware, and rich enough to drive external UIs without scraping pretty output.
24. Prefer `format Format.[ ... ]` for small diagnostic strings that stitch together primitives in `std`. Keep larger renderers and structured text generators on their own domain-specific builders instead of forcing everything through `Kernel.Format`.
25. Keep `Std.Date` as the civil-date surface and `Std.DateTime` as the calendar datetime surface. Leave `Std.Calendar` as the lower-level Gregorian math helper rather than turning it into the primary application-facing API.
26. Keep `Std.Range` order-based and comparator-carrying. Interval operations should respect stored `Included` / `Excluded` / `Unbounded` bounds without growing a step or enumeration model into the core range type.
27. Keep UDP support datagram-first. `Std.Net.UdpSocket` is the core surface; any `UdpServer` convenience wrapper should preserve packet boundaries and avoid pretending UDP has accept/listener semantics.
28. `Std.Runtime` owns the actor runtime implementation. Keep scheduler, mailbox, timer, and process internals under `std/src/runtime`, and treat the `actors` package as a compatibility facade during the migration.
29. `Std.System` owns raw target-triple parsing. Keep the public triple type on `Std.System.TargetTriple`, expose the current machine as `Std.System.host_triple`, and do not duplicate `arch-vendor-os[-abi]` parsing in higher layers.
30. Keep `Std.Runtime` internal blocking coordination on `Kernel.Sync`, not `Std.Sync`, so public `Std.Sync` can evolve toward actor-level coordination without creating a bootstrap cycle in runtime internals.
31. Keep `Std.Telemetry.emit` lock-free on the hot path. Use actor delivery plus atomic server-state reads instead of guarding every event emission with `Kernel.Sync.Mutex`.
32. Keep public `Std.Sync.Mutex` and `Std.Sync.Condition` actor-backed. Code above the runtime should treat them as process-owned coordination primitives, not as aliases for `Kernel.Sync`.
33. Keep low-level `std` helpers that only need local mutation off `Std.Sync`. Use plain mutable records for local accumulators, and reserve `Std.Sync` for shared actor-facing coordination.
34. Keep `std` test binaries module-scoped. Prefer one suite file per public module or tight nested module surface (for example `std_io_reader_tests.ml`, `std_net_uri_tests.ml`) instead of broad mixed suites that exercise unrelated APIs together.
35. Keep `Std.Global.print*` delegating to `Kernel.IO`'s blocking whole-write stdio helpers. Human-mode CLI rendering calls them per line, so keep that path narrow and avoid iovec-array construction or richer writer abstractions in the hot loop.
36. Keep `Std.Log` actor-friendly and serialized through handler processes. Application-level logging should flow through handlers with explicit drain semantics such as `Log.flush`; reserve `Std.Global.print*` for low-level terminal output where blocking writes are acceptable.
37. Keep `Std.IO.Stdin.open_` handle-backed and brokered through `Runtime.spawn_blocked`, but make `Std.IO.stdin ()` return a `Reader.t` for the common case. Kernel stdin reads stay blocking; `std` owns the broker, `Reader` adaptation, and local buffering through `Std.IO.BufReader` without flipping stdio descriptors into nonblocking mode. `Std.IO.Stdin` itself should stay a thin raw-read handle plus `to_reader`.
38. Keep `Std.IO.stdout ()` and `Std.IO.stderr ()` returning `Writer.t` values layered over the existing stdio modules. That symmetry is the base for future reader/writer composition like `pipe`, so do not force callers back into ad hoc writer wrappers for common stdout/stderr flows.
39. Keep `Std.IO.Reader` buffer-first. Its core source ops are `read`, `read_vectored`, `read_to_end`, `read_to_string`, and `read_exact`, all filling caller-owned `Std.IO.Buffer` or `Std.StringBuilder` destinations instead of returning heap strings or borrowed slices directly. Buffered stdio adapters should still treat empty vectored writes as no-ops so the top-level `Writer` surface behaves consistently across in-memory and stdio-backed sinks.
40. `Std.Test.Cli run-tests --json` is a JSONL lifecycle stream, not a single JSON object. Emit per-suite and per-case progress before the final `TestSummary` line, reserve the top-level `"type"` field for event names, keep per-case metadata under fields like `"test_type"`, and treat timed out tests as ordinary case results so the harness continues running the rest of the suite.
41. Keep `Std.Test` case-level progress structured. Property runners and snapshot helpers should emit shared `Test.Context.progress` values so `Std.Test.Cli` can stream `TestCaseProgress` events uniformly instead of package-specific ad hoc logs.
42. Keep `Std.Test.Runner` responsible for bounded case-level concurrency. Tests within one suite may execute in parallel up to `config.concurrency`, but reporter callbacks and JSON lifecycle emission must stay centralized in the suite owner process so output remains serialized and machine-readable.
43. Code above `std/src/runtime` should use `Std.Exception`, not `Kernel.Exception`. Keep the public backtrace/exception helpers in `Std` so higher layers stay off the kernel boundary.
44. `Std.Command.output` may stream line-oriented stdout callbacks and idle heartbeats while a child process is still running. Keep that idle callback optional and low-overhead when unused so CLI wrappers can expose long silent subprocess work without penalizing ordinary command calls.
45. `Std.Command` child stdout/stderr readers must stay on the blocking lane. Pipe and file reads inside command wrappers should use `Runtime.spawn_blocked`, not normal actors, so child-process I/O does not stall schedulers.
46. Keep off-heap syscall-facing byte storage owned by `kernel`, and re-export it through `Std.IO`. `Std.IO.Buffer` is the default off-heap buffer surface, with `Std.IO.IoBuffer` retained as the exact kernel-shaped API; code above `std` should not reach into `Kernel.IO` directly for parsing or buffering primitives.
47. Keep `Std.IO` explicit about copy boundaries. Off-heap buffers and borrowed views should flow through `Buffer`, `IoSlice`, `IoVec`, and `IoBuffer`; `Reader` and `Writer` should operate on caller-provided `Buffer` / `IoVec` values by default, while heap text building belongs on `Std.StringBuilder`. `Std.IO.BufReader` is the only standard borrowed-slice layer: `buffered`, `peek`, `read_slice`, and `read_line` return borrowed `IoSlice` views that are valid only until the next `BufReader` operation that may refill, consume, or reset the internal buffer. Public `Std.IO` APIs should surface one closed `IO.Error` type and the shared one-parameter `IO.result` alias instead of propagating per-reader or per-writer error type parameters.
48. Keep shared reader and writer internals on the checked kernel I/O surface. Bounds-sensitive calls should use `Result`-returning `Kernel.IO` operations by default, and only drop to `_unchecked` helpers after a local invariant has been established in the hot path.
49. Keep new `Std.Data` parser substrates additive and benchmarked before they replace string-first defaults. View-backed parser experiments such as `JsonStream` should reuse existing value/error models where possible and prove a measurable win before becoming the main public entry point.
50. Keep `Std.Net.Http.Body` as the explicit lazy ownership boundary for HTTP payloads. Request and response helpers may accept owned strings for ergonomics, but parser and transport code should prefer carrying `IoSlice`-backed bodies until a caller explicitly asks for `Body.to_string`.

## Validate

`timeout 30 riot build std`
`timeout 30 riot test -p std`
`timeout 30 riot bench -p std`
