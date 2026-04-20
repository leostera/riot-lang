# kernel-new AGENTS

`kernel-new` is Riot's new platform abstraction layer. It should stay narrow: portable public contracts over platform-specific implementations, with just enough foundational types to let `std` build on top of it.

## Rules

1. Keep public APIs portable. Platform-specific modules should live underneath public handles such as `Fs.File` or `Async`.
2. Do not expose `Unix.file_descr` or similar platform-native handles in the public surface.
3. Keep `Reader` and `Writer` out of `kernel-new`; those stay in `std`.
4. Put all native code in `native/`, and keep it Riot-authored.
5. Do not depend on `stdlib` or `unix` in `kernel-new` implementation code. If a compiler-owned type such as `string` or `option` must be referenced, keep that dependency explicit and minimal.
6. Prefer explicit error variants over exception-driven APIs. Each public module should own a small typed `error` type, and `Kernel_new.Error` should wrap those typed errors at package boundaries. Native stubs should return `Result.t` with canonical `SystemError.t` codes instead of surfacing platform exceptions into OCaml.
7. Keep numeric system-error code bridges internal. `Kernel_new.SystemError` is the public symbolic contract; raw code values belong only in package-internal native plumbing.
8. Build new native stubs mechanically and narrowly. Avoid monolithic helpers that smuggle policy into C.
9. Treat OCaml heap pointers as unstable across `caml_enter_blocking_section()`. Blocking native calls must not retain `String_val`, `Bytes_val`, or similar heap-backed pointers; copy them first or move the kernel-facing buffer type to owned off-heap storage.
10. Keep `Kernel.IO.IoVec` self-contained and off-heap. Segments are owned byte slices for syscall-facing `readv`/`writev`; `from_string`/`from_bytes` copy into that storage rather than aliasing OCaml heap buffers.
11. Keep `Kernel.IO.IoSlice`, `IoVec`, and `Buffer` checked-by-default. Range-sensitive operations should return `Result.t`; reserve `_unchecked` helpers for hot paths that have already established bounds once.
12. Make zero-copy slicing the default within kernel I/O. `IoSlice.sub`, `shift`, and `split_at` should produce shared off-heap views, while `from_*` / `to_*` names remain the explicit copy boundaries into or out of OCaml heap `string` / `bytes`.
13. If a capability has a real async or readiness-driven path, do not add a blocking helper for it in `kernel-new`. Fast metadata/sysinfo calls are fine when they are inherently synchronous.
14. Tests belong in `tests/` and benchmarks in `bench/`, using `std` as a dev-dependency.
15. Start with the Unix backend, but keep the directory structure ready for additional backends under each public module.
16. Source-layout and code-hygiene rules do not belong in unit tests. Encode them in docs, review guidance, or separate tooling instead.
17. Do not add `Backend.ml` shim modules. Prefer local backend files such as `env/unix.ml`; if the current planner cannot support a deeper nested split yet, keep the implementation in the public module rather than introducing a backend shim.
18. Keep `Kernel.Random.Source` entropy-only. OS randomness belongs here; PRNG policy, distributions, and sampling combinators belong in `std`.

## Validate

`timeout 30 riot build kernel-new`
`timeout 180 riot test -p kernel-new`
`timeout 180 riot bench -p kernel-new --json`
