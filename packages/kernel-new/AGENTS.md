# kernel-new AGENTS

`kernel-new` is Riot's new platform abstraction layer. It should stay narrow: portable public contracts over platform-specific implementations, with just enough foundational types to let `std` build on top of it.

## Rules

1. Keep public APIs portable. Platform-specific modules should live underneath public handles such as `Fs.File` or `Async`.
2. Do not expose `Unix.file_descr` or similar platform-native handles in the public surface.
3. Keep `Reader` and `Writer` out of `kernel-new`; those stay in `std`.
4. Put all native code in `native/`, and keep it Riot-authored.
5. Do not depend on `stdlib` or `unix` in `kernel-new` implementation code. If a compiler-owned type such as `string` or `option` must be referenced, keep that dependency explicit and minimal.
6. Prefer explicit error variants over exception-driven APIs. Native stubs should return `Result.t` with canonical `Error.t` codes instead of surfacing Unix exceptions into OCaml.
7. Build new native stubs mechanically and narrowly. Avoid monolithic helpers that smuggle policy into C.
8. Tests belong in `tests/` and benchmarks in `bench/`, using `std` as a dev-dependency.
9. Start with the Unix backend, but keep the directory structure ready for additional backends under each public module.

## Validate

`timeout 30 riot build kernel-new`
`timeout 180 riot test -p kernel-new`
`timeout 180 riot bench -p kernel-new --json`
