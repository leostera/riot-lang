# zort runtime specs

This directory is the source-inspection spec for [`vendor/ocaml/runtime`](../../vendor/ocaml/runtime).
It records observable behavior of the OCaml C runtime so zort can decide what to mirror,
what to shim, and what to drop.

## Scope

- These notes describe what the OCaml runtime does today, as implemented in C and runtime assembly.
- They focus on behavior that is externally visible to generated code, the FFI, the GC, and runtime services.
- They do not assume zort must preserve every detail.
- They do assume that any intentional divergence should be explicit and justified against this baseline.

## Reading model

- `Core runtime contract`: value representation, allocation, GC, roots, exceptions, callbacks, and startup.
- `Runtime-hosted primitives`: channels, syscalls, lexing/parsing engines, and other non-core services that happen to live in the runtime tree.
- `Support-only implementation`: skip lists, hash helpers, digest/compression helpers, and architecture assembly glue are only documented where they change visible behavior.

## Spec map

- [`constructors.md`](./constructors.md): value model, tags, immediates, closures, lazy blocks, and constructor-level invariants.
- [`effects-and-continuations.md`](./effects-and-continuations.md): fiber stacks, continuation linearity, `perform`/`reperform`/`resume`, callback boundaries, and effect-visible backtraces.
- [`allocator-policy.md`](./allocator-policy.md): heap allocation paths, string/block initialization, external memory, and static allocator services.
- [`primitive-boundary-and-native-dynlink.md`](./primitive-boundary-and-native-dynlink.md): primitive tables, named values, native plugin loading, and metadata registration.
- [`compiler-runtime-integration.md`](./compiler-runtime-integration.md): native compiler/runtime seam, startup and metadata expectations, and the minimum compatibility surface needed to link compiler-emitted code against zort.
- [`gc-roots.md`](./gc-roots.md): local/global roots, root scanning, remembered sets, write barriers, and atomic field helpers.
- [`gc-strategy.md`](./gc-strategy.md): minor/major GC structure, promotion, phase machine, and domain interaction.
- [`gc-control-and-stats.md`](./gc-control-and-stats.md): `Gc.get/set/stat`, explicit collection operations, runtime parameter rendering, and tweak knobs.
- [`signals-and-stack-overflow.md`](./signals-and-stack-overflow.md): alternate signal stacks, native poll/raise paths, and platform stack-overflow handling.
- [`platform-capabilities.md`](./platform-capabilities.md): target capabilities, build-time capability reduction, runtime permissions, and the host-substrate split.
- [`sync-primitives.md`](./sync-primitives.md): runtime-owned mutex/condition behavior and OS-error mapping.
- [`string-semantics.md`](./string-semantics.md): strings, bytes, arrays, float arrays, and element access rules.
- [`numeric-primitives.md`](./numeric-primitives.md): integer/float parsing and formatting, string/bytes primitives, and array/float-array operations.
- [`custom-blocks.md`](./custom-blocks.md): custom blocks, bigarrays, boxed numerics, and out-of-heap resources.
- [`comparison-hashing.md`](./comparison-hashing.md): structural compare, generic hash, NaN rules, abstract/custom behavior, and variant hashing.
- [`marshaling-and-code-loading.md`](./marshaling-and-code-loading.md): `Marshal`, code fragments, closure serialization rules, and code identity.
- [`exceptions-callbacks-and-backtraces.md`](./exceptions-callbacks-and-backtraces.md): exception buckets, callback semantics, effect boundary handling, and backtraces.
- [`weak-finalizers-and-memprof.md`](./weak-finalizers-and-memprof.md): weak arrays, ephemerons, finalizers, and memprof tracking.
- [`startup-domains-and-signals.md`](./startup-domains-and-signals.md): startup/shutdown, domains, stop-the-world behavior, signals, and process lifecycle.
- [`benchmark-depth.md`](./benchmark-depth.md): retained filename; now documents observability, GC/debug controls, and runtime event surfaces relevant to measurement.
- [`runtime-hosted-primitives.md`](./runtime-hosted-primitives.md): channels, basic syscalls, and lexing/parsing engines that live in the runtime tree.

## Executable protocol models

- [`gc/`](./gc/README.md): bounded TLA+ models for generational GC protocol behavior.
- [`effects/`](./effects/README.md): bounded TLA+ models for continuation and callback-boundary behavior.
- [`domains/`](./domains/README.md): bounded TLA+ models for domain-lane ownership and runnable transfer.
- [`runtime/`](./runtime/README.md): bounded TLA+ models for pending-action draining and other cross-cutting runtime-service protocols.

## Coverage notes

- The specs are based on source inspection of `vendor/ocaml/runtime/*.c` and `vendor/ocaml/runtime/caml/*.h`.
- Architecture-specific assembly files are treated as ABI glue unless they surface directly in documented behavior such as callbacks, effect stack switching, code fragments, or startup.
- Helper files such as `md5.c`, `blake2.c`, `zstd.c`, `prng.c`, `platform.c`, `unix.c`, and `win32.c` are folded into the nearest behavior spec when they affect visible semantics, and otherwise treated as implementation support rather than standalone contracts.
- These docs are intended to be rewritten again when zort makes explicit design departures.
