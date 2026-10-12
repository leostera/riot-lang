# Initial AArch64 value and call ABI

**Status:** approved for the first owned-native implementation on 2026-10-12.

This records the user's selected initial representation policy and target. It
is intentionally smaller than OxCaml's layout lattice or rustc's full target
ABI classification. It does not select an allocator/GC or freeze every object
field layout.

## Target and object ABI

- **Target platform:** AArch64 macOS on Apple Silicon (`aarch64-apple-darwin`).
- **Object format:** Mach-O.
- **Assembler/linker driver:** Clang, using its Apple AArch64 target and
  system linker. The native compiler emits textual AArch64 assembly; it does
  not implement an object writer or linker.
- **Foreign/runtime ABI:** the Apple AArch64 C ABI for every call to a C
  runtime or Host helper, using the exact prototype in `riotc/runtime.h`.
  Clang's selected SDK/deployment target supplies target details; the ABI
  probe used `arm64-apple-macosx26.0` and SDK 27.0.

## Native value representations

The checked source type is retained through NativeIR. Representation lowering
then classifies each supported value as follows:

| Riot source value | Initial native representation |
| --- | --- |
| `unit` | Zero-sized: no argument slot and no return register. Keep the typed value in source/NativeIR semantics; erase it only during representation lowering. |
| `i8`, `i16`, `i32`, `i64` | Unboxed signed scalars, preserving width. Calls use the corresponding C fixed-width scalar type; Apple AArch64 call boundaries sign-extend `i8`/`i16` to 32 bits. Arithmetic still wraps at the source width. |
| `u8`, `u16`, `u32`, `u64` | Unboxed unsigned scalars, preserving width. Calls use the corresponding C fixed-width scalar type; Apple AArch64 call boundaries zero-extend `u8`/`u16` to 32 bits. |
| `bool` | Unboxed scalar mapped to C `_Bool`, canonical `0` or `1`. |
| String, tuple, record, variant, closure, and other composites | One pointer-shaped native value (C ABI `void *` shape at calls). The initial convention does not flatten composites into multiple argument or result registers. Their concrete heap layout/allocation policy is separate from this call-shape decision. |

A type variable must have a concrete instantiated representation before a
native function body is emitted. Generalized functions are specialized for
their concrete checked instantiations (monomorphized); an unresolved or
unsupported representation is rejected before Codegen, not silently treated
as a scalar, pointer, or `RtValue`.

## Internal Riot function convention

Riot source functions are unary/curried at the LambdaIR function boundary. An
instantiated native function uses the typed signature:

```text
native_fn(environment: pointer, argument?: ValueRep) -> ResultRep?
```

- The hidden environment pointer is the first machine argument for both direct
  and closure calls. A top-level direct call passes null. A closure call loads
  its code pointer and captured-environment pointer from the pointer-shaped
  native closure value, then calls the code pointer with that environment.
- There is at most one source argument at this boundary. If its checked type
  is `unit`, omit the machine argument. Thus `main : unit -> i32` is called
  with a null environment and no source argument.
- A scalar or composite-pointer result is returned directly in the target's
  normal result register (`w0` for up-to-32-bit scalar values, `x0` for 64-bit
  scalars/pointers). Apple AArch64 extends narrow signed/unsigned result values
  according to their source signedness. A `unit` result has no machine result.
- Native calls use the Apple AArch64 C procedure-call classification for these
  scalar/pointer signatures, with no separate private register convention. The
  hidden environment pointer occupies the first general-purpose argument slot
  (`x0`); a present source argument occupies the next suitable slot (`w1` or
  `x1`). Direct and indirect calls use the same convention. No aggregate-
  by-value convention or `RtValue`-shaped internal signature is introduced.

The intended whole-program OS entry point is a small C-ABI-compatible wrapper.
It receives `argc`/`argv`, initializes the C runtime's process/runtime-directory
state, initializes Riot module globals, invokes the mangled native Riot `main`
with a null environment and erased unit argument, and returns its `i32` result
as the process status. The source binding named `main` is not emitted as the
Mach-O entry symbol. This complete adapter is not implemented yet: the current
Codegen test harness emits a synthetic `_main` for one selected function and
does not initialize runtime state or module globals.

## Implemented LinearIR subset (2026-10-12)

The first MachineIR-to-LinearIR implementation keeps this approved signature
shape while deliberately using an all-spill allocator:

- Each non-unit MachineIR SSA value in the current `i32`/`bool` slice receives
  a distinct unboxed four-byte stack slot. `unit` receives no slot. This is a
  spill-all strategy, not a boxed `RtValue` representation or a claim about
  eventual aggregate memory layout.
- Spill offsets are four-byte aligned from the allocated stack-frame `sp`;
  the local frame size is rounded up to 16 bytes. Current Codegen saves/restores
  `x29`/`x30`, allocates that frame, and emits the epilogue at returns.
- Reloaded values and selected operations use caller-saved `w9`/`w10`/`w11`.
  The initial lowering allocates no callee-saved registers. Values are
  reloaded before use and written back after definitions, so control-flow
  edges copy block arguments explicitly through `w9`.
- A direct call to a top-level function or module initializer sets hidden
  environment `x0` to null, places its optional non-unit scalar argument in
  `w1`, emits the logical direct-call pseudo, and spills a scalar result from
  `w0`. `unit` arguments and results emit no data move. A function entry's
  optional scalar argument is spilled from `w1`; an entry `unit` argument is
  erased.
- The current LIR verifier checks slot ownership/uniqueness, frame alignment
  and bounds, physical scratch-register assignment, terminated block
  structure, and all branch labels. Unsupported closure-environment calls
  and Host helper calls fail before Codegen; Host marshalling to/from `RtValue`
  is not implemented by this slice.
- `emit.ml` lowers this verified slice to AArch64 instructions. A pattern
  failure calls the declared no-argument `rt_pattern_fail` helper from
  `runtime.h`/`runtime.c` under the Apple C ABI, then emits a `brk` fallback;
  other current trap pseudos use distinct `brk` immediates. This does not yet
  implement division/remainder lowering or general Host marshalling.

This conservative policy validates the first ABI lowering but is not the full
native backend. Narrow integer widths, pointers/composites, closures, and Host
helpers remain outside the current LIR slice. The selected-function harness
assembles and links with Clang for the approved target, but it is not the
whole-module entry adapter described above.

The native closure code-pointer convention is distinct from the C runtime's
legacy `RtFunction` type. Native closure values must not be passed to
`rt_apply` or stored as `RtFunction` callbacks unless a deliberate adapter is
implemented. C Host callbacks continue to use their declared `RtValue`
callback ABI at the helper boundary.

## Runtime and Host helper boundary

Calls from generated ARM code into `riotc/runtime.c` use the Apple AArch64 C
ABI, not the internal Riot function convention. A helper call must match its
C prototype and symbol in `riotc/runtime.h`; codegen must not infer helper
signatures from binding names alone.

`RtValue` is used only where a declared runtime/Host helper requires the
existing dynamic representation, and for the explicit conversion at that
boundary. It is not the default representation for NativeIR values or native
function calls. The runtime's current C layout is:

- `tag`: 32-bit C `int` at byte offset 0;
- four bytes of ABI padding;
- the 8-byte payload union at byte offset 8;
- total size 16 bytes, alignment 8.

When passed or returned by value on the probed Apple AArch64 C ABI, an
`RtValue` occupies a pair of general-purpose registers. A function declared
`RtValue f(RtValue environment, RtValue argument)` receives the environment in
`x0`/`x1`, the argument in `x2`/`x3`, and returns the `RtValue` in `x0`/`x1`.
This is a C ABI fact for the selected target, not a rule for native Riot
functions. The implementation deliberately uses the same Apple C register and
narrow-integer rules for the compatible native scalar/pointer signatures, but
native values remain typed rather than being represented as `RtValue`.

The marshalling boundary is explicit and type-directed. A native `i8`/`i16`
passed to a helper expecting `RtValue` is sign-extended into the `int64_t`
payload with the `RT_INT` tag; `u8`/`u16` are zero-extended. `i32`/`u32` and
signed `i64` values fit in that signed payload. The existing `RtValue` API has
no lossless typed `u64` representation for the full `u64` range; do not marshal
`u64` through `RT_INT` unless a helper explicitly defines a bit-preserving
contract. Otherwise add a typed helper or reject that boundary before Codegen.
A boolean uses `RT_BOOL` and canonical 0/1; unit uses `RT_UNIT`. Strings and
other composites use the declared runtime constructors/accessors or an
explicit adapter; a native pointer is not implicitly an `RtValue`. Helper
results are decoded according to the expected checked Riot type. Dynamic
closures returned by a helper remain a runtime boundary value until a
compatible adapter or subsequent declared helper call handles them.

C helper prototypes using `int`, `size_t`, pointers, or other C types use
those declared types directly under the same C ABI. Host operations retain the
behavior in `riotc/runtime.c`; the owned compiler adds no generated-C fallback.

## Reproducible `RtValue` ABI probe

[`rtvalue_abi_probe.c`](rtvalue_abi_probe.c) uses only C declarations and
static layout assertions. It checks the C runtime boundary; it does not
implement or select the native Riot ABI.

On the recorded host it was inspected with:

```sh
probe_dir="$(mktemp -d)"
clang --version
clang -target arm64-apple-macosx26.0 -O0 -S -I riotc \
  riotc/backend/native/aarch64/rtvalue_abi_probe.c \
  -o "$probe_dir/rtvalue_abi_probe.s"
clang -target arm64-apple-macosx26.0 -Xclang -fdump-record-layouts \
  -fsyntax-only -I riotc riotc/backend/native/aarch64/rtvalue_abi_probe.c
```

Apple Clang 21.0.0 reported the 16-byte/8-byte-aligned layout above. Its
assembly shows each by-value `RtValue` uses two consecutive GPRs; a callback
call remaps the function pointer and both argument pairs according to the C
prototype. The probe also provides a place to validate typed narrow scalar
arguments/results and the hidden-environment-plus-one-argument shape before
LinearIR depends on those rules. `riotc/runtime.c` also asserts that the
current length-aware string change preserves the legacy `RtValue` size,
alignment, and payload offset.

## Not decided by this ABI record

- The exact in-memory field layout of every tuple, record, and variant, and
  the allocator/GC policy for native aggregates and closures.
- A general adapter from typed native closures to the dynamic C `RtFunction`
  callback ABI; no such conversion is implicit.
- Native trap sequences and diagnostics for each `NativeTrapKind`.
- ABI support for source types or aggregate shapes not yet implemented by the
  AArch64 selector. MachineIR retains typed `unit`, `i32`, and `bool`, but the
  current physical LinearIR slice only gives data slots/registers to `i32` and
  `bool`; this does not claim that other types have Codegen yet.

## References

- [Apple: Writing ARM64 code for Apple platforms](https://developer.apple.com/documentation/xcode/writing-arm64-code-for-apple-platforms#Pass-arguments-to-functions-correctly), especially its caller-extension rule for arguments narrower than 32 bits.
- [rustc 1.99.0 AArch64 ABI classifier](https://github.com/rust-lang/rust/blob/1.99.0/compiler/rustc_target/src/callconv/aarch64.rs#L78), used as implementation corroboration for Darwin narrow integer argument and return extension.

Any change to the approved value classes, environment/argument convention,
unit erasure, or helper ABI requires an explicit design update before the
corresponding lowering is changed.
