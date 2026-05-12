# Raml Target Backend Surface

This document records how `asmcomp` splits target-specific work.

The main lesson is that "the target backend" is not one module.
It is a coordinated set of modules selected per architecture.

## 1. Target Selection In `asmcomp/dune`

`vendor/ocaml/asmcomp/dune` copies the target implementation chosen by
`ARCH` into the active backend module set:

- `arch.ml`
- `CSE.ml`
- `proc.ml`
- `reload.ml`
- `scheduling.ml`
- `selection.ml`
- `stackframe.ml`

It then generates `emit.ml` from the target's `emit.mlp`.

Supported target directories in this tree are:

- `amd64`
- `arm64`
- `power`
- `riscv`
- `s390x`

This is a simple but very concrete plugin mechanism.

## 2. What Each Target File Owns

### `arch`

`arch.mli` exposes:

- addressing modes
- target-specific operations
- data sizes and endianness
- alignment capabilities
- immediate predicates
- printing of addressing modes and specific operations
- command-line options

This is where the backend answers questions like:

- what addressing modes exist
- what special instructions exist
- which immediates fit
- whether unaligned access is allowed

### `proc`

`proc.mli` owns:

- physical register classes and counts
- register names
- calling conventions
- external-call conventions
- tailcall argument limits
- destruction sets for operations
- assembler invocation
- per-target initialization

This is the target's ABI hub.

In the `arm64` implementation, for example, `proc.ml` explicitly defines:

- caller-save and callee-save register maps
- the allocation pointer and domain-state registers
- OCaml calling convention
- C calling convention
- `Domainstate` argument passing for overflow arguments
- the max arguments that still preserve tailcalls

### `selection`

Target `selection.ml` specializes instruction selection.

The `arm64` selector shows the usual kinds of work:

- recognize legal immediates
- choose direct addressing modes
- fold shifts into arithmetic
- pick special multiply-add or sign-extension instructions
- special-case atomic loads
- pick inline primitive implementations such as `sqrt` or byte swaps

### `reload`

Target reloaders make illegal stack/register combinations legal after register
allocation.

The `arm64` reload pass, for example, treats `Imove32` specially because the
argument and result cannot both be stack locations.

### `scheduling`

Scheduling is target-selected because latency and hazard information are
target-specific even when the scheduling pass shape is shared.

### `stackframe`

Target stack-frame modules usually inherit `stackframegen` and provide target
facts such as trap-handler size.

### `emit`

Target emitters finally serialize:

- function bodies
- data sections
- frame descriptors
- debug information
- assembler directives

`emit.mlp` is generated into the active `emit.ml`.

## 3. Shared Target Helpers

Not all target logic lives inside one target directory.

The tree also includes:

- `branch_relaxation.mli`
- `branch_relaxation_intf.mli`
- `x86_ast.mli`
- `x86_dsl.*`
- `x86_gas.*`
- `x86_masm.*`
- `x86_proc.*`

`branch_relaxation` is especially important on targets whose conditional branch
encodings have limited displacement.

The generic relaxer asks the target for:

- branch classes
- max displacements
- instruction sizes
- relaxed forms of allocation, poll, bounds-check, and specific ops

So even very late "fix up branch distance" logic has a formal target interface.

## 4. Artifact And Linker Side

The target backend does not end at textual emission.

`asmcomp` also owns:

- `asmlink`
  link `.cmx/.o` into executables or shared objects
- `asmlibrarian`
  build archives
- `asmpackager`
  package several units as one packed compilation unit

This matters because target choice affects:

- object format
- assembler and linker invocation
- runtime helper symbol availability
- package/link compatibility

`asmpackager` is particularly revealing because it re-enters the normal native
pipeline rather than bypassing it.

## 5. One Concrete Example: `arm64`

The active checked-in `arch.mli` in this tree comes from `arm64`, and the
`arm64` backend shows the whole shape clearly.

It defines:

- AArch64-specific addressing modes
- fused arithmetic ops such as shift-add and multiply-add
- bounds-check far variants
- byte-swap and sign-extension specific ops
- an ABI with explicit domain-state argument overflow
- a trap-handler stack size of 16 bytes

The main point is not that `raml` should copy `arm64`.

The point is that a "target backend" already means:

- ISA rules
- calling convention
- runtime helper conventions
- frame policy
- emitter syntax and object generation

## 6. What This Implies For `raml`

The current target split suggests a few rules.

### Lock one target first

`zort/BACKLOG.md` already says this explicitly, and `asmcomp` agrees with that
direction.

Trying to define a fully abstract target-independent compatibility ABI before
one working target exists will slow the rewrite down.

### Keep target responsibilities separate

Do not hide:

- ABI layout
- special instruction selection
- reload legality
- frame policy
- emission syntax

inside one oversized "backend" module.

For the current `aarch64-apple-darwin` emitter, this also means target text
conventions must include legal assembler symbol spelling.
Do not emit raw operator names such as `+`, `&&`, or `<` directly as Mach-O
symbols; mangle punctuation-bearing names into assembler-safe spellings in the
emitter.

### Keep late target fixups explicit

Branch relaxation, frame-descriptor recording, and assembler/binary-backend
handoff are distinct phases today for good reasons.

### Preserve a target-independent core as long as possible

`asmcomp` keeps a lot of optimization work target-generic until quite late.
`raml` should likely do the same even if the IR names change.
