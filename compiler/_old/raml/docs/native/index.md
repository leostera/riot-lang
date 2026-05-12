# Raml Native Backend Manual

This directory is the step-1 manual for `compiler/raml`'s native backend work.

It is a source-driven snapshot of what the existing OCaml native backend does
today in:

- `vendor/ocaml/lambda`
- `vendor/ocaml/asmcomp`

The point is not to treat OCaml's backend as the design for `raml`.

The point is to make the seams, invariants, and runtime assumptions explicit
before we replace them with a Riot package that targets `zort`.

## How To Read This Manual

Start here:

- [pipeline.md](./pipeline.md)
  the end-to-end native compilation path and the major IR boundaries
- [lambda.md](./lambda.md)
  typedtree-to-Lambda translation, the Lambda IR, matching, simplification,
  TMC, and recursive-value lowering
- [cmm.md](./cmm.md)
  Cmm, Cmm generation, object layout helpers, constants, closures, and runtime
  assumptions
- [mach.md](./mach.md)
  selection, polling, Mach, register allocation, linearization, stack frames,
  scheduling, and emission order
- [targets.md](./targets.md)
  the target-specific backend surface and the linker/packager side
- [strategy.md](./strategy.md)
  what `raml` should keep from `asmcomp`, and why the first native path should
  emit direct assembly instead of targeting LLVM or Zig
- [zort-compatibility.md](./zort-compatibility.md)
  what this backend shape implies for a `zort`-targeted rewrite

## Scope

This manual covers:

- the native path that starts in `driver/optcompile.ml`
- the `Lambda.program` shape produced by `vendor/ocaml/lambda`
- the `asmcomp` pipeline from Cmm generation to object-file emission
- the target hooks that shape code generation
- the runtime contracts that are visible from these layers

This manual does not deeply cover:

- the typechecker
- the bytecode backend
- the `middle_end/` implementation details between simplified Lambda and the
  `Clambda.with_constants` input expected by `asmcomp`
- the OCaml runtime C and assembly implementation outside what the backend
  visibly assumes

That middle-end gap matters. The native backend is not simply
`Lambda -> Cmm -> Mach`.

The actual seam is:

- typedtree
- Lambda translation
- Lambda simplification
- middle end
- Clambda/closed Lambda with constants
- Cmm
- Mach
- Linear
- target emission and linking

## What This Manual Owns

These docs are meant to keep ownership boundaries explicit.

- `pipeline.md`
  owns the stage graph and the handoff points
- `lambda.md`
  owns the frontend-side backend boundary: Lambda IR and the passes that happen
  before the middle end
- `cmm.md`
  owns the machine-independent native IR and the backend-visible object model
- `mach.md`
  owns the machine-dependent but still target-generic optimization pipeline
- `targets.md`
  owns the target plugin surface and the artifact/linking side
- `strategy.md`
  owns the first concrete native-backend recommendation for `raml`
- `zort-compatibility.md`
  owns the compatibility implications for the `zort` runtime target

If two docs start trying to own the same seam, one of them is too wide.

## Current Big Picture

The current OCaml native backend is organized around a few strong ideas:

- frontend translation does a lot of semantic lowering early
- Lambda is already rich in runtime-facing primitives and control constructs
- pattern matching and generic recursive values are lowered before the middle
  end
- Cmm is where the runtime object model becomes explicit
- Mach and Linear form a classical target-aware backend pipeline
- target selection is not abstracted by one interface alone; it is spread
  across architecture description, calling convention, selection, reload,
  scheduling, stack-frame analysis, and emission

For `raml`, that means the job is not "rewrite one code generator file".

The job is to replace a whole stack of cooperating layers while keeping the
seams honest.

## Primary Source Anchors

The main source anchors used for this pass were:

- `vendor/ocaml/driver/optcompile.ml`
- `vendor/ocaml/lambda/lambda.mli`
- `vendor/ocaml/lambda/translcore.ml`
- `vendor/ocaml/lambda/translmod.ml`
- `vendor/ocaml/lambda/matching.mli`
- `vendor/ocaml/lambda/simplif.ml`
- `vendor/ocaml/lambda/tmc.mli`
- `vendor/ocaml/lambda/value_rec_compiler.ml`
- `vendor/ocaml/asmcomp/asmgen.ml`
- `vendor/ocaml/asmcomp/cmm.mli`
- `vendor/ocaml/asmcomp/cmmgen.ml`
- `vendor/ocaml/asmcomp/cmm_helpers.mli`
- `vendor/ocaml/asmcomp/selectgen.mli`
- `vendor/ocaml/asmcomp/mach.mli`
- `vendor/ocaml/asmcomp/polling.ml`
- `vendor/ocaml/asmcomp/stackframegen.ml`
- `vendor/ocaml/asmcomp/linear.mli`
- `vendor/ocaml/asmcomp/dune`
- `vendor/ocaml/asmcomp/asmlink.mli`
- `vendor/ocaml/asmcomp/asmlibrarian.mli`
- `vendor/ocaml/asmcomp/asmpackager.mli`
- `zort/spec/compiler-runtime-integration.md`
- `zort/BACKLOG.md`
