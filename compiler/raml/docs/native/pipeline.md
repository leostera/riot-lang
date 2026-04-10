# Raml Native Pipeline

This document records the current end-to-end shape of OCaml's native pipeline
as seen from `vendor/ocaml/lambda` and `vendor/ocaml/asmcomp`.

It is the shortest map from "typed OCaml source" to "linked native object".

## 1. Driver Entry Points

Native compilation enters through `vendor/ocaml/driver/optcompile.ml`.

There are two relevant native paths:

### Flambda path

- `Translmod.transl_implementation_flambda`
  builds a `Lambda.program`
- the program's `code` is dumped as raw Lambda if requested
- `Simplif.simplify_lambda` rewrites that code
- `Asmgen.compile_implementation` is called with
  `Flambda_middle_end.lambda_to_clambda`

### Closure path

- `Translmod.transl_store_implementation`
  builds a `Lambda.program`
- the full program is dumped as raw Lambda if requested
- only `program.code` is simplified
- `Asmgen.compile_implementation` is called with
  `Closure_middle_end.lambda_to_clambda`

The step-1 source set does not include a deep analysis of `middle_end/`.
That middle-end seam is still important because `asmcomp` does not consume
frontend Lambda directly. It consumes the output of a middle end:

- `type middle_end = ... -> Lambda.program -> Clambda.with_constants`

## 2. Stage Graph

In the current native pipeline, the stages are:

1. typedtree
2. typedtree-to-Lambda translation in `lambda/`
3. Lambda simplification and normalization
4. middle end
5. `Clambda.with_constants`
6. Cmm generation in `asmcomp/cmmgen.ml`
7. Cmm-to-Mach lowering and Mach passes in `asmcomp/asmgen.ml`
8. Mach-to-Linear lowering
9. target scheduling
10. target emission
11. external assembler or internal binary backend
12. linking, packaging, or archive construction

The useful design fact is that the pipeline has several hard seams already.
`raml` does not need one giant monolithic rewrite. It needs replacement seams
that stay equally explicit.

## 3. What `Lambda.program` Carries

The frontend-to-middle-end handoff is not just "some Lambda tree".

`Lambda.program` carries:

- `module_ident`
- `main_module_block_size`
- `required_globals`
- `code`

Those fields matter operationally.

### `required_globals`

`Translmod.required_globals` computes a set of compilation units whose
initializer effects must happen first.

It scans:

- `Pgetglobal` and `Psetglobal` uses in Lambda
- primitives recorded by `Translprim.get_used_primitives`
- environment-required globals gathered during translation

`Asmgen.compile_implementation` then feeds those globals into `Compilenv` before
running the middle end.

This is not optional bookkeeping. It is part of module-initialization ordering.

### `main_module_block_size`

The main module block size is carried all the way from translation.

The source comments describe two different uses:

- closure path:
  code mutates a preallocated global block via `Setfield(Getglobal(module))`
- flambda path:
  code produces a block value that later becomes symbol initialization

That means module initialization shape is backend-visible very early.

## 4. Lambda Simplification Is Part Of The Native Pipeline Contract

Both native paths run `Simplif.simplify_lambda`.

That pass currently does:

- local-function simplification in native mode or when debug is off
- exit simplification
- let simplification
- tail-modulo-cons rewriting via `Tmc.rewrite`
- tailcall info emission when annotations or wrong-tailcall warnings are active

So the middle end does not receive raw translation output.
It receives Lambda after several semantic rewrites.

For `raml`, this implies that any replacement backend needs an explicit answer
to this question:

do we preserve a Lambda-normalization phase, or do we move that work into a new
IR and its builders?

## 5. `asmcomp` Owns Everything From Cmm Down

`Asmgen.end_gen_implementation` is the backend handoff point after the middle
end.

It does:

- begin assembly
- `Cmmgen.compunit`
- `compile_phrases`
- optional generic-function emission for the native toplevel
- reference emission for external primitive symbols
- end assembly

The actual per-function pipeline inside `compile_fundecl` is:

1. Cmm invariants
2. instruction selection
3. polling instrumentation
4. allocation combining
5. common subexpression elimination
6. liveness
7. dead-code elimination
8. spilling suggestions
9. liveness again
10. live-range splitting
11. liveness again
12. register allocation
13. linearization
14. instruction scheduling
15. emission

That order is important because several later stages assume structure produced by
earlier ones.

## 6. Artifact Paths

The backend supports at least three different artifact flows:

### Normal native compilation

- `Asmgen.compile_implementation`
- emit assembly or use a binary backend
- assemble to an object file

### Start from saved Linear IR

- `Asmgen.compile_implementation_linear`
- load saved `Linear_format`
- emit from there

This is a real seam.
The pipeline can restart at emit-time.

### Link/archive/package flows

- `asmlink` links `.cmx/.o` into executables or shared objects
- `asmlibrarian` builds archives of `.cmx`
- `asmpackager` repackages compilation units into one packed unit

`asmpackager` is especially revealing because it goes back through Lambda
simplification and `Asmgen.compile_implementation`, not around them.

## 7. Design Implications For `raml`

The existing pipeline suggests a useful decomposition for `compiler/raml`:

- one layer for typedtree-or-Riot-semantic lowering into a backend-facing IR
- one normalization/simplification layer
- one middle-end boundary for closure conversion and constant lifting
- one machine-independent native IR
- one machine-dependent but target-generic IR
- one target plugin surface
- one artifact/linking surface

Trying to collapse all of that into a single "emit zort calls directly from the
frontend" design would erase the seams that currently keep the system workable.

The first rewrite should keep those seams visible even if the concrete IR names
change.
