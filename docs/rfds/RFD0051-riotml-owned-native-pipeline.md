# RFD0051 - RiotML Owned Native Compiler Pipeline

- Feature Name: `riotml_owned_native_compiler_pipeline`
- Start Date: `2026-10-12`
- Status: `presented`
- RFD PR: `(not yet opened)`
- Riot Issue: `(not yet opened)`

## Summary
[summary]: #summary

This RFD specifies an owned native compiler pipeline for the TinyML-written
RiotML compiler: `.ml → AST → TypedAST → LambdaIR → NativeIR → ARM → Codegen`.
`LambdaIR` is the compiler's typed, desugared core language; `NativeIR` is a
target-independent SSA control-flow IR; the ARM backend lowers it through
machine and linear IRs before emitting AArch64 assembly. An initial
compiler-pipeline slice is implemented in bootstrap-compatible TinyML/RiotML
source under `riotc/`, not as a Rust Stage 0 backend; whole-module native
compilation remains unfinished. RFD0050's
bootstrap architecture is revised to C-first/no-JavaScript: Rust Stage 0 emits
C and Clang builds a C-hosted bootstrap compiler. That C is only a bridge; the
owned RiotML backend must never fall back to generated C. RFD0050's language,
typechecking, module, integer, string, Host, and failure-safety contracts are
preserved. The existing C-plus-Clang source backend remains a measured
behavioral/performance baseline until the owned pipeline passes the complete
native self-rebuild acceptance chain.

## Motivation
[motivation]: #motivation

The earlier RFD0050 proposal used a JavaScript-hosted compiler and C-plus-Clang
source backend. The measured C-plus-Clang run remains a historical behavioral
and performance baseline, distinct from the revised bootstrap bridge. The
current required bootstrap is Rust Stage 0 to C, then Clang to a C-hosted
compiler; JavaScript/Node are not required. The owned backend does not use
emitted C for RiotML source modules and is intended to give Riot ownership of
the machine-level representations and ARM code generation.

Riot ML is intended to own its language and compiler rather than permanently
rely on a host compiler's intermediate representation and code generator. A
clear chain of Riot-owned representations makes each lowering stage reviewable,
lets the typechecker hand proven facts to later passes, and gives the native
backend room to grow without mixing source semantics, runtime representation,
ARM instructions, and assembly syntax into one emitter. It also gives a future
compiler contributor a place to inspect where a source construct changes
meaning or representation.

The goal is not to imitate all of OCaml's optimizer or to claim that another
pipeline is automatically faster. It is to own a small, testable path from the
RiotML core to native code and measure it end to end. The current `riotc`
compiler already has a parser, name checking, rank-1 HM checking, module
loading, and a C backend. The proposal builds on those contracts while making
the backend boundary explicit.

## Goals

- Define and implement the compiler pipeline in RiotML source under `riotc/`.
- Keep parsing, type checking, language semantics, target-independent lowering,
  ARM lowering, and code emission as distinct reviewable stages.
- Make `LambdaIR` the typed, desugared core of supported RiotML semantics.
- Make `NativeIR` a target-independent SSA form with explicit control flow.
- Make ARM-specific selection, ABI/register handling, and code emission explicit
  stages rather than direct source-to-assembly translation.
- Preserve RFD0050's language/type/Host/failure-safety contracts while revising
  its bootstrap acceptance chain to C-first/no-JavaScript.
- Keep each IR printable and validated so failures can be localized to a pass.

## Non-goals

- Adding a native backend to the Rust Stage 0 TinyML compiler. Its C output
  is a bootstrap bridge only; RiotML native codegen remains in `riotc/`.
- Using generated C as a fallback for RiotML source constructs or weakening
  RFD0050's language/failure-safety contract.
- Redesigning RiotML syntax, rank-1 HM typing, modules, integers, strings, or
  the Host contract.
- Implementing actors, distribution, package management, Lean verification,
  an LSP, or a general-purpose optimization framework.
- Supporting every TinyML type in the first native backend; unsupported
  constructs must be rejected clearly rather than assigned a fallback
  representation.
- Writing a custom assembler, linker, object-file writer, or direct machine
  code encoder in the first slice.
- Selecting a final heap/GC design or changing runtime semantics in this RFD.

## Guide-level explanation
[guide-level-explanation]: #guide-level-explanation

A contributor should be able to follow one supported source program through
named representations. The parser produces syntax, the checker produces a
resolved and typed program, and lowering turns that program into a small
semantic core before the backend makes control flow and machine operations
explicit.

For example, the smoke program from RFD0050:

```ml
let main = fn () -> 42
```

first becomes an AST declaration, then a TypedAST declaration with the checked
entry type `unit -> i32`. LambdaIR represents the function and its return value
without ARM details. NativeIR gives the function a control-flow block and a
single SSA value for `42`. ARM lowering selects the return-value and return
instructions; Codegen emits assembly that the system assembler and linker turn
into the executable.

```mermaid
flowchart LR
  S[.ml source] --> A[AST]
  A --> T[TypedAST]
  T --> L[LambdaIR: core RiotML]
  L --> N[NativeIR: target-independent SSA]
  N --> M[ARM MachineIR: selected ops, virtual registers]
  M --> R[ARM LinearIR: ABI, allocation, spills]
  R --> C[Codegen: AArch64 assembly]
  C --> X[Assembler and linker]
  X --> E[Native executable]
  A -. parse error .-> D[Structured diagnostics]
  T -. type/name error .-> D
  L -. unsupported or invalid lowering .-> D
  N -. invalid IR .-> D
  M -. unsupported target operation .-> D
```

Each stage has a stable debug printer and a validator. During development, a
contributor can print or stop after any stage instead of debugging only the
final executable. No later stage silently repairs a type error or accepts a
malformed earlier IR.

The required no-JavaScript compiler path is:

```text
Rust Stage 0 TinyML compiler
        │ emits the compiler source graph as C (bootstrap bridge)
        ▼
Clang-built C-hosted bootstrap compiler
        │ compiles Riotc through the owned IR/AArch64 pipeline
        ▼
Native RiotML compiler
        │ rebuilds a successor and compiles the status-42 fixture
        ▼
AArch64 native executable
```

This does not move native code generation into Stage 0. It makes the
TinyML-written compiler's native path Riot-owned; Stage 0 C is not a source
backend or fallback.

## Reference-level explanation
[reference-level-explanation]: #reference-level-explanation

### 1. Stage boundaries and responsibilities

#### 1.1 `.ml → AST`

The parser in `riotc/syntax/` remains responsible for lexical and syntactic
validity. AST nodes preserve source spans and describe syntax, not guessed
runtime representations. Parse failures become source diagnostics and stop the
pipeline.

**Why keep this stage:** syntax errors must be reported before typechecking,
and the source AST should remain useful for diagnostics and future tools even
when later IRs desugar syntax.

#### 1.2 `AST → TypedAST`

Name resolution and rank-1 HM checking establish the semantic facts used by
lowering. The TypedAST records resolved binding/entity identities, inferred or
annotated types, and source spans. Only an error-free checked module graph may
enter LambdaIR lowering. Checker diagnostics remain structured until the CLI
renders them.

**Why keep this stage:** the backend should consume trusted, resolved names and
types rather than repeat lookup or infer types from syntax. This also keeps an
invalid program from producing a plausible executable, as required by RFD0050.

The implementation may evolve the current `Check.Inference` API to return a
TypedAST; this RFD does not prescribe the concrete inference algorithm or
change the RFD0050 type-system contract.

#### 1.3 `TypedAST → LambdaIR`

LambdaIR is the typed, desugared, target-independent core RiotML language. It
removes source-only distinctions while preserving the semantics needed by the
backend. Its initial constructs should be the smallest complete semantic set
needed by `riotc` itself: values, recursive function declarations and
closures, direct and indirect calls, lets, primitive operations,
aggregates/constructors, and explicit matches or their equivalent core
control-flow form. It preserves source provenance for diagnostics.

LambdaIR does not encode ARM registers, stack slots, calling conventions,
object formats, or assembly text. Closure behavior must be specified here or at
its explicit lowering boundary; it must not be simulated by a placeholder
runtime call that discards captures or call arguments.

**Why keep this stage:** it gives Riot ML one canonical core semantics for
native lowering and future analyses. Source syntax can evolve without forcing
every backend pass to understand every surface form.

#### 1.4 `LambdaIR → NativeIR`

NativeIR is a target-independent SSA control-flow graph. Values have stable
IDs and exactly one definition; blocks have explicit parameters; branches,
switches, and returns are terminators. Joins pass values through block
parameters (the phi-equivalent form), not implicit mutable environments.

NativeIR operations preserve the exact checked source type needed for correct
lowering, including integer width and signedness. Calls, allocation, runtime
operations, and potentially trapping operations have explicit semantics or
effect classification. Aggregate layout and runtime calls are represented
symbolically; ARM registers and encodings are not.

Every NativeIR function is validated before target selection:

- the entry block exists and each block has one terminator;
- block and value IDs are unique and refer to existing definitions;
- definitions dominate their uses, including edge arguments;
- branch/switch edge arguments match destination block parameters in arity and
  type;
- return values match the declared result type;
- primitive operands and results satisfy the operation's exact type contract.

The verifier must reject malformed IR with a compiler diagnostic or internal
compiler error; it must not substitute zero, `Ptr`, or another guessed type.

**Why use SSA here:** values become explicit and immutable, which makes
control-flow merges, constant folding, liveness, and later register assignment
tractable. Keeping this IR target-independent allows new target backends without
repeating language-semantic lowering.

#### 1.5 `NativeIR → ARM MachineIR`

ARM MachineIR is target-specific instruction selection over virtual registers.
It defines the supported AArch64 operation set, condition codes, immediate
constraints, labels, call forms, and data operations. Target information
includes an explicit architecture/platform/ABI selection rather than assuming
that all ARM64 systems share one assembly or calling convention.

The approved first target is macOS AArch64 on Apple Silicon with Mach-O and
the Apple AArch64 C ABI for declared runtime helpers; Clang assembles and links
the emitted text. The initial convention is zero-sized `unit`, typed unboxed
scalars, pointer-shaped composites, a hidden closure environment, and
specialization at concrete types. This does not approve concrete aggregate
layouts or every helper call shape; those remain explicitly typed and must be
verified before use. Instructions must distinguish source widths and signed
operations. Unsupported NativeIR operations produce diagnostics before
Codegen.

**Why split this stage from NativeIR:** target selection is where generic
operations become ISA-specific. It should not infect the typed language core
or prevent a later target from consuming NativeIR.

#### 1.6 `ARM MachineIR → ARM LinearIR`

ARM LinearIR makes execution order and machine resources explicit. This stage
lowers the selected virtual-register instructions according to the target ABI,
assigns physical registers, inserts spills and reloads, lays out stack slots and
frames, and resolves block control flow to an instruction sequence. Scheduling
and more advanced register allocation are optional optimizations, not
prerequisites for correctness.

**Why separate this stage:** instruction selection and physical resource
assignment have different invariants. A simple initial allocator can be
replaced without changing LambdaIR, NativeIR, or the instruction selector.

#### 1.7 `ARM LinearIR → Codegen`

Codegen emits AArch64 assembly for the declared platform and ABI. In the first
implementation, a configured system assembler and linker may produce the object
file and executable. This keeps instruction selection, register allocation,
frame layout, symbol conventions, and assembly emission Riot-owned without
requiring Riot to implement an assembler or linker.

The generated program may call explicitly declared runtime/Host helpers. Those
calls use a documented ABI; emitting C for each RiotML source module is not a
fallback for unsupported code. The existing C runtime may remain a support
library during migration if its ABI is explicitly represented and tested.

**Why this final boundary:** textual assembly is inspectable and can be tested
before execution. Delegating assembly/linking is a bounded toolchain dependency,
not a return to C as the source-language backend.

### 2. Shared invariants

All stages preserve these invariants:

1. **Source identity:** resolved names use binding/entity identity, not display
   names or sanitized assembly names. Backend symbols are uniquely mangled and
   collision-checked.
2. **Type fidelity:** each lowering preserves integer width, signedness,
   function/closure-call semantics, and nominal constructor identity needed for
   the supported subset. No stage silently coerces or defaults an unsupported
   representation.
3. **Evaluation behavior:** lowering preserves source evaluation order,
   short-circuit behavior where specified, observable effects, and trap
   behavior.
4. **Control-flow validity:** every block terminates; every edge transfers the
   correct arguments; matches retain all arms and failure behavior.
5. **ABI correctness:** calls, return values, stack alignment, callee-saved
   registers, symbols, and runtime helpers agree with the chosen target ABI.
6. **Artifact safety:** a failed parse, check, IR validation, target lowering,
   assembler, or link step returns failure and does not leave a plausible
   executable at the requested output path.
7. **Stage 0 boundary:** Rust TinyML emits C for the required bootstrap and
   Clang builds the C-hosted compiler. JavaScript/Node are not invoked in the
   acceptance chain. The C output is a bootstrap bridge only; all Riot-owned
   native IR lowering and code generation lives in compiler source under
   `riotc/`, with no generated-C fallback.

### 3. Diagnostics and observability

Each stage returns either a valid next-stage value or diagnostics. Diagnostics
carry source spans whenever an error can be traced to user source; backend-only
failures identify the pass, function, target, and relevant IR location.

The compiler offers developer-facing stage dumps, for example:

```text
riotml compile source.ml --dump=ast
riotml compile source.ml --dump=typed-ast
riotml compile source.ml --dump=lambda
riotml compile source.ml --dump=native
riotml compile source.ml --dump=arm
```

Exact CLI syntax may follow the existing `riotc` CLI, but each dump format is
stable enough for tests and debugging. A verifier runs at IR boundaries in
tests and may be enabled in development builds. No debug dump is required for
normal successful compilation.

### 4. Runtime and compatibility boundary

This RFD owns the compiler representation pipeline, not a new source-language
runtime contract. Generated native code must preserve RFD0050's supported
program behavior and Host operations. The initial macOS/Mach-O call ABI and
unit/scalar/pointer convention are approved and recorded in
`riotc/backend/native/aarch64/ABI.md`; concrete aggregate/string layouts,
allocation, closure environments, and general helper marshalling remain
unimplemented. Do not infer those details from ABI documentation or pass
unsupported values to Codegen without explicit typed lowering.

During bring-up, the existing C backend and runtime provide a behavioral
reference. The owned backend may call existing C runtime functions through
explicitly declared and tested helper ABIs, but it must not use generated C as
an implicit fallback for source constructs. Keep the C backend as a measured
baseline until the full end-to-end acceptance criteria pass from a clean
checkout and behavior agrees for the supported corpus.

### 5. Validation and acceptance criteria

The pipeline is ready to replace the current C backend only when:

1. Unit tests exercise parsing/typechecking and each IR lowering, including
   positive and negative stage-verifier cases.
2. The native corpus covers every construct used by the compiler source and
   the RFD0050 smoke program; unsupported forms are rejected before codegen.
3. Differential tests compare the existing C backend and owned backend on
   shared fixtures until the C path is retired.
4. Rust Stage 0 emits C for the Riotc source graph; Clang builds the
   C-hosted bootstrap compiler with Node/JavaScript excluded. That compiler
   produces a native compiler only through the owned pipeline (never C
   fallback).
5. That native compiler rebuilds the same compiler source into a successor
   native compiler, and the successor compiles a `main : unit -> i32` fixture
   whose executable exits with status 42.
6. Ill-typed input is rejected before codegen; assembler/linker failures return
   non-zero status and leave no plausible executable.
7. Formatting and `cargo test --workspace` pass; the documented acceptance
   sequence works from a clean checkout with no generated/untracked
   prerequisites.
8. Compile time and output size are measured against the C-plus-Clang baseline.
   This RFD sets no numeric performance target; a threshold must be agreed
   before performance is claimed as a release criterion.

The C-first chain replaces RFD0050's earlier JavaScript bootstrap proposal
without changing its language, typechecking, module, integer, string, Host, or
failure-safety contracts. The native self-rebuild criterion still requires the
successor compiler and status-42 fixture; the verified direct-call-graph slice
is progress, not completion.

## Drawbacks
[drawbacks]: #drawbacks

- Riot takes on more compiler implementation: typed IR construction, multiple
  lowerings, IR validation, instruction selection, ABI handling, register
  allocation, and assembly emission.
- More IR boundaries improve isolation but create design and maintenance costs;
  poorly specified IRs can become conversion boilerplate rather than useful
  semantic boundaries.
- A direct ARM backend is tied to an ABI and platform beyond the ISA. Supporting
  Apple arm64 and Linux/AAPCS64 may require separate assembly/object and runtime
  integration work.
- The existing C backend will need to remain temporarily for comparison,
  increasing maintenance until the owned path is validated and the C path is
  deliberately removed.
- An owned backend is not automatically faster. Naive lowering, all-spill
  code, poor ABI integration, or repeated passes could remain slow or produce
  larger binaries; profiling and agreed targets are required.
- Delegating final assembly/linking retains external assembler/linker
  dependencies, although it avoids implementing those large tools.

## Rationale and alternatives
[rationale-and-alternatives]: #rationale-and-alternatives

### Proposed design: owned stages with target-independent SSA

A typed, desugared LambdaIR separates RiotML semantics from machine details.
A target-independent SSA NativeIR provides explicit control flow and a stable
input to target backends. ARM MachineIR and LinearIR isolate instruction
selection from ABI/register allocation. Codegen then emits inspectable assembly.
This follows the useful separation found in OCaml's native pipeline without
copying its multiple optimizing middle ends or runtime.

### Keep C plus Clang as the measured baseline

This is the simplest existing source backend and provides a behavioral oracle
during implementation. It is retained until the owned path passes acceptance,
but is not the permanent native architecture. Per-source generated C mediates
native semantics through a C compiler rather than Riot-owned IR and ARM
lowering. The historical self-rebuild measurements motivate measurement of the
owned path, but do not prove it will be faster.

### Lower LambdaIR directly to ARM assembly

This uses fewer IRs and could produce a first executable sooner. It couples
language-semantic lowering, runtime representation, control-flow construction,
ABI details, and assembly syntax in one path. It also makes IR-level
optimization, validation, and future backends harder. It may be appropriate for
a deliberately throwaway spike, but not for the owned pipeline this RFD
proposes.

### Use an external IR/code generator (for example, LLVM)

An external backend could supply instruction selection, register allocation,
and object generation. It would reduce the amount of ARM machinery Riot must
write, but introduce a large external compiler dependency and a foreign IR/API
as the core backend contract. The request is to own this pipeline; adopting an
external code generator would need a separate RFD and explicit approval.

### Put ARM details directly into NativeIR

One target-specific SSA IR could remove a conversion stage. That makes every
NativeIR producer target-aware and makes future architectures either translate
from ARM concepts or duplicate the frontend. Keeping NativeIR target-independent
costs one lowering but establishes a cleaner extension point.

### Do nothing

Without the owned pipeline, the existing C backend remains the behavioral
baseline and cannot provide the required native self-rebuild architecture. The
C-first Stage 0 bridge only bootstraps the C-hosted compiler; it does not
replace the Riot-owned IR/AArch64 path.

## Prior art
[prior-art]: #prior-art

- [RFD0049 - Becoming a Language](RFD0049-becoming-a-language.md) makes Riot
  ML's parser, typed core, compiler, runtime semantics, diagnostics, and tooling
  Riot-owned concerns, while explicitly leaving the final core and backend
  architecture open. This RFD specifies one compiler-pipeline slice of that
  direction.
- [RFD0050 - TinyML Bootstrap Compiler](RFD0050-tinyml-bootstrap.md) defines
  the TinyML language/Host contracts and revised C-first/no-JavaScript
  acceptance chain. This RFD specifies the owned RiotML native pipeline and
  retains the existing C-plus-Clang source backend as a measured baseline.
- The Rust Stage 0 experiment at commit
  [`892cf9459`](https://github.com/leostera/riot-lang/commit/892cf9459)
  sketched a block-parameter SSA program, a NativeIR-to-AArch64 selector, and
  textual assembly emission. It is useful design prior art, not code to restore
  to Stage 0: the RFD0050 trim intentionally removed that Rust backend. Its
  strongest reusable idea is the separation between generic SSA and target MIR;
  its incomplete match/control-flow, symbol identity, type-width, and register
  handling show why the new IR contracts need verifiers and end-to-end tests.
- OCaml 5.5.1's `driver/compile_common.ml` separates parsing and typechecking;
  `driver/optcompile.ml` lowers Typedtree through Lambda and a middle-end; and
  `asmcomp/asmgen.ml` lowers Clambda through Cmm, instruction selection,
  register allocation, Linear, emission, and the configured assembler. Its
  AArch64 modules separate instruction selection, procedure/ABI policy, and
  assembly emission. Riot should adopt the separation and observability, not
  OCaml's full optimization machinery. See [OCaml compiler source](https://github.com/ocaml/ocaml/tree/5.5.1/driver),
  [native backend](https://github.com/ocaml/ocaml/tree/5.5.1/asmcomp), and
  [AArch64 backend](https://github.com/ocaml/ocaml/tree/5.5.1/asmcomp/arm64).

## Decisions and remaining questions
[decisions-and-remaining-questions]: #decisions-and-remaining-questions

### Implementation state and decisions

- An initial pipeline slice is implemented in TinyML/RiotML source under
  `riotc/`, not in Rust Stage 0. TypedAST, LambdaIR, verified
  target-independent NativeIR SSA, AArch64 MachineIR/LinearIR, and assembly
  Codegen have positive/negative tests.
- LambdaIR is the typed, desugared, target-independent core; NativeIR has
  explicit blocks, terminators, effects, and block arguments.
- The approved first target is macOS AArch64/Mach-O using Clang for assembly
  and linking. `ABI.md` records zero-sized unit, typed unboxed scalars,
  pointer-shaped composites, hidden closure environment, concrete-type
  specialization, and explicit helper boundaries.
- The currently verified native slice covers unit/i32/bool, opaque-pointer
  pass-through, scalar/control-flow/direct calls, i32 div/rem traps, pattern
  failure, and the exact `Host.args` bridge. A direct-call-graph harness also
  compiles a bounded same-module graph and runs status 42 from a clean C-first
  bootstrap with Node excluded.
- This is not whole-module Codegen: module initialization/global loads,
  concrete strings/aggregates/constructors/projections, general Host marshalling,
  and the compiler-source corpus remain incomplete. Native Riotc self-rebuild
  and the successor smoke are not established. Unsupported operations fail
  explicitly before Codegen; the C backend is not a fallback.
- RFD0050's language and failure-safety contracts remain unchanged; its
  bootstrap/acceptance architecture is revised to C-first/no-JavaScript.
  Existing C-plus-Clang source compilation remains the measured baseline.

### Unresolved representation and release questions

- The approved ABI settles call shapes and pointer-shaped composites, but not
  concrete string/aggregate/closure layouts or allocation. Which explicit
  typed helper boundaries and runtime object representation should implement
  those operations? Do not infer these details from `void *` shape or the
  legacy tagged `RtValue` runtime.
- What additional declared Host/runtime helper bridges are needed for the full
  compiler-source corpus? Each boundary requires an exact prototype and
  differential tests.
- What minimal optimization passes are needed after correctness and before
  performance measurement? Constant folding and DCE must respect exact
  fixed-width arithmetic and effect/trap behavior.
- What compile-time and output-size target makes the new compiler
  “reasonable”? No benchmark or numeric threshold is asserted here.
- What sequencing/cutover policy retires the current C backend after
  differential and bootstrap acceptance tests are reliable?

## Future possibilities
[future-possibilities]: #future-possibilities

Once the first owned ARM pipeline is correct, Riot may add additional target
backends that consume NativeIR, richer optimization passes, direct object-file
or machine-code emission, and target-specific debug information. Those are
future extensions, not prerequisites for accepting the stage boundaries in
this RFD. Changes to the runtime/GC model, actor lowering, or source-language
type contract require their own design review.
