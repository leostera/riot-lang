# RFD0050 - TinyML Bootstrap Compiler

- Feature Name: `tinyml_bootstrap_compiler`
- Start Date: `2026-10-11`
- Status: `presented`
- RFD PR: `(not yet opened)`
- Riot Issue: `(not yet opened)`

## Summary
[summary]: #summary

This RFD defines a C-first, no-JavaScript bootstrap for Riot's first native
compiler. Rust TinyML Stage 0 compiles the bootstrap-compatible RiotML compiler
source graph to C; Clang builds that Stage 0 output as a C-hosted bootstrap
compiler. Stage 0 C is a bridge only: it does not authorize generated-C
fallback for RiotML source modules. The C-hosted compiler must use the
RiotML-owned TypedAST/LambdaIR/NativeIR/AArch64 pipeline for native code. The
acceptance chain requires a native compiler, a native self-rebuild, and a
status-42 smoke program, with no JavaScript or Node execution.

- TinyML provides HM inference, modules, recursive functions, annotations,
  integer and string primitives, tuples, records, variants, and matching.
- Module imports resolve real source modules and preserve imported schemes and
  nominal type metadata.
- The compiler-focused standard library and host interface are bundled with
  TinyML and sufficient to read sources, write output, and invoke a native
  toolchain.
- Rust Stage 0 emits C for the supported bootstrap subset; Clang builds the
  C-hosted compiler. JavaScript and Node are not required in the bootstrap
  chain.
- Floats, actors, distribution, Lean verification, an LSP, package management,
  WASM, and a native TinyML backend are out of scope.

## Motivation
[motivation]: #motivation

The payoff for TinyML is to make the first Riot compiler implementable in Riot's
own eventual implementation language. That requires more than parsing a few
expressions: the compiler will need to represent syntax and types, traverse
recursive data, split code across modules, and produce source or native output.
It does not require Stage 0 Rust to compile TinyML directly to native code, nor
does it require TinyML to become a complete application language.

The repository has a parser, a Hindley–Milner-style checker, source-module
loading, and a JavaScript backend for ordinary TinyML development. Stage 0
integration tests cover functions, recursion, closures, recursive ADTs,
records, tuples, patterns, fixed-width integers, strings, and Host behavior.
For Riotc bootstrap, however, the required chain is Rust Stage 0 → C source →
Clang-built C-hosted compiler → RiotML-owned native backend. The Stage 0 C
output is only a bridge, distinct from the existing C-plus-Clang source
backend retained as a behavioral/performance baseline. The native checker
performs rank-1 HM inference for its implemented subset, with tests for local
and imported polymorphism and nominal type mismatches. The complete native
self-rebuild acceptance chain remains unfinished.

The bootstrap has two compilers and distinct backend roles:

1. **Rust TinyML Stage 0:** compiles the supported Riotc `.ml` source graph to
   deterministic C. This is a bootstrap bridge, not the RiotML source backend.
2. **RiotML compiler implemented in TinyML/RiotML:** Clang builds Stage 0's C
   output into a C-hosted compiler. That compiler's RiotML source modules must
   use the owned TypedAST, LambdaIR, NativeIR, ARM MachineIR, ARM LinearIR, and
   AArch64 Codegen path. Unsupported source constructs fail before Codegen;
   generated C is never a fallback.

The C-hosted RiotML compiler must compile its own source into a native RiotML
compiler executable. That executable must rebuild a successor and the
successor must compile and run a status-42 fixture. JavaScript and Node are
not required or invoked in this chain. TinyML Stage 0 does not need a native
backend.

## Guide-level explanation
[guide-level-explanation]: #guide-level-explanation

A contributor should be able to write a small compiler in TinyML using ordinary
ML techniques:

```ml
type Expr =
  | Int(i32)
  | Add(Expr, Expr)
  | Let(string, Expr, Expr)

let evaluate = fn (expr: Expr) -> match expr {
  | Int(value) -> value
  | Add(left, right) -> evaluate left + evaluate right
  | Let(_, value, body) -> evaluate body
}
```

The example illustrates the supported recursive-data style. Stage 0 checks
and executes recursive ADT matches, so the RiotML compiler can represent its
own syntax and types rather than merely parsing them.

A compiler will also need to divide its implementation into files. It should be
possible to build a module that defines an AST and then bring its types and
functions into scope from another module:

```ml
use Syntax.{Expr}
use Parser.{parse}

let compile_source = fn source -> parse source
```

This is a small module system, not package management. The compiler finds
source files on explicit search paths, checks module dependencies, loads their
interfaces, and makes only the selected names available. A standard library
ships with the TinyML compiler and is available to programs without requiring a
separate package installation.

The required native-compiler bootstrap follows this sequence:

```text
RiotML compiler implementation source under riotc/ (bootstrap-compatible .ml)
        │
        ├── Rust TinyML Stage 0 compiles it to C (bootstrap bridge only)
        │
        └── Clang builds the C-hosted bootstrap compiler
                    │
                    ├── compiles the same source through RiotML-owned IR/AArch64
                    └── produces riotml-compiler.native
                              │
                              ├── rebuilds a native successor
                              └── successor compiles/runs a RiotML fixture
                                    └── program exits with status 42
```

The C-hosted compiler and its native product implement the RiotML compiler;
Stage 0 remains Rust and only provides the C bootstrap bridge. The source
compiled by the RiotML compiler must stay within its documented supported
RiotML subset. This is the required native self-compilation proof: no weaker
parser-only, C-output-only, or generated-C fallback satisfies the acceptance
bar. JavaScript and Node are excluded from the required chain.

TinyML's CLI exists to make that path inspectable and testable. `lex`, `parse`,
`check`, `emit`, and `compile` are enough if each reports meaningful errors and
lets tests exercise the same compiler pipeline. An editor server is not part
of the bootstrap acceptance bar.

## Reference-level explanation
[reference-level-explanation]: #reference-level-explanation

### 1. Language contract

Stage 0 supports the following source-language capabilities:

- **Declarations and modules:** top-level values and functions, recursive
definitions, `use` imports, and a documented module-to-source-file mapping.
  Every module has a stable identity derived from its source path/name. Module
  dependencies are resolved before checking; unresolved modules, missing
  imported members, duplicate module identities, and dependency cycles produce
  errors.
- **Functions and expressions:** function definitions and function values,
  application, local `let`, blocks, literals, operators, and match expressions.
  Top-level recursive definitions are sufficient initially; local recursive
  binding syntax is not required unless the compiler implementation needs it.
- **Data and patterns:** tuples, named record forms, nominal algebraic data
  types, constructors, and patterns for those forms. These must preserve
  constructor identity and payload/field types across module boundaries.
- **Annotations and inference:** annotations on values, parameters, and
  results using `(expr: Type)` ascriptions, `(pattern: Type)` parameter
  hints, and `let pattern: Type = expr` local hints; result types are
  constrained by ascribing the body. These coexist with rank-1 Hindley–Milner
  inference and let-polymorphism. The polymorphic identity function must
  typecheck and be independently usable at distinct types. Type errors must be
  rejected by the compiler entrypoint.
- **Primitive values:** `unit`, `bool`, Unicode text strings, and all
  fixed-width integer types `i8`, `i16`, `i32`, `i64`, `u8`, `u16`, `u32`,
  and `u64`. Text files cross the host boundary as strict UTF-8; no separate
  byte-string primitive is required by the bootstrap subset.
  Floating-point types are not part of Stage 0. Integer literals use an
  expected integer annotation when present and otherwise default to `i32`;
  literals outside the selected type's range are errors. Integer operations
  wrap modulo the selected width, signed division truncates toward zero,
  remainder follows that quotient, and division by zero traps at runtime.
  Values are represented exactly in the JavaScript backend, using `BigInt` and
  width-aware lowering rather than lossy `Number` arithmetic.
- **Operators:** arithmetic and comparisons require operands of the same exact
  integer type; there are no implicit signed/unsigned conversions. For
  compatibility with the execution-tested Stage 0 backend, `string + string`
  remains string concatenation. The bundled `String` module provides
  `concat`, `length`, `get`, `slice`, and `equal`; indices count Unicode scalar
  values, `get` returns `Option<i32>` containing a code point, and `slice`
  clamps its half-open range to string bounds. Other text/byte operations are
  standard-library functions, not overloaded arithmetic.

The implementation must not accept an expression after recording a type error
and then silently treat its output as valid. Internal APIs may retain
best-effort intermediate trees for diagnostics, but `check` and `compile` must
fail when the checked module has errors.

The type system is intentionally limited to rank-1 HM. This RFD does not require
higher-rank types, type classes, subtyping, structural polymorphic records,
effect inference, or a formal Lean proof. “Sound” here means that supported
programs are checked according to a specified, tested typing relation and that
the implementation rejects violations of that relation; it is not a claim of
formal verification.

### 2. Modules and interfaces

`use Module.{value, Type, Constructor}` resolves to a source `.ml` file. A
dot-separated module name maps to a relative path with snake-case path
components and a `.ml` suffix (`Compiler.Parser` maps to
`compiler/parser.ml`). Module components use UpperCamelCase without acronym
runs; path conversion inserts `_` before each uppercase letter other than the
first and lowercases it. Lookup checks the importing module's directory,
explicit `-I` directories in command-line order, then bundled `tinyml/lib/` and
`tinyml/lib/std/` roots. The module graph must be acyclic and is checked and
compiled dependency-first. Every top-level declaration is exported initially;
`use` imports only the explicitly selected names. Missing modules/members,
duplicate identities, and ambiguous or invalid imports are errors.

Imported values are instantiated normally, while nominal types preserve their
constructor schemes, tags, and record fields. Stage 0 resolves source on each
build; a stable binary interface format is not required. There is no package
registry, dependency solver, conditional compilation, or complex visibility
system.

### 3. Bundled standard library and host boundary

TinyML ships a small standard library as part of this compiler project. Its
initial purpose is to support compiler implementations, not to mirror the
future Riot standard library. It includes the basic list/option/result types
and operations plus a compiler-focused Unicode text API. A separate byte-string
abstraction is deferred unless the compiler implementation demonstrates a need
for it.

The bundled `Host` module is an intrinsic, not a general FFI declaration
system. `Host.args` excludes the executable name. The compiler also uses
internal `current_directory` and `runtime_directory` intrinsics to locate its
native runtime; these do not provide general filesystem access. The public
initial contract is:

```text
Host.args         : unit -> List<string>
Host.read_file    : string -> Result<string, string>
Host.write_file   : string -> string -> Result<unit, string>
Host.write_stdout : string -> unit
Host.write_stderr : string -> unit
Host.run_process  : string -> List<string> -> Result<i32, string>
```

`run_process` returns the child exit code (including non-zero codes) as `Ok`;
spawn failures are `Error`. The optional JS target implements these operations
with Node APIs, including strict UTF-8 file decoding and `child_process` without
shell interpolation; it is not used by the required bootstrap. The RiotML
native compiler links a host implementation of the same contract;
`run_process` invokes declared native toolchain operations such as Clang
assembly/linking. Failures return `Result` or cause a non-zero compiler exit,
never a placeholder success. No general-purpose FFI syntax or actor/runtime
API is needed.

### 4. Backend strategy

The required Stage 0 output is deterministic C for the supported compiler
source graph. Clang builds that output as a C-hosted bootstrap compiler. This
C is a bootstrap artifact, not Riotc's native source backend and not a fallback
for unsupported RiotML constructs.

The Rust Stage 0 compiler retains supported C and JavaScript targets for their
respective uses, but the required Riotc bootstrap path invokes only the C
target and Clang. Native code generation is implemented in the RiotML compiler
under `riotc/`: `.ml → AST → TypedAST → LambdaIR → NativeIR → ARM MachineIR →
ARM LinearIR → AArch64 Codegen`. The existing C-plus-Clang source backend
remains a measured behavioral/performance baseline during migration; it must
not be selected as a fallback by the owned native command.

The owned native backend must support every source construct used by the
compiler implementation itself, in addition to the smoke-test subset below.
It is not acceptable to compile only a parser or to stop after emitting Stage
0 C without producing the native compiler executable. No Rust Stage 0 native
backend is introduced. The owned pipeline must reject unsupported operations
before Codegen, and executable tests must validate observable behavior.

### 5. RiotML first native compiler slice

The RiotML front end implements the documented shared bootstrap subset used
by its own source: modules, top-level and recursive functions, local `let`,
rank-1 HM types, annotations, integer and string values, tuples, records,
variants, and pattern matching. Its native backend must support every one of
those constructs used by the compiler implementation. It parses and typechecks
each input module before code generation; invalid input must produce a
diagnostic and non-zero status without leaving a plausible output executable.

The first user-program fixture is deliberately small:

```ml
let main = fn () -> 42
```

The compiler treats `main : unit -> i32` as the native entry point. The
existing C-plus-Clang `compile` command remains a differential baseline; the
owned AArch64 command must also compile supported fixtures and return status
42. An ill-typed fixture (for example, `let main = fn () -> 1 + true`) must be
rejected before Codegen. The production acceptance path requires native
compiler self-rebuild, not just this small fixture.

**Current implementation status (2026-10-12):** Rust TinyML Stage 0 emits
deterministic C for the complete Riotc source graph; Clang builds a working
C-hosted compiler. From a clean git archive, with `PATH=/usr/bin:/bin`, the
C-hosted C-backend status-42 smoke and an owned two-function AArch64
direct-call-graph smoke both pass; ill-typed input is rejected before Codegen
and stale executable/assembly outputs are removed. The owned path also has
verified TypedAST, LambdaIR, NativeIR SSA, AArch64 MachineIR/LinearIR, assembly
Codegen, scalar/control-flow/direct-call differential tests, and an explicit
`Host.args` bridge. It is not yet a whole-module compiler: module initialization,
globals, concrete string/aggregate operations, general Host marshalling, and
native self-rebuild remain incomplete. Unsupported operations fail explicitly;
there is no generated-C fallback. `Check.Inference` checks the complete
source-module graph with rank-1 HM inference, independently instantiates local
`let` and imported value schemes, and preserves nominal type/constructor
identities and payload constraints across modules. Its type-error tests cover
local/imported polymorphism, nominal mismatches, invalid primitive operations,
bad calls, unknown names/types, and incompatible patterns. The C-first
Stage 0-to-C-hosted bridge is validated, but the required native compiler,
successor self-rebuild, and full status-42 acceptance chain have not passed.

### 6. Validation-focused CLI

The retained commands are:

- `tinyml lex FILE`: show tokens and source locations.
- `tinyml parse FILE`: parse and print a stable AST representation.
- `tinyml check FILE [-I DIR ...]`: resolve modules and report parse/type/module
  errors; success means the complete reachable source graph checks.
- `tinyml emit --stage ... FILE`: expose selected intermediate stages for
  debugging and snapshots.
- `tinyml compile FILE -o OUTPUT`: compile to the supported Stage 0 host
  artifact and report all failures with non-zero status.

An LSP, formatter, package manager, optimizer, and multi-target CLI are
explicitly not required. The commands should stay only while they help validate
the compiler or exercise the Stage 0 workflow.

### 7. Completion criteria

The C-first Riot compiler bootstrap is complete when all of the following
hold:

1. The documented language subset parses and is typechecked across multiple
   source modules, including imported polymorphic values and nominal data
   types.
2. Positive and negative tests cover principal HM behavior, annotations,
   recursive definitions, operator typing, module resolution, records,
   variants, and pattern matching. Invalid programs cannot pass `check` or
   `compile`.
3. A compact compiler-focused TinyML standard library is bundled and used by
   the tests; no manual, undocumented interface setup is needed for ordinary
   modules.
4. Stage 0 compiles TinyML source into the chosen runnable host target, and
   tests execute the result and check its observable behavior.
5. The RiotML compiler implementation is written in the documented
   bootstrap-compatible `.ml` subset and Stage 0 emits deterministic C for
   its complete source graph.
6. Clang builds that C into a working C-hosted bootstrap compiler with Node and
   JavaScript excluded from `PATH`.
7. The C-hosted compiler builds Riotc through its owned ARM pipeline; the
   resulting native compiler rebuilds a successor, and the successor compiles
   and runs a status-42 fixture. Ill-typed source fails before Codegen and
   leaves no plausible executable. Generated C is not a native-source fallback.
8. The documented test command passes on the supported development host, and
   the known unsupported subset is stated plainly.

### 8. Current progress and remaining work

The TinyML language, module loader, compiler-focused library, and Host
contracts have positive and negative coverage. On 2026-10-12, Stage 0 emitted
deterministic C for the Riotc graph; Clang built the C-hosted compiler; and a
clean archive tested with `PATH=/usr/bin:/bin` passed both the existing
C-backend smoke and a bounded owned AArch64 direct-call-graph smoke (status 42).
Ill-typed input was rejected with stale output cleanup. The full workspace
suite and formatting checks pass. The native Riotc compiler, whole-module
globals/initialization, full compiler-source corpus, successor self-rebuild,
and final native acceptance chain are still incomplete. Keep the removed LSP,
pending WASM output, and any Rust Stage 0 native backend out of this design.

## Drawbacks
[drawbacks]: #drawbacks

- The C-first bridge keeps a C representation of the compiler source and adds
  Clang as a bootstrap build dependency; the bridge is not the owned source
  backend and must not become a fallback.
- JavaScript number semantics do not directly preserve all fixed-width signed
  and unsigned integers, especially 64-bit values. The Stage 0 backend must
  implement the specified exact `BigInt` representation and width-aware
  operators; silently using imprecise `Number` values is unacceptable.
- A minimal language can still grow unexpectedly when compiler code needs
  containers, bytes, error handling, or convenient syntax. The completion
  criteria need to prevent these needs from becoming a rationale for a broad
  general-purpose runtime.
- Removing the incomplete Stage 0 AArch64 work discards experiments that
  might have been reusable, but avoids making an unvalidated implementation a
  prerequisite for Riot compiler development.
- The first Riot compiler's native backend will need its own correct ABI,
  runtime/host boundary, and end-to-end tests. TinyML Stage 0 does not solve
  those problems automatically.

## Rationale and alternatives
[rationale-and-alternatives]: #rationale-and-alternatives

### Authoritative chain: C-first Stage 0 bridge; RiotML-owned native backend

This uses Rust TinyML to emit C for the compiler source graph, then Clang to
build a C-hosted bootstrap compiler. It establishes the initial executable
without Node. The RiotML compiler itself owns native code generation through
TypedAST, LambdaIR, NativeIR, ARM MachineIR, ARM LinearIR, and AArch64 Codegen.
The existing C-plus-Clang source backend remains a differential/performance
baseline but is not used as a fallback by the owned path. The end-to-end native
self-rebuild acceptance chain remains outstanding.

### Chosen alternative: C as the Stage 0 bootstrap target

C is selected as the required Stage 0 output because it can be compiled by
Clang in a no-JavaScript chain. Stage 0's emitted C is only a bootstrap bridge;
it does not move native code generation into Rust or authorize generated-C
fallback for RiotML source constructs.

### Rejected alternative: make Stage 0 itself produce native code

This would require a separate native runtime and ABI before TinyML can compile
the RiotML compiler. The previous AArch64 experiment was removed because it had
placeholder runtime behavior and incomplete lowering. Native code generation
is required, but it belongs in the TinyML-written RiotML compiler; Stage 0
only needs to emit the C bootstrap bridge.

### Alternative: shrink TinyML to a parser/checker only

A parser and checker might be enough to experiment with Riot compiler logic,
but it does not meet the stated bootstrap goal: Stage 0 must execute a
TinyML-written compiler. Some working output target is necessary.

### Do nothing

Continuing to expand the current project without a completion contract leaves
multiple partially implemented backends and tools competing for effort. It
would keep the project useful for experimentation but would not establish that
TinyML can implement the Riot compiler.

## Prior art
[prior-art]: #prior-art

- The current `tinyml` crate separates parsing, checking, Lambda IR, and
  JavaScript backend lowering. Its JavaScript integration tests provide a
  concrete host target to reuse. The removed AArch64 snapshots showed why
  output snapshots alone are insufficient evidence for a native backend.
- `tinyml/lib/std/list.ml`, `tinyml/lib/std/option.ml`, and
  `tinyml/lib/std/result.ml` show a start on source-level
  algebraic data types and recursive functions. Other standard-library files
  use unsupported module or external-declaration forms, so the project needs an
  intentionally supported bootstrap library rather than assuming the current
  directory is already a working package.
- RFD0002 documents Riot's prior bootstrap process: bootstrap implementations
  are intentionally narrower than the full system and exist to reach a useful
  next stage. The same principle applies here, though TinyML's next stage is a
  Riot compiler written in TinyML.
- [RFD0049 - Becoming a Language](RFD0049-becoming-a-language.md) frames Riot
  ML as a language project and treats the current prototype as bootstrap
  material and evidence, rather than as a permanent product definition. This
  RFD narrows that broad direction to the concrete TinyML bootstrap contract.

## Decisions and remaining questions
[decisions-and-remaining-questions]: #decisions-and-remaining-questions

### Implementation contract recorded by this revision

- Rust Stage 0 emits C for the Riotc source graph; Clang builds the
  C-hosted bootstrap compiler. The required chain invokes no JavaScript or Node.
- RiotML native compilation is owned by the `.ml` pipeline under `riotc/`; the
  existing C-plus-Clang source backend is only a behavioral/performance
  baseline, never an implicit fallback. Native self-rebuild remains unverified.
- Stage 0 supports all fixed-width signed and unsigned integers from 8 to 64
  bits, exact contextual integer literals (default `i32`), width-wrapping
  arithmetic, truncating division, and no floats or implicit conversions.
- Modules use snake-case `.ml` paths from dotted module names, source-local and
  ordered `-I` roots followed by bundled `lib/` and `lib/std/`, and an acyclic
  dependency graph.
- `Host` is a compiler-provided intrinsic module for arguments, file I/O,
  diagnostics/output, and safe process invocation; it is not arbitrary FFI.
- The completion proof requires the C-hosted bootstrap compiler to compile
  Riotc through the owned ARM pipeline, then the native compiler to rebuild a
  successor; the successor compiles and runs a valid `main : unit -> i32`
  fixture with status 42 while rejecting ill-typed input before Codegen.

### Remaining implementation-time details

- Rank-1 HM inference, polymorphic local/imported values, and nominal type
  checking are implemented and have positive/negative tests; whole native
  self-compilation is not yet established.
- A clean archive passes deterministic Stage 0 C emission, Clang host build,
  C-backend smoke, bounded owned direct-call-graph smoke, and ill-typed-output
  cleanup with Node excluded. This does not prove native Riotc self-rebuild.
- Scalar/control-flow/direct-call Codegen and `Host.args` are implemented.
  Module globals/initialization, concrete aggregate and string operations,
  general Host marshalling, and the full compiler-source corpus remain
  unsupported in the owned backend. Preserve explicit failures until those
  features are implemented and tested.

### Out of scope

- Actor syntax and semantics, `spawn`/`send`/`recv`, distribution, and an actor
  runtime.
- A Lean-verified type system or actor calculus.
- A package registry, dependency solver, LSP, formatter, or full-featured
  standard library.
- TinyML self-hosting or a native TinyML runtime, unless later work chooses to
  pursue either independently.

## Future possibilities
[future-possibilities]: #future-possibilities

After the Riot compiler can compile its own source to native code, it may
become useful to reduce dependence on Clang or replace portions of Stage 0
with later compiler stages. These are future bootstrap improvements, not
prerequisites for native self-rebuild. Actor support,
floating-point types, richer modules, and editor tooling should be designed for
Riot ML itself rather than pulled into TinyML by default.
