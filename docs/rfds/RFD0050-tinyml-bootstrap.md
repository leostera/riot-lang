# RFD0050 - TinyML Bootstrap Compiler

- Feature Name: `tinyml_bootstrap_compiler`
- Start Date: `2026-10-11`
- Status: `presented`
- RFD PR: `(not yet opened)`
- Riot Issue: `(not yet opened)`

## Summary
[summary]: #summary

This RFD defines a two-step bootstrap for Riot's first native compiler. The Rust
Stage 0 TinyML compiler compiles a TinyML implementation of the RiotML compiler
to JavaScript. Running that JavaScript compiler then compiles the same
bootstrap-compatible compiler source to a native RiotML compiler executable,
which compiles and runs a native RiotML smoke-test program. TinyML itself needs
only the JavaScript host target; the native backend is part of the
TinyML-written RiotML compiler.

- TinyML provides HM inference, modules, recursive functions, annotations,
  integer and string primitives, tuples, records, variants, and matching.
- Module imports resolve real source modules and preserve imported schemes and
  nominal type metadata.
- The compiler-focused standard library and host interface are bundled with
  TinyML and sufficient to read sources, write output, and invoke a native
  toolchain.
- Stage 0 retains JavaScript as its only required backend and keeps only tools
  that validate the bootstrap path.
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

The repository has a parser, a Hindley–Milner-style checker, Lambda IR, source
module loading, and a JavaScript backend. Stage 0 now executes integration
tests for functions, recursion, closures, recursive ADTs, records, tuples,
patterns, fixed-width integers, strings, and the Node host boundary. Its
compiler-focused standard library is loaded from source and bundled into the
compiler. These prerequisites have now supported the first end-to-end bootstrap
proof: Stage 0 produces the JS compiler, that compiler produces a native
compiler, and the native compiler builds and runs a fixture. The native
checker still has incomplete HM inference, so full compiler typechecking and
the release gate remain work in progress.

The bootstrap has two compilers and two backend roles:

1. **Stage 0 TinyML compiler:** a Rust implementation that compiles the
   bootstrap-compatible TinyML source subset to runnable JavaScript.
2. **RiotML compiler implemented in TinyML:** the compiler source is written in
   that shared subset, so Stage 0 can compile it to JavaScript. Its frontend
   accepts the same compiler source as RiotML, and its native backend builds a
   native executable (C plus Clang is the current default candidate and has
   passed the end-to-end acceptance test).

The JS-hosted RiotML compiler must compile its own source into a native
RiotML compiler executable. That native executable must then compile a RiotML
program to native output that runs correctly. TinyML's Rust compiler does not
need a native backend; it only needs to produce the JavaScript bootstrap host.

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

The first native-compiler bootstrap follows this sequence:

```text
RiotML compiler implementation source (written in bootstrap TinyML subset)
        │
        ├── Rust Stage 0 TinyML compiler compiles it to riotml-compiler.js
        │
        └── Node runs riotml-compiler.js on that same compiler source
                    │
                    ├── uses native backend (C plus Clang)
                    └── produces riotml-compiler.native
                              │
                              └── compiles a RiotML fixture to native code
                                    └── program exits with status 42
```

The JS-hosted compiler and its native product implement the RiotML compiler;
Stage 0 remains Rust and only hosts the first executable. The source compiled
by the RiotML compiler must stay within the documented shared TinyML/RiotML
bootstrap subset so the compiler can compile itself. This is the required
native self-compilation proof: no weaker parser-only or C-output-only substitute
satisfies the acceptance bar.

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
system. `Host.args` excludes the executable name. Its initial contract is:

```text
Host.args         : unit -> List<string>
Host.read_file    : string -> Result<string, string>
Host.write_file   : string -> string -> Result<unit, string>
Host.write_stdout : string -> unit
Host.write_stderr : string -> unit
Host.run_process  : string -> List<string> -> Result<i32, string>
```

`run_process` returns the child exit code (including non-zero codes) as `Ok`;
spawn failures are `Error`. The JS target implements these operations with
Node APIs, including strict UTF-8 file decoding and `child_process` without
shell interpolation. The RiotML
native compiler links a host implementation of the same contract;
`run_process` lets it invoke the selected native toolchain (Clang in the
current C-plus-Clang implementation). Failures return `Result` or cause a
non-zero compiler exit, never a placeholder success. No general-purpose FFI
syntax or actor/runtime API is needed.

### 4. Backend strategy

The required Stage 0 output is a runnable program on the developer's host. The
Stage 0 target is JavaScript because it is the only current target with
integration tests that execute generated programs. TinyML's JS output is a
bootstrap artifact, not the Riot compiler's final output.

The Stage 0 Rust compiler emits JavaScript modules only. The native code
backend is implemented as part of the RiotML compiler written in TinyML. The
current default candidate is C plus Clang: emit C for the bootstrap compiler
source, invoke Clang through `Host.run_process` to create
`riotml-compiler.native`, then use that executable to compile a smoke-test
RiotML program and invoke Clang again. The end-to-end acceptance test validates
this route; it remains provisional through the release gate. Changing the
bootstrap architecture requires user approval.

The native backend must support every source construct used by the compiler
implementation itself, in addition to the smoke-test subset below. It is not
acceptable to compile only a parser or to stop after emitting C without
producing the native compiler executable. The Rust AArch64/SSA pipeline and its
placeholder runtime were removed in the Stage 0 trim. The current JS IR may be
simplified, but executable tests must continue to validate its behavior.

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

The compiler treats `main : unit -> i32` as the native entry point. Under the
current C-plus-Clang route it emits C and invokes Clang; running the result
must exit with status 42. An ill-typed fixture (for example,
`let main = fn () -> 1 + true`) must be rejected before native code generation.
The native backend still supports all constructs used by the compiler
implementation, even if this smoke program uses only a small part of the
language.

**Current implementation status:** the end-to-end test now compiles the
TinyML-written compiler to JavaScript, runs that JavaScript to compile the same
compiler source into a native executable, then uses the native executable to
build and run the status-42 fixture and reject `1 + true`. This validates the
C-plus-Clang route and the required acceptance chain. The native checker now validates import members and value/type/constructor
name existence, rejects statically visible primitive mismatches, and propagates
inferred primitive function types through selected imports. These checks cover
annotated calls and primitive-operator constraints on inferred parameters.
Generic and nominal types still use the `DynamicType` fallback, so this is not
full HM inference and some dynamic calls can evade mismatch checks. The
language/typechecking requirement above remains in force; this boundary still
needs strengthening.

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

TinyML Stage 0 is complete when all of the following hold:

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
5. The RiotML compiler implementation is written only in the documented
   TinyML/RiotML bootstrap-compatible subset and compiled by Stage 0 to JS.
6. Running that JS compiler on the same compiler source uses the current
   C-plus-Clang default candidate and produces a native RiotML compiler
   executable.
7. Running the native compiler on a RiotML smoke-test fixture emits a native
   program that compiles and exits with the expected status (42); invalid
   source produces a non-zero result and useful diagnostics.
8. The documented test command passes on the supported development host, and
   the known unsupported subset is stated plainly.

### 8. Current progress and remaining work

The Stage 0 language, module loader, compiler-focused library, and Node Host
contracts have positive and negative test coverage. The JS-to-native-to-smoke
program chain is exercised by
`riotml_compiler_emits_native_smoke_program_and_rejects_bad_types`. Remaining
work is on the native compiler's typechecking boundary and release portability:

- The source graph loader validates missing modules, cycles, and selected
  import members. The native checker resolves value/type/constructor names and
  propagates inferred primitive function types through imports, but its dynamic
  fallback does not yet implement the full rank-1 HM contract for generic and
  nominal bindings.
- Make runtime discovery independent of the repository working directory, then
  run the bootstrap flow from a clean checkout without generated artifacts.
- Keep the already removed LSP, pending WASM output, Rust AArch64 backend, and
  placeholder runtime out of Stage 0.

## Drawbacks
[drawbacks]: #drawbacks

- Choosing JavaScript makes Node a Stage 0 runtime dependency and adds a host
  artifact that is not itself native.
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

### Authoritative chain: JavaScript Stage 0; C plus Clang default candidate

This reuses the backend with the strongest existing evidence: generated modules
are executed by Node tests. TinyML can serve as the compiler implementation
language without requiring a native TinyML runtime. The RiotML compiler owns
native code generation; C plus Clang is the current default candidate, and the
JS-to-native-self-compile acceptance test validates that it can complete the
required chain. This native route remains provisional through the release gate.

### Rejected alternative: C as the Stage 0 host target

Stage 0 must emit JavaScript and run under Node for this bootstrap. Making C the
Stage 0 host target would change the required architecture rather than simplify
this implementation, and is therefore not a fallback within this RFD.

### Rejected alternative: make Stage 0 itself produce native code

This would require a separate native runtime and ABI before TinyML can compile
the RiotML compiler. The previous AArch64 experiment was removed because it had
placeholder runtime behavior and incomplete lowering. Native code generation
is required, but it belongs in the TinyML-written RiotML compiler; Stage 0
only needs to emit JavaScript.

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
- RFD0049 frames Riot ML as a language project and treats the current prototype
  as bootstrap material and evidence, rather than as a permanent product
  definition. This RFD narrows that broad direction to the concrete TinyML
  bootstrap contract.

## Decisions and remaining questions
[decisions-and-remaining-questions]: #decisions-and-remaining-questions

### Implementation contract recorded by this revision

- Stage 0 emits JavaScript and runs under Node; it does not compile TinyML to
  native code.
- The RiotML compiler's native backend currently uses C plus Clang as its
  default candidate. Its compiler implementation is restricted to the shared
  TinyML/RiotML bootstrap subset so it can compile its own source; the
  end-to-end self-compilation acceptance test verifies this route.
- Stage 0 supports all fixed-width signed and unsigned integers from 8 to 64
  bits, exact contextual integer literals (default `i32`), width-wrapping
  arithmetic, truncating division, and no floats or implicit conversions.
- Modules use snake-case `.ml` paths from dotted module names, source-local and
  ordered `-I` roots followed by bundled `lib/` and `lib/std/`, and an acyclic
  dependency graph.
- `Host` is a compiler-provided intrinsic module for arguments, file I/O,
  diagnostics/output, and safe process invocation; it is not arbitrary FFI.
- The completion proof requires the JS-hosted compiler to compile its own
  source into a native compiler executable, then use that native executable to
  compile a valid `main : unit -> i32` fixture, run it, and verify exit status
  42 while rejecting an ill-typed fixture.

### Remaining implementation-time details

- Complete HM inference and type propagation through local and imported
  bindings in the native RiotML checker; the current dynamic fallback is
  explicitly insufficient for the full language/typechecking contract.
- Make native runtime discovery independent of the repository working
  directory, then exercise the bootstrap from a clean checkout.
- Record/variant emission, the tagged C value representation, and the required
  List/String/Host operations now support the compiler-source acceptance path.
  Keep these surfaces constrained to RFD0050's shared subset and expand them
  only when a failing contract test requires it.

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
become useful to reduce dependence on Node or Clang or replace portions of Stage
0 with later compiler stages. Those are future bootstrap improvements, not
prerequisites for the initial native self-compilation proof. Actor support,
floating-point types, richer modules, and editor tooling should be designed for
Riot ML itself rather than pulled into TinyML by default.
