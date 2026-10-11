# RFD0050 - TinyML Bootstrap Compiler

- Feature Name: `tinyml_bootstrap_compiler`
- Start Date: `2026-10-11`
- Status: `presented`
- RFD PR: `(not yet opened)`
- Riot Issue: `(not yet opened)`

## Summary
[summary]: #summary

This RFD proposes narrowing TinyML to the smallest practical ML implementation
language for writing Riot's first compiler. The Rust TinyML compiler remains the
bootstrap compiler, but its required output is an executable host program—not a
native TinyML executable. The recommended first host target is JavaScript,
which already has meaningful end-to-end tests. A TinyML-written Riot compiler
can then contain its own native backend and emit native Riot programs.

- TinyML needs modules, HM inference, functions, recursive definitions, useful
  literals and operators, tuples, records, variants, pattern matching, and
  annotations.
- Cross-module `use` must resolve and typecheck values and types; a handful of
  parser-only import nodes is not enough.
- The implementation language needs a small bundled standard library and a
  narrow host interface for compiler I/O, not an actor runtime or general
  platform framework.
- Stage 0 should keep a single well-tested host backend and validation-focused
  commands; it does not need TinyML-native code generation, an LSP, or a
  production-quality optimizer.
- Floats, actors, distribution, package management, and a broad runtime remain
  out of scope.

## Motivation
[motivation]: #motivation

The payoff for TinyML is to make the first Riot compiler implementable in Riot's
own eventual implementation language. That requires more than parsing a few
expressions: the compiler will need to represent syntax and types, traverse
recursive data, split code across modules, and produce source or native output.
It does not require TinyML to become a complete application language or to
compile itself to native code.

The current repository already contains a substantial start: a parser, a
Hindley–Milner-style checker, Lambda and native IRs, a JavaScript backend, and an
AArch64 assembly path. `cargo test --workspace` passes, and integration tests
execute JavaScript output for functions, recursion, closures, matches, records,
and strings. Those are useful foundations, but the tests do not demonstrate a
working native compiler. The AArch64 tests snapshot emitted assembly; the
bundled native runtime returns placeholder values, and native match lowering
does not implement match arms. The standard-library files are not a coherent,
automatically loaded package, and some use syntax TinyML does not parse.

There are also frontend gaps behind the passing tests. For example, a direct
Stage 0 check of a match over a self-recursive `Expr` variant currently reports
type mismatches, so parser support and simple `Option` tests do not yet prove
that recursive compiler ASTs typecheck. This is exactly the kind of gap the
focused positive and negative corpus should expose and close.

The current feature surface obscures the actual bootstrap boundary. An
AArch64 backend, a JavaScript backend, an LSP, pending WASM snapshots, and
runtime layout contracts all make the project look closer to a production
compiler than it is. Finishing every path would delay the thing TinyML is meant
to unlock: a compiler written in TinyML.

The bootstrap has two distinct compilers and two distinct backend roles:

1. **Stage 0 TinyML compiler:** Rust implementation that compiles TinyML source
   into a runnable host program. Its first output target can be JavaScript.
2. **Riot compiler written in TinyML:** a program compiled by Stage 0. This
   program implements Riot parsing/checking and can contain a backend that
   emits C or native code for Riot programs.

The second compiler's native backend does not depend on TinyML having its own
native backend. Confusing those two responsibilities would cause TinyML to
rebuild a native pipeline that the first Riot compiler can implement itself.

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

The exact annotation syntax is illustrative. This describes the desired
subset; the current checker does not yet accept this self-recursive variant
example. What matters is that Stage 0 can typecheck algebraic data types,
recursive functions, local names, and pattern matching, rather than merely
parsing them.

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

For the first compiler run, the flow is:

```text
TinyML Riot compiler source
        │
        ├── Rust Stage 0 compiler compiles it to JavaScript
        │
        └── Node runs that JavaScript compiler on Riot source
                    │
                    └── Riot compiler emits C/native output
```

This proves that TinyML is useful for implementing the Riot compiler. It is not
a claim that the Rust Stage 0 compiler is self-hosting, or that TinyML code has
been compiled to native code. Native code generation belongs to the
TinyML-written Riot compiler when that compiler is ready to provide it.

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
  results, plus rank-1 Hindley–Milner inference and let-polymorphism. The
  polymorphic identity function must typecheck and be independently usable at
  distinct types. Type errors must be rejected by the compiler entrypoint.
- **Primitive values:** `unit`, `bool`, strings/bytes, and fixed-width signed
  and unsigned integer types. The intended integer family is `i8`, `i16`,
  `i32`, `i64`, `u8`, `u16`, `u32`, and `u64`; floating-point types are
  deferred. Integer literal range checking, defaulting, and overflow behavior
  must be made explicit before the numeric subset is considered complete.
- **Operators:** arithmetic and comparisons over supported integer types, with
  a clear rule that operands have compatible types. String operations should
  be ordinary standard-library functions unless a primitive operation is
  required by the host implementation.

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

`use Module.{value, Type, Constructor}` resolves to an actual source module or
its checked interface. Import selection is validated, imported value schemes
are instantiated normally, and imported nominal types preserve their
constructor schemes, tags, and fields. Names from unrelated imports must not
be injected indiscriminately into the same scope.

Initially, module compilation can be whole-program and dependency ordered.
There is no requirement for a stable public binary interface format. If cached
interfaces are useful, they are an implementation detail that can be rebuilt
from source and versioned independently. The compiler does not need a package
registry, dependency solver, conditional compilation, or complex visibility
rules.

### 3. Bundled standard library and host boundary

TinyML ships a small standard library as part of this compiler project. Its
initial purpose is to support compiler implementations, not to mirror the
future Riot standard library. It should include only what the compiler needs,
which is expected to include basic list/option/result types and operations plus
usable string/byte operations.

The host boundary should remain small and explicit. At minimum, a compiler
program needs to read source files and write generated output; it may also need
command-line arguments and diagnostic output. A Stage 0 `Host` module can
provide those operations, with a JavaScript implementation using Node APIs.
This does not require arbitrary FFI syntax or a general runtime. If later
backends need equivalent host operations, they implement the same narrow
contract.

### 4. Backend strategy

The required Stage 0 output is a runnable program on the developer's host. The
proposal recommends retaining JavaScript as the first required target because
it is the only current target with integration tests that execute generated
programs. TinyML's JS output should be treated as a bootstrap artifact, not as
the Riot compiler's final output.

The Riot compiler, once written in TinyML, may emit C and invoke Clang, emit
AArch64 directly, or use another native route. That is a separate backend
choice and should be selected based on the smallest implementation that can
produce correct native Riot programs.

The current TinyML AArch64 pipeline may be kept as an experiment, but it is not
part of the Stage 0 completion contract unless it gains end-to-end tests and a
real runtime. Existing Lambda/SSA/MIR code is reusable only where it reduces
work; Stage 0 does not promise to preserve those IRs or target APIs. The same
applies to the current JS IR: it may be simplified, but its executed behavior
is valuable evidence and should not be discarded without a replacement test.

### 5. Validation-focused CLI

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

### 6. Completion criteria

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
5. A meaningful first Riot compiler slice is written in TinyML, compiled by
   Stage 0, and run on Riot source. The slice should parse and typecheck a
   documented Riot subset and emit native output for a smoke-test program.
6. The generated native smoke-test program runs and produces the expected
   result. This proves the TinyML-written compiler can produce native output;
   it does not require TinyML itself to have a native backend.
7. The documented test command passes on the supported development host, and
   the known unsupported subset is stated plainly.

### 7. Migration from the current implementation

The implementation should first establish the behavior that already works,
then remove or defer work that does not serve the contract:

- Keep the parser/checker features that meet the language contract; add tests
  before treating existing syntax as supported.
- Complete module loading and imported-type/value checking, rather than counting
  `use` parsing or interface serialization as module support.
- Treat the existing JS backend as the likely Stage 0 host target and extend
  only what is needed for the compiler's Host API and language semantics.
- Do not invest further in LSP or pending WASM work for Stage 0.
- Keep the AArch64 pipeline only if it can be tested as executable semantics;
  otherwise leave it isolated or remove it without blocking the bootstrap.
- Replace the placeholder standard-library and runtime boundary with the small
  bundled library and explicit host functions above.

## Drawbacks
[drawbacks]: #drawbacks

- Choosing JavaScript makes Node a Stage 0 runtime dependency and adds a host
  artifact that is not itself native.
- JavaScript number semantics do not directly preserve all fixed-width signed
  and unsigned integers, especially 64-bit values. A correct representation
  and operator strategy must be selected; silently using imprecise `Number`
  values is unacceptable.
- A minimal language can still grow unexpectedly when compiler code needs
  containers, bytes, error handling, or convenient syntax. The completion
  criteria need to prevent these needs from becoming a rationale for a broad
  general-purpose runtime.
- Deferring the current AArch64 backend may discard work that could eventually
  be useful. Keeping it as a required target, however, risks making an
  unvalidated implementation the critical path.
- The first Riot compiler's native backend will need its own correct ABI,
  runtime/host boundary, and end-to-end tests. TinyML Stage 0 does not solve
  those problems automatically.

## Rationale and alternatives
[rationale-and-alternatives]: #rationale-and-alternatives

### Recommended: JavaScript as the Stage 0 host target

This reuses the backend with the strongest existing evidence: generated modules
are executed by Node tests. It allows TinyML to serve as the implementation
language while the Riot compiler separately develops a native backend. It
avoids requiring the TinyML native runtime to work before compiler development
can start.

### Alternative: C as the Stage 0 host target

C plus Clang can yield a native Stage 0 compiler and has a simple deployment
story. It is a reasonable fallback if Node-hosted file I/O or integer
representation becomes a blocker. It still requires a C emitter, a runtime
contract, and end-to-end tests, and therefore is not automatically less work
than using the tested JS backend.

### Alternative: finish TinyML's AArch64 backend first

This could produce a native Stage 0 compiler without a C compiler dependency.
However, the current pipeline has placeholder runtime behavior and incorrect or
missing lowering for important constructs. It would require enough backend,
ABI, runtime, and execution-test work to delay the Riot compiler—the very work
that the TinyML-written compiler is meant to enable. It should be chosen only
if end-to-end measurements show that it is the shortest path, not because the
IR stages already exist.

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

- The current `tinyml` crate already separates parsing, checking, Lambda IR,
  native IR, and backend lowering. Its JavaScript integration tests provide a
  concrete path to reuse, while native snapshots demonstrate why output
  snapshots alone are insufficient evidence.
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

## Unresolved questions
[unresolved-questions]: #unresolved-questions

### To resolve before acceptance

- Is JavaScript the agreed Stage 0 execution target, with Node as an explicit
  host dependency, or should the proposal require native C output from TinyML
  itself?
- Which signed/unsigned integer widths must Stage 0 provide? The proposal lists
  8-, 16-, 32-, and 64-bit types, but the smallest useful set may be smaller.
- What are integer literal defaulting, overflow, division, and signedness rules?
  These must be consistent across typechecking, JavaScript output, and any
  native backend implemented by the Riot compiler.
- Should the compiler-facing host API be an intrinsic `Host` module, or should
  Stage 0 add a small foreign-function declaration form?
- What exact Riot compiler vertical slice is enough to prove Stage 0 useful?
  The proposal expects parsing, checking, and native output for a small source
  subset, but the first subset should be named before implementation starts.

### To resolve during implementation

- Exact surface syntax for top-level annotations, record declarations, module
  paths, and module search paths.
- Whether module interfaces are loaded from source on every build or cached in
  a small versioned artifact.
- The minimal List/String/Byte operations needed by the Riot compiler's first
  implementation.
- Whether current Lambda/SSA/AArch64 code can be retained without delaying the
  chosen Stage 0 host target.

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

After the Riot compiler exists and has a working native backend, it may become
useful to compile that compiler to native code, reduce dependence on Node or
Clang, or replace portions of Stage 0 with later compiler stages. Those are
future bootstrap improvements, not prerequisites for starting Riot compiler
work. Actor support, broader numeric types, richer modules, and editor tooling
should be designed for Riot ML itself rather than pulled into TinyML by default.
