# RFD0050 Implementation Checklist: TinyML Bootstrap Compiler

This checklist tracks implementation of [RFD0050](RFD0050-tinyml-bootstrap.md).
The RFD is the design authority. Update it and this checklist together if the
user-approved contract changes.

## Phase 0: Trim nonessential Stage 0 surface — complete

- [x] Remove the Stage 0 LSP and its dependencies.
- [x] Remove pending WASM output and unused snapshots.
- [x] Remove the incomplete Stage 0 AArch64/native backend, SSA pipeline, and
  placeholder runtime crate. This does **not** remove the required native
  backend from the TinyML-written RiotML compiler.
- [x] Retain the JavaScript backend and validation-focused CLI.
- [x] Update the Neovim plugin to provide syntax support without starting an
  LSP.
- [x] Prune dependencies and tests that only served removed targets.

**Exit:** Stage 0 has one supported output target and no partial LSP, WASM, or
native pipeline. Completed in commit `52d32dec6` (`refactor(tinyml): trim Stage
0 to JavaScript`); `cargo test --workspace` passed.

## Phase 1: Lock the bootstrap and language contract

- [x] Stage 0 is Rust and emits JavaScript for Node; TinyML itself has no native
  backend requirement.
- [x] Require a JS-hosted RiotML compiler to compile its own source to a native
  RiotML compiler executable; that executable must compile and run a native
  program. The native self-compilation acceptance bar is unchanged.
- [x] Define `i8/i16/i32/i64` and `u8/u16/u32/u64`; integer literals use an
  expected type or default to `i32`, are range-checked, and arithmetic wraps at
  width. Signed division truncates toward zero; division by zero traps; there
  are no implicit signedness conversions. Floats are out of scope.
- [x] Record annotation syntax: `(expr: Type)`, `(pattern: Type)`, and
  `let pattern: Type = expr`; constrain a function result by ascribing its body.
- [x] Preserve execution-tested `string + string` concatenation; keep other
  string/byte operations in the standard library.
- [x] Define dotted-module-to-snake-case `.ml` paths, search-root order,
  selected imports, and acyclic dependency graphs.
- [x] Specify the intrinsic `Host` API for arguments, file I/O, output, and
  process invocation. It is not general FFI.
- [x] Define the first RiotML user-program smoke slice: `main : unit -> i32`,
  source `let main = fn () -> 42`, native executable exit status 42, and
  rejection of ill-typed input before code generation.
- [x] Record C plus Clang as the default native-backend candidate, not a
  finalized choice; validate before finalizing. Ask the user before any
  bootstrap architecture change or reduction of completion criteria.
- [x] Update RFD0050 to make the two-step JS-to-native-compiler bootstrap
  authoritative and distinguish the RiotML backend from Stage 0.

**Exit:** the implementation target, language rules, module policy, host
boundary, and native self-compilation proof are recorded in RFD0050. Remaining
ABI/layout and library-operation details are implementation decisions that must
preserve the contract.

## Phase 2: Establish the compiler acceptance corpus

- [ ] Keep all TinyML source fixtures under `tinyml/tests/fixtures/` and organize
  them by the behavior they demonstrate.
- [ ] Add positive fixtures for polymorphic identity, recursion, annotations,
  every integer type, tuples, records, recursive variants, and patterns.
- [ ] Add negative fixtures for type mismatches, invalid annotations,
  out-of-range literals, unknown names/types/constructors, duplicate
  declarations, invalid imports, and incomplete matches as required.
- [ ] Make checker tests assert success/failure and diagnostic kinds; snapshots
  alone must not make invalid input appear accepted.
- [ ] Add a regression test for the recursive `Expr` match in RFD0050 and
  deliberately ill-typed near-neighbors.
- [ ] Add executable JS coverage for recursion, ADTs, and patterns.

**Exit:** tests distinguish accepted programs from rejected ones and exercise
the constructs needed by the compiler implementation.

## Phase 3: Make HM inference reliable for compiler data

- [ ] Fix and test type mismatches for self-recursive nominal variants such as
  `Expr`.
- [ ] Specify top-level recursion as monomorphic during body inference,
  followed by generalization at the declaration boundary.
- [ ] Verify constructor schemes preserve type parameters and instantiate
  independently at every use.
- [ ] Verify let-polymorphism with identity at unrelated types, including an
  imported identity function.
- [ ] Unify annotations with inferred types; never replace an inferred type
  silently after a mismatch.
- [ ] Ensure `check` and `compile` fail on any checker error before lowering or
  code generation.
- [ ] Test duplicate, unknown, arity, and match-coverage diagnostics required
  by the language contract.

**Exit:** the positive/negative checker corpus passes and invalid programs are
rejected before code generation.

## Phase 4: Resolve source modules and ship library modules — complete

- [x] Resolve a deterministic source-module graph from the root module, `use`
  declarations, and ordered include directories.
- [x] Validate selected imported values, types, constructors, and fields;
  expose only selected names and detect conflicts.
- [x] Preserve imported schemes and nominal constructor/record metadata.
- [x] Report missing modules/members and dependency cycles; compile dependencies
  in stable order.
- [x] Make bundled-library lookup independent of the working directory and
  embed library source files in the compiler distribution.
- [x] Retain only `tinyml/lib/` modules that parse, typecheck, and serve the
  compiler. Exclude unsupported Actor, argument-parser, and aggregation
  sketches from the default library.
- [x] Add multi-module integration tests for a polymorphic import, recursive
  ADTs, record metadata, bundled modules, and dependency JavaScript output.

**Exit:** programs compile against their own modules and supported bundled
modules without manually generated interface files. Verified by the module
loader tests, `tinyml/tests/module_tests.rs`, and the CLI tests that check
source imports and execute a dependency-linked JS program.

## Phase 5: Close primitive and host boundaries

- [ ] Implement all agreed fixed-width integer types through parsing,
  validation, inference, lowering, and JS code generation.
- [ ] Preserve exact semantics for all widths in JS; use exact integers and
  width-aware operations rather than lossy `Number` arithmetic.
- [ ] Implement literal range checks, contextual typing/defaulting, arithmetic
  wrap behavior, division, and comparison semantics.
- [ ] Reject floating-point types consistently.
- [ ] Specify string/byte representation and implement only operations needed
  by the compiler, with positive and negative tests.
- [ ] Implement and test the `Host` contract for arguments, file reads/writes,
  stdout/stderr, and safe process execution in the JS target.
- [ ] Test host failures as diagnostics/non-zero exits rather than placeholder
  values.

**Exit:** compiler code can read source, process it with exact primitive
semantics, and write output through the explicit host boundary.

## Phase 6: Complete the Stage 0 JavaScript path

- [ ] Compile reachable TinyML modules to JS with stable module imports/exports.
- [ ] Execute tests for recursion, closures, ADT tags/payloads, records, tuples,
  matches, integers, and strings.
- [ ] Diagnose unsupported constructs rather than emitting plausible incorrect
  JS.
- [ ] Ensure compiled programs can use the Stage 0 `Host` implementation.
- [ ] Verify `tinyml compile` honors output paths, resolves the bundled library
  independently of the current directory, and fails on compile errors.
- [ ] Document the validation CLI and exact command behavior.
- [ ] Add an end-to-end test that compiles a TinyML program with Rust Stage 0,
  runs it under Node, and checks output and exit status.

**Exit:** compiler-source-sized TinyML programs compile to and run correctly as
JavaScript using the supported Stage 0 path.

## Phase 7: Prove native self-compilation of the RiotML compiler

- [ ] Write the RiotML compiler implementation in TinyML using only the shared
  bootstrap subset; place it in the compiler project's chosen source tree.
- [ ] Compile the complete implementation with Rust Stage 0 to JS.
- [ ] Run the JS compiler on the same implementation source and produce a
  native RiotML compiler executable; do not substitute C output or parser-only
  behavior for this proof.
- [ ] Validate C plus Clang as the default candidate for the RiotML native
  backend. If it fails the smallest-correctness/work test, stop for user input
  before changing the native-backend route or architecture.
- [ ] Use the native compiler on the `main : unit -> i32` fixture, build the
  result, run it, and verify exit status 42.
- [ ] Test an ill-typed RiotML program and verify a useful diagnostic, non-zero
  status, and no misleading output executable.
- [ ] Record exact commands and dependencies for a clean-checkout bootstrap.

**Exit:** Stage 0-produced JS compiles the RiotML compiler source into a native
compiler, and that native compiler emits a runnable native program with the
expected behavior.

## Phase 8: Release gate

- [ ] Document the supported TinyML/RiotML subset, host requirements, library
  layout, and explicit non-goals.
- [ ] Run `cargo fmt --all -- --check` and `cargo test --workspace` from the
  repository root.
- [ ] Repeat the documented bootstrap from a clean checkout with no
  untracked/generated prerequisites.
- [ ] Exercise every supported standard-library module; remove or clearly
  exclude unsupported sketches.
- [ ] Confirm there are no generated artifacts or stale snapshots and that the
  test suite is deterministic.

**Done:** all release-gate tasks pass, and native RiotML self-compilation is
repeatable from a clean checkout.
