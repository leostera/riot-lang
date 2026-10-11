# RiotML compiler bootstrap source

This is the repository-root source tree for the RiotML compiler implemented in
TinyML. Stage 0 compiles it to JavaScript; running that compiler under Node then
compiles the same source graph to a native compiler using C and Clang. The
native compiler can compile and run a native RiotML program.

## End-to-end bootstrap

Run these commands from the repository root. They require Rust/Cargo, Node,
Clang, and the POSIX process APIs used by the C runtime.

```sh
mkdir -p build
cargo run -p tinyml -- compile -I riotc -o build/riotc.mjs riotc/main.ml
cat > build/run-compiler.mjs <<'EOF'
import { main } from "./riotc.mjs";
process.exitCode = Number(main(undefined));
EOF

# Stage 0-hosted compiler builds the native compiler from the same source.
node build/run-compiler.mjs compile riotc/main.ml -o build/riotml-compiler.native

# The native compiler builds and runs a native RiotML program.
printf 'let main = fn () -> 42\n' > build/smoke.ml
build/riotml-compiler.native compile build/smoke.ml -o build/smoke
build/smoke
```

The smoke executable exits with status 42. The integration test
`riotml_compiler_emits_native_smoke_program_and_rejects_bad_types` performs the
full chain in temporary directories. It verifies that the native compiler
builds this executable, runs it from an unrelated working directory, and
rejects invalid programs before leaving output executables behind. The Rust
Stage 0 compiler itself still emits JavaScript only.

## Native compiler slice and checking

The frontend loads and parses the complete reachable source-module graph before
checking or emitting C. `Check.Inference` performs rank-1 Hindley–Milner
inference over the compiler's supported native subset, preserving polymorphic
value schemes and nominal type identities across module boundaries. Local
`let` bindings and imported values are independently instantiated; constructors
and patterns are checked against their payload and nominal types. The native
entry point must have type `unit -> i32`.

The self-compilation test exercises both accepted and rejected HM cases: local
polymorphic identity at distinct types; imported polymorphic identity and a
generic nominal variant; mismatches between a nominal variant and a different
variant; ambiguous imports and local/imported name conflicts; duplicate value
and pattern bindings; bad primitive operators and function arguments; unknown
names/types; and incompatible match patterns. A rejected source fails before C generation,
returns a non-zero status, and leaves no plausible executable. The checker does
not use an unknown/dynamic-type escape hatch.

The RiotML native slice supports the constructs needed by the compiler source:
functions and closures, local bindings, applications, `i32`, booleans, strings,
tuples, records, named variants, and their supported patterns and matches. This
is narrower than Stage 0 TinyML's documented fixed-width integer contract; it
is not a claim that every Stage 0 type or operator has a native RiotML lowering.

`Host` and the bundled `String` module are connected to the native runtime;
Clang is invoked through `Host.run_process`. The compiler embeds its runtime
source directory in the generated native executable. At compile time it checks
the embedded directory, the current directory's `riotc/`, and the source
module's directory before using the repository-relative fallback. The
integration test verifies runtime discovery while the native compiler runs
outside the repository.

The compiler CLI currently accepts `compile SOURCE -o OUTPUT`. It resolves
imports relative to the source module, then uses the source root and bundled
TinyML library roots. C plus Clang remains provisional through the release gate;
changing the bootstrap architecture requires user approval.

## Source organization

- `syntax/ast.ml` and `syntax/parser/`: domain-specific source types and the
  lexer/parser, including complete source-module parsing.
- `check/names.ml`: value, type, constructor, import, and pattern-name checks.
- `check/inference.ml`: unification, generalization, instantiation, module
  interfaces, nominal constructor schemes, and native-entry validation.
- `modules.ml`: dependency-first module graph loading, missing-module
  reporting, and cycle detection.
- `codegen/c/emit.ml`: tagged-runtime C emission for source modules.
- `runtime.h`, `runtime.c`: tagged values, closures, module environments, and
  the native host runtime linked by Clang.
- `main.ml`: compiler pipeline, runtime discovery, and CLI.
