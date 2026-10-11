# RiotML compiler bootstrap source

This is the repository-root source tree for the RiotML compiler implemented in
TinyML. Stage 0 compiles it to JavaScript; running that compiler under Node then
compiles the same source graph to a native compiler using C and Clang. The
native compiler can compile and run a native RiotML program.

## End-to-end bootstrap

Run these commands from the repository root. They require Rust/Cargo, Node,
Clang, and the POSIX process APIs used by the provisional C runtime.

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
`riotml_compiler_emits_native_smoke_program_and_rejects_bad_types` performs
this full chain in temporary directories. It also checks that the native
compiler rejects `let main = fn () -> 1 + true` without leaving an output
executable. The Rust Stage 0 compiler itself still emits JavaScript only.

## Native compiler slice and current limits

The frontend loads and parses the complete reachable source-module graph before
checking or emitting C. The native backend includes functions and closures,
local bindings, applications, i32 and boolean expressions, strings, tuples,
records, named variants, and the corresponding patterns and matches used by the
compiler source. `Host` and the bundled `String` module are connected to the
native runtime; Clang is invoked through `Host.run_process`.

This is an implementation slice, not a claim that the RiotML typechecker is a
complete HM checker. It checks module imports and value/type/constructor name
existence, and its current value-type model includes i32, bool, string, unit,
functions, and a dynamic fallback for compiler-source constructs. It rejects
statically evident errors such as `1 + true`, unbound names, and mismatched
calls when a function parameter is annotated or its primitive uses in the
function body constrain the argument type. Inferred primitive function types
are propagated through selected imports, but generic and nominal types still
use the dynamic fallback; some dynamic calls can evade static mismatch checks.
Do not treat the current native checker as a general-purpose validation
boundary. The separate Rust Stage 0 TinyML checker
continues to provide the tested HM, module, primitive, and host contracts
documented in RFD0050.

The compiler CLI currently accepts `compile SOURCE -o OUTPUT`. It resolves
imports relative to the source module and the source root, followed by the
bundled TinyML library roots. Run the native compiler from the repository root
so its checked-in `riotc/runtime.c` is available to Clang; path-independent
runtime discovery remains future robustness work.

## Source organization

- `syntax/lexer.ml`: source tokenization and source spans.
- `syntax/ast.ml`: domain-specific names, types, patterns, expressions, and
  module declarations.
- `syntax/parser.ml` and `syntax/parser/`: entry parsing, complete module
  parsing, and smaller grammar components.
- `modules.ml`: dependency-first module graph loading, missing-module reporting,
  and cycle detection.
- `check/typer.ml`: expression checking and native entry validation.
- `check/names.ml`: source-module value/type/constructor name resolution.
- `lambda/ir.ml`, `lambda/lower.ml`: checked entry expressions to Lambda IR.
- `codegen/c/emit.ml`: C emission for standalone expressions and module graphs.
- `runtime.h`, `runtime.c`: tagged values, closures, module environments, and
  the native host runtime linked by Clang.
- `main.ml`: compiler pipeline and CLI.
