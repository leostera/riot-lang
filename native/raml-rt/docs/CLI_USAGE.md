# RAML-RT CLI Usage

## Installation

```bash
cargo build --release
# Binary will be at: target/release/raml-rt
```

## Commands

### `raml-rt run <file>`

Run an OCaml bytecode file.

```bash
# Run a .cmo file (when parser is complete)
raml-rt run program.cmo

# Run an executable
raml-rt run program.out
```

**Example:**
```bash
$ raml-rt run test_simple.cmo
Loading bytecode from: test_simple.cmo
✓ Loaded successfully!
  Code instructions: 12
  Primitives: 0

Running bytecode...
─────────────────────────────────────────
42
─────────────────────────────────────────
✓ Execution completed successfully!
Final result: Int(42)
```

### `raml-rt info <file>`

Display detailed information about a bytecode file.

```bash
raml-rt info program.cmo
```

**Example output:**
```
═══════════════════════════════════════════
  Bytecode File Information
═══════════════════════════════════════════

File: test_simple.cmo

Code Section:
  Instructions: 12 words (48 bytes)

First 20 instructions:
  [   0] 1728053248 (0x67000000)
  [   1]  704643072 (0x2a000000)
  ...

Data Section:
  Global values: 0

Primitives Section:
  Primitives: 3
  [  0] caml_ml_output_int
  [  1] caml_ml_output_string
  [  2] caml_ml_output_char

Symbols Section:
  Debug symbols: 0

═══════════════════════════════════════════
```

### `raml-rt --version`

Show version and feature information.

```bash
$ raml-rt --version
raml-rt 0.1.0
OCaml bytecode interpreter written in Rust

Features:
  - 137/140 opcodes implemented (98%)
  - Generational garbage collector
  - Effect handlers (delimited continuations)
  - WASM compilation support
```

### `raml-rt --help`

Show usage information.

## Current Limitations

### .cmo Files
Currently, .cmo file loading requires completing the OCaml marshal parser to extract bytecode positions from the compilation_unit structure. The `info` command will work, but `run` may fail with invalid opcodes.

**Workaround**: Use executable files (.out) or hand-crafted bytecode for now.

### Executable Files with Shebang
Script executables (starting with `#!/...`) are partially supported but may fail. 

**Workaround**: Compile to standalone bytecode or use .cmo files.

## Working Examples

### Hand-Crafted Bytecode

Create a simple bytecode file:
```bash
# This will work once we support raw bytecode files
# For now, use the examples in the source code
```

### OCaml Executable

```bash
# Compile to executable
ocamlc -o test.out test.ml

# Run with raml-rt
raml-rt run test.out
```

## Development

### Running from Source

```bash
cargo run -- run test.cmo
cargo run -- info test.cmo
cargo run -- --version
```

### Building

```bash
# Debug build
cargo build

# Release build (optimized)
cargo build --release

# With WASM target
wasm-pack build --target web
```

## Exit Codes

- `0` - Success
- `1` - Error (file not found, parse error, runtime error, etc.)

## Environment

The CLI uses standard input/output:
- `stdout` - Program output
- `stderr` - Runtime messages, errors, debug info

All runtime messages (loading, execution status) go to stderr, so you can redirect stdout cleanly:

```bash
raml-rt run program.cmo > output.txt
# stderr shows progress
# stdout has program output only
```

## Future Enhancements

- [ ] Raw bytecode file support (`.byte`)
- [ ] Complete .cmo loading with marshal parser
- [ ] .cma archive support
- [ ] Interactive REPL mode
- [ ] Disassembly mode (`raml-rt disasm <file>`)
- [ ] Debugging support
- [ ] Performance profiling
