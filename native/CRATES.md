# Native Crates

Overview of the RaML (Rust Abstract Machine Layer) crate structure.

## Crates

### 1. raml-core
**Core OCaml value representation**

- Pure Rust implementation of OCaml's value model
- Tagged pointers, blocks, GC colors, tags
- No dependencies, fast compile times
- Use for low-level control

```rust
use raml_core::prelude::*;

let x = Value::int(42);
let header = BlockHeader::new(3, Tag::CONS, GcColor::White);
```

**Dependencies**: None  
**Used by**: raml-ffi, raml-rt

---

### 2. raml-derive
**Procedural macros for derive**

- `#[derive(Value)]` for automatic conversions
- Generates `Into<Value>` and `TryFrom<Value>`
- Supports structs and enums

```rust
#[derive(Value)]
struct Point { x: i32, y: i32 }

#[derive(Value)]
enum Color { Red, Green, Blue, RGB(u8, u8, u8) }
```

**Dependencies**: syn, quote, proc-macro2  
**Used by**: raml-ffi

---

### 3. raml-ffi
**High-level FFI facade**

- Re-exports raml-core + raml-derive
- `#[derive(Value)]` for automatic conversions
- Recommended for most users

```rust
use raml_ffi::prelude::*;

#[derive(Value)]
struct Point { x: i32, y: i32 }

let point = Point { x: 10, y: 20 };
let val: Value = point.into();
```

**Dependencies**: raml-core, raml-derive  
**Used by**: User code

---

### 4. raml-bindgen
**OCaml binding generator**

- CLI tool to generate OCaml bindings from Rust
- Analyzes `#[no_mangle]` functions and `#[derive(Value)]` types
- Generates `.ml` and `.mli` files
- Foundation for writing native OCaml extensions in Rust

```bash
raml-bindgen path/to/crate --generate-mli
```

Generates:
```ocaml
external add : int -> int -> int = "rust_add"
external point_new : int -> int -> 'a = "rust_point_new"
```

**Dependencies**: syn, quote, clap, walkdir  
**Used by**: Library developers writing OCaml extensions

---

### 5. raml-rt
**OCaml bytecode runtime**

- Interprets OCaml bytecode
- 295 primitives (63% coverage)
- Memory management, GC, fibers
- 20,000+ lines of Rust

```rust
use raml_rt::Runtime;

let mut rt = Runtime::new();
rt.load_bytecode(&bytecode)?;
rt.run()?;
```

**Dependencies**: raml-core  
**Used by**: raml-kernel

---

### 6. example-lib
**Example native library**

- Demonstrates writing OCaml-callable Rust code
- Shows `#[unsafe(no_mangle)]` usage
- Includes generated OCaml bindings

```rust
#[unsafe(no_mangle)]
pub extern "C" fn add(a: i32, b: i32) -> i32 {
    a + b
}
```

**Dependencies**: raml-ffi  
**Used by**: Developers as reference

---

## Dependency Graph

```
Runtime Stack:
  raml-kernel
      └── raml-rt
          └── raml-core

FFI Stack:
  raml-ffi
    ├── raml-core (types)
    └── raml-derive (proc macros)
        └── syn, quote, proc-macro2

Tools:
  raml-bindgen
    └── syn, quote, clap, walkdir
    
  example-lib
    └── raml-ffi
```

**Dependencies**: raml-rt  
**Used by**: End users

---

## Dependency Graph

```
raml-kernel
    └── raml-rt
        └── raml-core
            (no dependencies)

raml-ffi
    └── raml-core
        (no dependencies)
```

## Future Additions

### raml-derive (planned)
Proc macro for `#[derive(Value)]`

```
raml-ffi
  ├── raml-core (types)
  └── raml-derive (macros)
```

---

## Quick Reference

| Need | Use |
|------|-----|
| Low-level value types | `raml-core` |
| Derive macros only | `raml-derive` |
| FFI with derives | `raml-ffi` ✨ |
| Generate OCaml bindings | `raml-bindgen` 🔧 |
| Run OCaml bytecode | `raml-rt` |
| CLI executable | `raml-kernel` |
| Example reference | `example-lib` |

## Examples

```bash
# Core value usage
cargo run -p raml-core --example basic_usage

# FFI usage
cargo run -p raml-ffi --example ffi_usage

# Run OCaml bytecode
cargo build -p raml-kernel
./target/debug/raml-kernel program.cmo
```

## Statistics

- **raml-core**: ~600 LOC, 5 tests, 0 dependencies
- **raml-derive**: ~260 LOC, proc macro, syn/quote deps
- **raml-ffi**: ~50 LOC, re-exports core + derive
- **raml-bindgen**: ~800 LOC, CLI tool, syn/clap deps
- **raml-rt**: ~20,000 LOC, 295 primitives
- **raml-kernel**: ~100 LOC, CLI wrapper
- **example-lib**: ~80 LOC, reference implementation

Total workspace: ~21,890 LOC
