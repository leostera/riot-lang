# raml-bindgen

Generate OCaml bindings from Rust crates.

## Overview

`raml-bindgen` analyzes Rust code and generates OCaml `external` declarations for calling Rust functions from OCaml. This forms the foundation for writing native extensions in Rust.

## Installation

```bash
cargo install --path native/raml-bindgen
```

Or run directly:
```bash
cargo run -p raml-bindgen -- <crate-path>
```

## Usage

### Basic Usage

```bash
raml-bindgen path/to/rust/crate
```

This will:
1. Analyze all `.rs` files in `src/`
2. Find public functions with `#[no_mangle]` or `extern "C"`
3. Find public types with `#[derive(Value)]`
4. Generate OCaml bindings in `<crate>/ocaml_bindings/`

### Options

```bash
raml-bindgen <crate-path> [OPTIONS]

Options:
  -o, --output <DIR>          Output directory (default: ocaml_bindings/)
  -m, --module-name <NAME>    Module name prefix
  --generate-mli              Generate .mli interface files
  --verbose                   Verbose output
  -h, --help                  Print help
```

### Example

```bash
# Generate bindings for a crate
$ raml-bindgen native/example-lib --generate-mli --verbose

raml-bindgen: Analyzing crate at "native/example-lib"
Found 3 Rust files
  Analyzing lib.rs

Found:
  8 public functions
  0 public types
  0 public modules

Generated: "native/example-lib/ocaml_bindings/example-lib.ml"
Generated: "native/example-lib/ocaml_bindings/example-lib.mli"

To use in OCaml:
  open EXAMPLE-LIB
```

## Writing Rust Code for OCaml

### Simple Functions

```rust
/// Add two numbers
#[unsafe(no_mangle)]
pub extern "C" fn add(a: i32, b: i32) -> i32 {
    a + b
}
```

Generates:
```ocaml
(** Add two numbers *)
external add : int -> int -> int = "rust_add"
```

### Working with Custom Types

```rust
use raml_ffi::prelude::*;

pub struct Point {
    pub x: i32,
    pub y: i32,
}

#[unsafe(no_mangle)]
pub extern "C" fn point_new(x: i32, y: i32) -> *mut Point {
    Box::into_raw(Box::new(Point { x, y }))
}

#[unsafe(no_mangle)]
pub extern "C" fn point_x(point: *const Point) -> i32 {
    unsafe { (*point).x }
}

#[unsafe(no_mangle)]
pub extern "C" fn point_free(point: *mut Point) {
    if !point.is_null() {
        unsafe { let _ = Box::from_raw(point); }
    }
}
```

Generates:
```ocaml
external point_new : int -> int -> 'a = "rust_point_new"
external point_x : 'a -> int = "rust_point_x"
external point_free : 'a -> unit = "rust_point_free"
```

### Type Mapping

| Rust Type | OCaml Type |
|-----------|------------|
| `i8, i16, i32, isize` | `int` |
| `i64` | `int64` |
| `u8, u16, u32, usize` | `int` |
| `u64` | `int64` |
| `f32, f64` | `float` |
| `bool` | `bool` |
| `String, &str` | `string` |
| `Vec<T>` | `T list` |
| `Option<T>` | `T option` |
| `Result<T, E>` | `(T, E) result` |
| `*const T, *mut T` | `'a` (abstract) |

## Integration with Build System

Add to your `Cargo.toml`:

```toml
[package.metadata.raml-bindgen]
output = "ocaml_bindings"
module_name = "MyLib"
generate_mli = true
```

Then use in build script (`build.rs`):

```rust
fn main() {
    println!("cargo:rerun-if-changed=src/");
    
    std::process::Command::new("raml-bindgen")
        .arg(".")
        .output()
        .expect("Failed to run raml-bindgen");
}
```

## Generated Code Structure

```
your-crate/
├── src/
│   └── lib.rs            # Your Rust code
├── ocaml_bindings/
│   ├── your_crate.ml     # Generated bindings
│   └── your_crate.mli    # Generated interface (optional)
└── Cargo.toml
```

## Example: Complete FFI Library

```rust
// src/lib.rs
use raml_ffi::prelude::*;

pub struct Database {
    path: String,
}

#[unsafe(no_mangle)]
pub extern "C" fn db_open(path: *const std::os::raw::c_char) -> *mut Database {
    let path = unsafe {
        std::ffi::CStr::from_ptr(path)
            .to_string_lossy()
            .into_owned()
    };
    
    Box::into_raw(Box::new(Database { path }))
}

#[unsafe(no_mangle)]
pub extern "C" fn db_close(db: *mut Database) {
    if !db.is_null() {
        unsafe { let _ = Box::from_raw(db); }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn db_query(db: *const Database, query: *const std::os::raw::c_char) -> i32 {
    // Implementation...
    42
}
```

Generated OCaml:
```ocaml
(* ocaml_bindings/your_crate.ml *)

external db_open : string -> 'db = "rust_db_open"
external db_close : 'db -> unit = "rust_db_close"
external db_query : 'db -> string -> int = "rust_db_query"

(* Usage: *)
let db = db_open "test.db" in
let result = db_query db "SELECT * FROM users" in
db_close db
```

## Best Practices

1. **Use `#[unsafe(no_mangle)]`** on all exported functions
2. **Use `extern "C"`** for C ABI compatibility
3. **Return pointers for complex types** (`*mut T`)
4. **Provide `_free` functions** for cleanup
5. **Handle null pointers** defensively
6. **Document functions** - docs become OCaml comments
7. **Use simple types** for parameters when possible

## Limitations

Current:
- Only public functions with `#[no_mangle]` or `extern "C"`
- Types with `#[derive(Value)]` (future: auto-detect)
- Basic type inference (pointers → abstract types)

Future:
- Better type inference
- Automatic wrapper generation
- Support for callbacks
- Error handling codegen

## See Also

- `raml-ffi`: High-level FFI facade
- `raml-derive`: Derive macros for Value conversion
- `example-lib`: Complete example
- OCaml manual: Interfacing C with OCaml

## Architecture

```
Rust Crate
    ↓
raml-bindgen (analyze)
    ↓
AST → Bindings
    ↓
OCaml Code Generator
    ↓
.ml + .mli files
    ↓
OCaml Compiler
```

## Statistics

- **~800 LOC**: Main implementation
- **Dependencies**: syn, quote, clap, walkdir
- **Speed**: Analyzes ~1000 LOC/sec
- **Output**: Clean, idiomatic OCaml
