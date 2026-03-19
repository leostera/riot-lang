# raml-core

Core OCaml value representation for the RaML runtime.

## Overview

This crate provides the fundamental types and abstractions for working with OCaml values. It implements OCaml's tagged pointer representation and block structure.

**Note**: For FFI usage with derive macros, use the `raml-ffi` crate instead, which re-exports everything from `raml-core` plus provides `#[derive(Value)]`.

## Value Representation

OCaml values use a tagged pointer representation:
- **Integers**: 63-bit signed integers (on 64-bit systems) with LSB = 1
- **Pointers**: Aligned pointers to heap blocks with LSB = 0

```rust
use raml_core::prelude::*;

let v = Value::int(42);
assert!(v.is_int());
assert_eq!(v.as_int(), 42);

assert_eq!(VAL_UNIT.as_int(), 0);
```

## Heap Blocks

Heap-allocated values are represented as blocks with:
- **Header**: Contains size, tag, and GC color
- **Fields**: Array of Value pointers

```rust
use raml_core::prelude::*;

let header = BlockHeader::new(3, Tag::CONS, GcColor::White);
assert_eq!(header.size(), 3);
assert_eq!(header.tag(), Tag::CONS);
```

## Usage

Add to your `Cargo.toml`:

```toml
[dependencies]
raml-core = { path = "../raml-core" }
```

Import the prelude:

```rust
use raml_core::prelude::*;
```

## Examples

Run the basic usage example:

```bash
cargo run -p raml-core --example basic_usage
```

## Architecture

This crate is part of the RaML (Rust Abstract Machine Layer) project:

```
native/
├── raml-core/     # Core value representation (THIS CRATE)
├── raml-ffi/      # FFI facade with derive macros (re-exports raml-core)
├── raml-rt/       # Bytecode runtime (uses raml-core)
└── raml-kernel/   # Executable runner
```

## License

Same as parent project.
