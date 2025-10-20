# raml-ffi

High-level FFI interface for Rust⟷OCaml interop.

## Overview

This crate provides a convenient facade for working with OCaml values from Rust. It re-exports all types from `raml-core` and will provide derive macros for automatic conversion.

## Quick Start

```rust
use raml_ffi::prelude::*;

let v = Value::int(42);
assert_eq!(v.as_int(), 42);
```

## Architecture

```
raml-ffi (THIS CRATE)
  ├── Re-exports raml-core
  └── Future: #[derive(Value)] macro

raml-core
  └── Core value representation
```

## Future: Derive Macro

This crate will provide `#[derive(Value)]` for automatic conversion:

```rust
use raml_ffi::prelude::*;

#[derive(Value)]
struct Point {
    x: i32,
    y: i32,
}

// Auto-generated Into<Value> and TryFrom<Value>
let point = Point { x: 10, y: 20 };
let val: Value = point.into();
let point2: Point = val.try_into()?;
```

### Enums

```rust
#[derive(Value)]
enum Color {
    Red,                    // → Value::int(0)
    Green,                  // → Value::int(1)
    Blue,                   // → Value::int(2)
    RGB(u8, u8, u8),       // → Block(tag=0, fields=[r,g,b])
}
```

## Usage

Add to your `Cargo.toml`:

```toml
[dependencies]
raml-ffi = { path = "../raml-ffi" }
```

Import the prelude:

```rust
use raml_ffi::prelude::*;
```

## Implementation Plan

1. **Phase 1** (Current): Re-export raml-core ✅
2. **Phase 2**: Create raml-derive proc macro crate
3. **Phase 3**: Implement `#[derive(Value)]` for structs
4. **Phase 4**: Implement `#[derive(Value)]` for enums
5. **Phase 5**: Add error handling and validation

## See Also

- `raml-core`: Core value representation (implementation details)
- `raml-rt`: OCaml bytecode runtime
- `raml-kernel`: Executable runner
