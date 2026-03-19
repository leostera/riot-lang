# riot-ffi

High-level FFI interface for Rust⟷OCaml interop.

## Overview

This crate provides a convenient facade for working with OCaml values from Rust. It re-exports all types from `riot-core` and the `riot-derive` `Value` macro.

## Quick Start

```rust
use riot_ffi::prelude::*;

let v = Value::int(42);
assert_eq!(v.as_int(), 42);
```

## Architecture

```
riot-ffi (this crate)
  ├── Re-exports riot-core
  └── Re-exports riot-derive::Value

riot-core
  └── Core value representation
```

## Derive Macro

This crate already exposes `#[derive(Value)]` for automatic conversion:

```rust
use riot_ffi::prelude::*;

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
riot-ffi = { path = "../riot-ffi" }
```

Import the prelude:

```rust
use riot_ffi::prelude::*;
```

## What It Re-exports

- Everything from `riot-core`
- `riot_derive::Value`
- A `prelude` module containing the common imports for FFI crates

## See Also

- `riot-core`: Core value representation
- `riot-derive`: Proc-macro implementation details
- `hello-rust`: End-to-end smoke-test crate
