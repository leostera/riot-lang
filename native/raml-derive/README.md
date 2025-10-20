# raml-derive

Procedural macros for automatic Rust⟷OCaml value conversions.

## Overview

This crate provides `#[derive(Value)]` to automatically generate `Into<Value>` and `TryFrom<Value>` implementations for Rust types.

## Features

✅ **Structs** → OCaml records  
✅ **Enums (unit variants)** → OCaml constant constructors  
✅ **Enums (data variants)** → OCaml blocks with tags  
✅ **Round-trip conversions** (Rust → OCaml → Rust)

## Usage

Add to your `Cargo.toml`:

```toml
[dependencies]
raml-core = { path = "../raml-core" }
raml-derive = { path = "../raml-derive" }
```

Or use the high-level `raml-ffi` facade:

```toml
[dependencies]
raml-ffi = { path = "../raml-ffi" }
```

## Examples

### Structs

```rust
use raml_ffi::prelude::*;

#[derive(Value)]
struct Point {
    x: i32,
    y: i32,
}

let point = Point { x: 10, y: 20 };
let val: Value = point.into();           // → Block(tag=0, [10, 20])
let point2: Point = val.try_into()?;      // ← Reconstructed
```

Generated OCaml equivalent:
```ocaml
type point = { x : int; y : int }
```

### Enums (Constant Constructors)

```rust
#[derive(Value)]
enum Color {
    Red,      // → Value::int(0)
    Green,    // → Value::int(1)
    Blue,     // → Value::int(2)
}
```

Generated OCaml equivalent:
```ocaml
type color = Red | Green | Blue
```

### Enums (Data Constructors)

```rust
#[derive(Value)]
enum Shape {
    Circle(f64),                // → Block(tag=0, [radius])
    Rectangle(f64, f64),        // → Block(tag=1, [width, height])
}
```

Generated OCaml equivalent:
```ocaml
type shape =
  | Circle of float
  | Rectangle of float * float
```

### Mixed Enums

```rust
#[derive(Value)]
enum Color {
    Red,                        // → Value::int(0)
    Green,                      // → Value::int(1)
    Blue,                       // → Value::int(2)
    RGB(u8, u8, u8),           // → Block(tag=0, [r, g, b])
}
```

Generated OCaml equivalent:
```ocaml
type color =
  | Red
  | Green
  | Blue
  | RGB of int * int * int
```

## How It Works

### Structs

- Converted to OCaml blocks with tag = 0 (record tag)
- Fields mapped in definition order
- Each field must implement `Into<Value>` and `TryFrom<Value>`

### Enums

**Constant constructors** (no fields):
- First N variants without fields become integers 0..(N-1)
- Represented as `Value::int(n)`

**Data constructors** (with fields):
- Remaining variants become blocks with sequential tags starting at 0
- Tag identifies the variant
- Fields stored as block fields

## Requirements

All field types must implement:
- `Into<raml_core::Value>`
- `TryFrom<raml_core::Value>`

## Limitations

- ❌ Tuple structs not yet supported
- ❌ Named fields in enum variants not yet supported
- ❌ Generic types not yet supported
- ❌ Unions not supported

## Implementation Details

The macro generates:

```rust
impl Into<raml_core::Value> for YourType {
    fn into(self) -> raml_core::Value {
        // Allocate block, set fields
    }
}

impl TryFrom<raml_core::Value> for YourType {
    type Error = &'static str;
    
    fn try_from(value: raml_core::Value) -> Result<Self, Self::Error> {
        // Validate tag/size, extract fields
    }
}
```

## See Also

- `raml-core`: Core value representation
- `raml-ffi`: High-level FFI facade (re-exports this crate)
- Examples: `derive_demo.rs`, `simple_struct.rs`, `simple_enum.rs`
