# raml-derive Implementation

Implementation notes for the `#[derive(Value)]` proc macro.

## Overview

Generates `Into<Value>` and `TryFrom<Value>` implementations for:
- ✅ Structs (records)
- ✅ Enums (variants with/without data)

## Code Generation

### Structs

**Input:**
```rust
#[derive(Value)]
struct Point { x: i32, y: i32 }
```

**Generated:**
```rust
impl Into<raml_core::Value> for Point {
    fn into(self) -> raml_core::Value {
        unsafe {
            // Allocate block: tag=0 (record), size=2
            let ptr = alloc_block(0, 2);
            let block = &mut *ptr;
            
            // Set fields
            block.set_field(0, self.x.into());
            block.set_field(1, self.y.into());
            
            Value::from_block_ptr(ptr)
        }
    }
}

impl TryFrom<raml_core::Value> for Point {
    type Error = &'static str;
    
    fn try_from(value: Value) -> Result<Self, Self::Error> {
        // Validate: is_block, tag=0, size=2
        // Extract fields and convert
    }
}
```

### Enums (Unit Variants)

**Input:**
```rust
#[derive(Value)]
enum Color {
    Red,      // 0
    Green,    // 1
    Blue,     // 2
}
```

**Representation:**
- `Red` → `Value::int(0)`
- `Green` → `Value::int(1)`
- `Blue` → `Value::int(2)`

### Enums (Data Variants)

**Input:**
```rust
#[derive(Value)]
enum Shape {
    Circle(f64),              // Block(tag=0, [radius])
    Rectangle(f64, f64),      // Block(tag=1, [width, height])
}
```

**Generated:**
```rust
impl Into<raml_core::Value> for Shape {
    fn into(self) -> raml_core::Value {
        match self {
            Shape::Circle(f0) => {
                // Allocate block: tag=0, size=1
                // Set field 0 = radius
            },
            Shape::Rectangle(f0, f1) => {
                // Allocate block: tag=1, size=2
                // Set fields 0,1 = width, height
            },
        }
    }
}
```

### Mixed Enums

**Input:**
```rust
#[derive(Value)]
enum Color {
    Red,                      // int(0)
    Green,                    // int(1)
    Blue,                     // int(2)
    RGB(u8, u8, u8),         // Block(tag=0, [r,g,b])
}
```

**Strategy:**
1. Count unit variants: N = 3
2. First N variants → integers 0..(N-1)
3. Remaining variants → blocks with tags starting at 0

## OCaml Compatibility

### Records
```rust
struct Point { x: i32, y: i32 }
```
↔
```ocaml
type point = { x : int; y : int }
```

### Simple Variants
```rust
enum Color { Red, Green, Blue }
```
↔
```ocaml
type color = Red | Green | Blue
```

### Variants with Data
```rust
enum Shape {
    Circle(f64),
    Rectangle(f64, f64),
}
```
↔
```ocaml
type shape =
  | Circle of float
  | Rectangle of float * float
```

## Technical Details

### Block Allocation

```rust
unsafe {
    let layout = Layout::from_size_align_unchecked(
        size_of::<BlockHeader>() + field_count * size_of::<Value>(),
        align_of::<BlockHeader>()
    );
    
    let ptr = alloc(layout) as *mut Block;
    
    let header = BlockHeader::new(field_count, tag, GcColor::White);
    ptr::write(ptr as *mut BlockHeader, header);
    
    // ptr now points to valid Block
}
```

### Field Conversion

All fields must implement:
- `Into<raml_core::Value>` (for serialization)
- `TryFrom<raml_core::Value>` (for deserialization)

### Error Handling

`TryFrom` returns `Result<Self, &'static str>` with errors:
- `"Expected a block"` - value is not a block
- `"Expected record tag (0)"` - wrong tag for struct
- `"Wrong number of fields"` - size mismatch
- `"Field conversion failed"` - field TryFrom failed
- `"Invalid constant constructor tag"` - int out of range
- `"Unknown variant"` - unrecognized tag/size combo

## Limitations

Current:
- ❌ Tuple structs (`struct Point(i32, i32)`)
- ❌ Named enum variant fields (`Foo { x: i32 }`)
- ❌ Generic types (`struct Foo<T> { ... }`)
- ❌ Lifetimes and references
- ❌ Unions

Future:
- Better error messages (full error type)
- Attribute customization (`#[ocaml(tag = 5)]`)
- Generic support with trait bounds
- Custom conversion hooks

## Testing

Run examples:
```bash
cargo run -p raml-ffi --example derive_demo
cargo run -p raml-ffi --example simple_struct
cargo run -p raml-ffi --example simple_enum
cargo run -p raml-ffi --example mixed_enum
```

## Performance

- **Zero runtime overhead**: All conversions inline
- **Compile-time code generation**: No reflection
- **Unsafe but sound**: Proper allocation and initialization
- **GC-friendly**: Correct headers for OCaml GC

## See Also

- `raml-core`: Value representation details
- `raml-ffi`: High-level usage examples
- OCaml manual: Memory representation
