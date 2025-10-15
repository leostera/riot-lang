# Marshal Format Fix - What's Needed for .cmo Support

## The Problem

When you run `ocamlc -c program.ml`, it creates `program.cmo` - a relocatable bytecode object file.

The bytecode location is **hidden** inside a marshaled OCaml record at the end of the file:

```ocaml
type compilation_unit = {
  cu_name: string;           (* Field 0: Module name *)
  cu_pos: int;               (* Field 1: WHERE IS THE BYTECODE? *)
  cu_codesize: int;          (* Field 2: HOW LONG IS IT? *)
  cu_reloc: reloc_info list; (* Field 3: Relocation info *)
  cu_imports: crcs list;     (* Field 4: Module dependencies *)
  cu_primitives: string list;(* Field 5: What C functions needed? *)
  cu_force_link: bool;       (* Field 6 *)
  cu_debug: int;             (* Field 7: Debug info offset *)
  cu_debugsize: int;         (* Field 8: Debug info size *)
  cu_curry_fun: int list;    (* Field 9 *)
}
```

**We need to extract fields 1, 2, and 5!**

## What Works NOW

The marshal parser can read **simple values**:

```rust
// ✅ WORKS
reader.read_value()  // Returns a Value

// Value types that work:
- Integers (0x00-0x7F, 0xFC, 0xFD)
- Strings (0x90)
- Floats (0x91)
- Blocks (0x80-0x8F) - BUT...
```

## What DOESN'T Work

**Block field extraction!**

When we read a block (record/tuple), we parse all the fields but then **throw them away**:

```rust
// Current code in marshal.rs:189
fn read_block(&mut self, tag: u8, size: usize) -> Result<Value> {
    let mut fields = Vec::with_capacity(size);
    for _ in 0..size {
        fields.push(self.read_object()?);  // ✅ Reads each field
    }
    
    // ❌ BUG: We just return the first field!
    if size == 0 {
        Ok(VAL_UNIT)
    } else {
        Ok(fields[0])  // ❌ Loses all other fields!
    }
}
```

**This is the bug!** We read all 10 fields of `compilation_unit`, but only return `fields[0]` (the module name string).

We **lose** the crucial data:
- `fields[1]` = cu_pos (bytecode location)
- `fields[2]` = cu_codesize (bytecode size)
- `fields[5]` = cu_primitives (list of primitive names)

## The Fix Required

### Option 1: Return Blocks as Values

Make `Value` able to represent blocks with fields:

```rust
// In value.rs
pub enum Value {
    Int(isize),
    Block {
        tag: u8,
        fields: Vec<Value>,
    },
    // ... other types
}

// In marshal.rs
fn read_block(&mut self, tag: u8, size: usize) -> Result<Value> {
    let mut fields = Vec::with_capacity(size);
    for _ in 0..size {
        fields.push(self.read_object()?);
    }
    
    Ok(Value::Block { tag, fields })  // ✅ Keep all fields!
}
```

Then in `read_compilation_unit()`:

```rust
match reader.read_value()? {
    Value::Block { tag: 0, fields } => {
        // Extract the fields we need
        let cu_pos = extract_int(&fields[1])?;       // Field 1
        let cu_codesize = extract_int(&fields[2])?;  // Field 2
        let cu_primitives = extract_string_list(&fields[5])?; // Field 5
        
        Ok(CompilationUnit {
            code_offset: cu_pos as usize,
            code_size: cu_codesize as usize,
            primitives: cu_primitives,
            debug_offset: 0,
            debug_size: 0,
        })
    }
    _ => Err(Error::InvalidBytecode("Expected block".to_string()))
}
```

### Option 2: Extract Fields During Unmarshaling

Add a method to extract specific fields:

```rust
impl MarshalReader {
    /// Read a record and extract specific fields
    pub fn read_record_fields(&mut self, field_indices: &[usize]) 
        -> Result<Vec<Value>> 
    {
        let tag = self.data[self.pos];
        self.pos += 1;
        
        if tag < 0x80 || tag > 0x8F {
            return Err(Error::InvalidBytecode("Not a block".to_string()));
        }
        
        let size = self.read_size()?;
        let mut fields = Vec::with_capacity(size);
        
        for i in 0..size {
            let field_value = self.read_object()?;
            if field_indices.contains(&i) {
                fields.push(field_value);
            }
        }
        
        Ok(fields)
    }
}

// Usage:
let fields = reader.read_record_fields(&[1, 2, 5])?;
let cu_pos = extract_int(&fields[0])?;
let cu_codesize = extract_int(&fields[1])?;
let cu_primitives = extract_string_list(&fields[2])?;
```

## Helper Functions Needed

### Extract Integer from Value
```rust
fn extract_int(value: &Value) -> Result<isize> {
    match value {
        Value::Int(n) => Ok(*n),
        _ => Err(Error::InvalidBytecode("Expected int".to_string()))
    }
}
```

### Extract String List from Value
```rust
fn extract_string_list(value: &Value) -> Result<Vec<String>> {
    // OCaml lists are encoded as:
    // - [] = Block(tag=0, size=0)  // Empty list
    // - x::xs = Block(tag=0, size=2, fields=[x, xs])  // Cons cell
    
    match value {
        Value::Block { tag: 0, fields } if fields.is_empty() => {
            Ok(Vec::new())  // Empty list
        }
        Value::Block { tag: 0, fields } if fields.len() == 2 => {
            // Cons cell: [head, tail]
            let head = extract_string(&fields[0])?;
            let tail = extract_string_list(&fields[1])?;
            let mut result = vec![head];
            result.extend(tail);
            Ok(result)
        }
        _ => Err(Error::InvalidBytecode("Expected list".to_string()))
    }
}
```

### Extract String from Value
```rust
fn extract_string(value: &Value) -> Result<String> {
    match value {
        Value::String(s) => Ok(s.clone()),
        _ => Err(Error::InvalidBytecode("Expected string".to_string()))
    }
}
```

## Concrete Example

Let's say `test_simple.cmo` contains this marshaled data:

```
[Marshal header: 20 bytes]
[Block tag=0, size=10]  ← compilation_unit record
  [Field 0: String "Test_simple"]      ← cu_name
  [Field 1: Int 16]                    ← cu_pos (bytecode at offset 16)
  [Field 2: Int 224]                   ← cu_codesize (224 bytes)
  [Field 3: Block (relocation data)]   ← cu_reloc
  [Field 4: Block (imports)]           ← cu_imports
  [Field 5: List ["caml_print_int"]]   ← cu_primitives
  [Field 6: Int 0]                     ← cu_force_link
  [Field 7: Int 0]                     ← cu_debug
  [Field 8: Int 0]                     ← cu_debugsize
  [Field 9: Block (curry info)]        ← cu_curry_fun
```

**Current behavior:**
```rust
reader.read_value() // Returns fields[0] = "Test_simple"
// ❌ Lost fields 1-9!
```

**After fix:**
```rust
reader.read_value() // Returns Block { tag: 0, fields: [...all 10 fields...] }

// Extract what we need:
cu_pos = fields[1] = 16
cu_codesize = fields[2] = 224
cu_primitives = fields[5] = ["caml_print_int"]

// Seek to offset 16, read 224 bytes → BYTECODE! ✅
```

## Implementation Plan

### Step 1: Update Value Type (30 minutes)
```rust
// src/value.rs
pub enum Value {
    Int(isize),
    String(String),       // Add this
    Block {               // Add this
        tag: u8,
        fields: Vec<Value>,
    },
}
```

### Step 2: Fix read_block() (15 minutes)
```rust
// src/runtime/marshal.rs
fn read_block(&mut self, tag: u8, size: usize) -> Result<Value> {
    let mut fields = Vec::with_capacity(size);
    for _ in 0..size {
        fields.push(self.read_object()?);
    }
    Ok(Value::Block { tag, fields })  // ✅ Return all fields
}
```

### Step 3: Fix read_string() (15 minutes)
```rust
fn read_string(&mut self, len: usize) -> Result<Value> {
    let bytes = &self.data[self.pos..self.pos + len];
    self.pos += len;
    
    let padding = (4 - (len % 4)) % 4;
    self.pos += padding;
    
    let s = String::from_utf8_lossy(bytes).to_string();
    Ok(Value::String(s))  // ✅ Return actual string
}
```

### Step 4: Add Helper Functions (30 minutes)
```rust
// In bytecode.rs
fn extract_int(value: &Value) -> Result<isize> { ... }
fn extract_string(value: &Value) -> Result<String> { ... }
fn extract_string_list(value: &Value) -> Result<Vec<String>> { ... }
```

### Step 5: Update read_compilation_unit() (30 minutes)
```rust
fn read_compilation_unit<R: Read + Seek>(file: &mut R) -> Result<CompilationUnit> {
    // ... read marshal data ...
    
    match reader.read_value()? {
        Value::Block { tag: 0, fields } if fields.len() >= 6 => {
            let cu_pos = extract_int(&fields[1])?;
            let cu_codesize = extract_int(&fields[2])?;
            let cu_primitives = extract_string_list(&fields[5])?;
            
            Ok(CompilationUnit {
                code_offset: cu_pos as usize,
                code_size: cu_codesize as usize,
                primitives: cu_primitives,
                debug_offset: 0,
                debug_size: 0,
            })
        }
        _ => Err(Error::InvalidBytecode(
            "Invalid compilation_unit structure".to_string()
        ))
    }
}
```

### Step 6: Test! (30 minutes)
```bash
echo 'let () = print_int 42' > test.ml
ocamlc -c test.ml  # Creates test.cmo

./target/release/raml-rt info test.cmo
# Should output:
# ✓ Loaded successfully!
#   Code instructions: 56
#   Primitives: 1
#   Primitives used:
#     [0] caml_print_int
```

## Total Time Estimate

- **2-3 hours** for a complete implementation
- **4-5 hours** if you hit edge cases (nested lists, etc.)

## Files to Modify

1. `src/value.rs` - Add Block and String variants
2. `src/runtime/marshal.rs` - Fix read_block() and read_string()
3. `src/runtime/bytecode.rs` - Add helper functions, update read_compilation_unit()
4. Test with real .cmo files

## Why This Matters

With this fix:
- ✅ Load `.cmo` files (224 bytes instead of 23KB)
- ✅ Modular loading (only what you need)
- ✅ 10x smaller bundles
- ✅ Browser performance improvement

**This is the ONLY thing blocking .cmo support!**

The bytecode loader is perfect. The marshal parser reads data correctly. We just need to **not throw away the fields we already read**.
