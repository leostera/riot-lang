# RAML Native Runtime - C API Compatibility

## Overview

The RAML native runtime (`src/native/`) provides a **C-compatible API** that allows RAML to serve as a drop-in replacement for OCaml's standard runtime library (`libcamlrun.a`).

This enables OCaml programs compiled with `ocamlopt` to run on RAML without modification.

## Architecture

```text
┌─────────────────────────────────────────────────┐
│         OCaml Native Code (.o files)            │
│                                                 │
│  - Compiled with ocamlopt                       │
│  - Calls runtime functions via C ABI            │
│  - Accesses global variables (young_ptr, etc.)  │
└──────────────┬──────────────────────────────────┘
               │ C calling convention
               │
┌──────────────▼──────────────────────────────────┐
│      RAML Native Runtime (this module)          │
│                                                 │
│  Exports:                                       │
│  - caml_alloc_small()                           │
│  - caml_apply()                                 │
│  - caml_raise_exception()                       │
│  - caml_young_ptr (global variable)             │
│  - ... (28 essential functions)                 │
└──────────────┬──────────────────────────────────┘
               │ Rust internal API
               │
┌──────────────▼──────────────────────────────────┐
│         RAML Core Runtime                       │
│                                                 │
│  - Heap (src/runtime/memory.rs)                 │
│  - GC (src/runtime/gc.rs)                       │
│  - Interpreter (src/runtime/interpreter.rs)     │
│  - Effect Handlers (src/runtime/fiber.rs)       │
└─────────────────────────────────────────────────┘
```

## C API Surface

### Memory Management (11 functions)

| Function | Status | Description |
|----------|--------|-------------|
| `caml_alloc_small` | ❌ TODO | Fast allocation from minor heap |
| `caml_alloc_shr` | ❌ TODO | Allocate in major heap |
| `caml_alloc_string` | ❌ TODO | Allocate string |
| `caml_modify` | ⚠️ Partial | Update field with write barrier |
| `caml_initialize` | ✅ Done | Initialize field (no barrier) |
| `caml_young_ptr` | ✅ Done | Global: next free space |
| `caml_young_limit` | ✅ Done | Global: heap limit |
| `caml_call_gc` | ❌ TODO | Explicit GC trigger |
| `caml_gc_message` | ❌ TODO | GC logging |
| `caml_allocation_point` | ❌ TODO | Allocation hook |
| `caml_alloc_dummy` | ❌ TODO | Placeholder allocation |

### Function Application (4 functions)

| Function | Status | Description |
|----------|--------|-------------|
| `caml_apply` | ❌ TODO | Apply function to 1 arg |
| `caml_apply2` | ❌ TODO | Apply function to 2 args |
| `caml_apply3` | ❌ TODO | Apply function to 3 args |
| `caml_applyvN` | ❌ TODO | Apply function to N args |

### Arrays (3 functions)

| Function | Status | Description |
|----------|--------|-------------|
| `caml_make_vect` | ❌ TODO | Create array |
| `caml_array_get_addr` | ❌ TODO | Get element address |
| `caml_array_get_float` | ❌ TODO | Get float element |

### Exceptions (3 functions)

| Function | Status | Description |
|----------|--------|-------------|
| `caml_raise_exception` | ⚠️ Panic | Raise exception (panics for now) |
| `caml_raise_constant` | ⚠️ Panic | Raise constant exception |
| `caml_raise_with_arg` | ❌ TODO | Raise with argument |

### Strings (2 functions)

| Function | Status | Description |
|----------|--------|-------------|
| `caml_string_get` | ❌ TODO | Get character |
| `caml_string_set` | ❌ TODO | Set character |

### Comparison (3 functions)

| Function | Status | Description |
|----------|--------|-------------|
| `caml_equal` | ⚠️ Shallow | Structural equality (shallow for now) |
| `caml_compare` | ⚠️ Shallow | Three-way comparison |
| `caml_hash` | ⚠️ Shallow | Hash function |

### C Interface (2 functions)

| Function | Status | Description |
|----------|--------|-------------|
| `caml_c_call` | ❌ TODO | Call C function from OCaml |
| `caml_callback` | ❌ TODO | Call OCaml from C |

**Total: 2/28 functions implemented** (7%)

## Implementation Priority

### Phase 1: Basic Functionality (Milestone: Hello World)
**Goal:** Run a simple OCaml program that prints output.

1. ✅ `caml_init_runtime` - Initialize runtime
2. ❌ `caml_alloc_small` - Allocate blocks
3. ❌ `caml_alloc_string` - Allocate strings
4. ❌ `caml_apply` - Function calls
5. ❌ `caml_young_ptr` / `caml_young_limit` - Fast allocation path

**Test:** Compile and run:
```ocaml
let () = print_endline "Hello from RAML!"
```

### Phase 2: Core Operations (Milestone: Basic Algorithms)
**Goal:** Support data structures and computation.

6. ❌ `caml_make_vect` - Arrays
7. ❌ `caml_modify` - Mutation with GC safety
8. ❌ `caml_equal` / `caml_compare` - Deep comparison
9. ❌ `caml_raise_exception` - Exception handling
10. ❌ `caml_call_gc` - Manual GC trigger

**Test:** Compile and run list/array operations, exceptions.

### Phase 3: Advanced Features (Milestone: Real Programs)
**Goal:** Run complex programs with full runtime support.

11. ❌ `caml_c_call` - FFI to C
12. ❌ `caml_callback` - C to OCaml calls
13. ❌ Effect handlers integration
14. ❌ Performance optimization (inline allocations)

**Test:** Run real-world programs (compiler, web server, etc.)

## Usage

### Building as a Library

To build RAML as a C-compatible library:

```bash
cd raml
cargo build --release --lib

# The .so/.dylib/.dll can be used as a drop-in replacement for libcamlrun
# Location: target/release/libraml_rt.so (Linux)
#           target/release/libraml_rt.dylib (macOS)
#           target/release/raml_rt.dll (Windows)
```

### Linking with OCaml Code

```bash
# Compile OCaml to native code
ocamlopt -c myprogram.ml -o myprogram.o

# Link with RAML runtime instead of libcamlrun
cc myprogram.o -L target/release -lraml_rt -o myprogram

# Run
./myprogram
```

### Testing with Simple Program

Create `test.ml`:
```ocaml
let () = print_endline "Hello from RAML!"
```

Compile and test:
```bash
# Compile to native
ocamlopt -c test.ml

# Link with RAML (when implemented)
cc test.o -L raml/target/release -lraml_rt -o test_raml

# Run
./test_raml
```

## Debugging with LLDB

RAML includes LLDB integration for debugging OCaml values. See [../.lldbinit](../.lldbinit).

### LLDB Commands

```bash
# Load RAML extensions
lldb ./myprogram
(lldb) command source raml/.lldbinit

# Print OCaml value in human-readable format
(lldb) pval my_variable
# Output: Int(42) or Block(addr=0x..., tag=0, size=3)

# Print interpreter stack
(lldb) pstack
# Output: Stack (depth=10):
#   [0] Int(1)
#   [1] Block(0x...)
#   ...

# Print heap statistics
(lldb) pheap

# Print block contents
(lldb) pblock 0x7fff12345678
```

### Custom Type Formatters

LLDB automatically formats OCaml values:

```
(lldb) print accu
(Value) $0 = Int(42)

(lldb) print block
(Block) $1 = Block(tag=0, size=3)
```

## Implementation Notes

### Memory Safety

All C FFI functions are marked `unsafe(no_mangle)` and must be carefully reviewed for:
- Null pointer dereferences
- Buffer overflows
- Race conditions (if adding threading support)
- GC safety (roots must be tracked)

### GC Integration

C API functions that allocate must:
1. Collect GC roots before allocation
2. Call heap allocation with roots
3. Handle out-of-memory errors

Example:
```rust
pub extern "C" fn caml_alloc_small(size: usize, tag: u8) -> usize {
    // 1. Collect roots
    let mut roots = collect_all_roots();
    
    // 2. Allocate (may trigger GC)
    let block = heap.alloc_block(size, tag, &mut roots)?;
    
    // 3. Return raw pointer
    block as usize
}
```

### Performance Considerations

The **hot path** for allocation is:
1. Native code checks `caml_young_ptr - size >= caml_young_limit`
2. If true, inline allocate (no function call!)
3. If false, call `caml_alloc_small` to trigger minor GC

This means we must:
- Keep `caml_young_ptr` and `caml_young_limit` up-to-date
- Make minor GC fast (~1ms typical)
- Consider per-thread heaps for parallelism

## Testing Strategy

### Unit Tests
Test individual C functions in isolation:
```rust
#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_caml_alloc_small() {
        caml_init_runtime();
        let ptr = caml_alloc_small(3, 0);
        assert!(ptr != 0);
        assert_eq!(ptr & 1, 0); // Block pointer
    }
}
```

### Integration Tests
Test with real OCaml bytecode:
```bash
# Compile test case
ocamlopt -c tests/simple.ml

# Link with RAML
cc tests/simple.o -L target/release -lraml_rt -o tests/simple_raml

# Run and check output
./tests/simple_raml
```

### Compatibility Tests
Run OCaml test suite against RAML runtime.

## Next Steps

1. **Implement Phase 1 functions** (Hello World milestone)
2. **Add comprehensive logging** for debugging
3. **Write unit tests** for each function
4. **Test with simple programs** (print, arithmetic)
5. **Profile and optimize** hot paths
6. **Add CI/CD** to test against OCaml programs

## References

- [OCaml Runtime System](https://v2.ocaml.org/manual/runtime.html)
- [OCaml Internals](https://dev.realworldocaml.org/runtime-memory-layout.html)
- [RAML Project README](../README.md)
- [Effect Handlers Implementation](../src/runtime/fiber.rs)
