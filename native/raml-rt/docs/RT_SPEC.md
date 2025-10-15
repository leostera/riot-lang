# RAML Runtime Specification
## Rust Abstract Machine Layer - Technical Specification

**Version:** 0.1  
**Date:** 2025-01-08  
**Based on:** OCaml 5.3.0 Runtime (~38K LOC C)

---

## Table of Contents

1. [Overview & Goals](#1-overview--goals)
2. [Value Representation](#2-value-representation)
3. [Memory Layout](#3-memory-layout)
4. [Garbage Collection](#4-garbage-collection)
5. [Bytecode Format](#5-bytecode-format)
6. [Instruction Set](#6-instruction-set)
7. [Calling Convention](#7-calling-convention)
8. [Concurrency Model](#8-concurrency-model)
9. [Effect Handlers](#9-effect-handlers)
10. [Foreign Function Interface](#10-foreign-function-interface)
11. [Implementation Roadmap](#11-implementation-roadmap)

---

## 1. Overview & Goals

### 1.1 What is RAML?

RAML is a **modern reimplementation** of the OCaml bytecode runtime in Rust. It aims to:

- ✅ **Execute OCaml bytecode** - Full compatibility with .cmo/.cma files
- ✅ **Match semantics exactly** - Same value repr, GC behavior, concurrency
- 🚀 **Improve performance** - Rust safety + potential JIT compilation
- 🔧 **Enable embedding** - Use OCaml code from Rust applications
- 📦 **Simplify deployment** - Single binary, no C runtime dependency

### 1.2 Design Principles

1. **Correctness First**: Match OCaml behavior exactly before optimizing
2. **Safe Rust**: Leverage Rust's type system, no `unsafe` where avoidable
3. **Incremental Development**: Build in layers (values → GC → interp → multicore)
4. **Measurable Progress**: Each phase produces working bytecode executor

### 1.3 Non-Goals (For V1)

- ❌ Native code execution (use OCaml's optimizing compiler)
- ❌ Source-level compatibility (only bytecode)
- ❌ C API compatibility (new Rust-friendly API)

---

## 2. Value Representation

### 2.1 Tagged Pointer Scheme

OCaml uses 1-bit tagging to distinguish integers from heap pointers:

```
┌─────────────────────────────────────────────────────┐
│                    64-bit Value                      │
├─────────────────────────────────────────────────────┤
│                                                       │
│  Integer:  [───────── 63 bits ────────]│1│          │
│                 (signed integer)                      │
│                                                       │
│  Pointer:  [───────── 63 bits ────────]│0│          │
│              (address must be aligned)                │
└─────────────────────────────────────────────────────┘

LSB = 1 → Integer (immediate value)
LSB = 0 → Pointer (to heap block)
```

**Rust Type:**

```rust
#[repr(transparent)]
#[derive(Copy, Clone, PartialEq, Eq)]
pub struct Value(usize);

impl Value {
    /// Tag an integer for OCaml representation
    #[inline(always)]
    pub fn int(n: isize) -> Self {
        Value(((n as usize) << 1) | 1)
    }
    
    /// Extract integer from tagged value
    #[inline(always)]
    pub fn as_int(self) -> isize {
        (self.0 as isize) >> 1
    }
    
    /// Check if value is an integer
    #[inline(always)]
    pub fn is_int(self) -> bool {
        (self.0 & 1) != 0
    }
    
    /// Check if value is a block pointer
    #[inline(always)]
    pub fn is_block(self) -> bool {
        (self.0 & 1) == 0
    }
    
    /// Unsafe: reinterpret as block pointer
    #[inline(always)]
    pub unsafe fn as_block_ptr(self) -> *const BlockHeader {
        self.0 as *const BlockHeader
    }
}
```

### 2.2 Immediate Values

Some values don't need heap allocation:

| OCaml Type | Representation | Example |
|------------|---------------|---------|
| `int` | `(n << 1) \| 1` | `42` → `0x55` |
| `bool` | `Val_int(0/1)` | `true` → `0x3`, `false` → `0x1` |
| `unit` | `Val_int(0)` | `()` → `0x1` |
| `[]` (empty list) | `Val_int(0)` | `[]` → `0x1` |
| `None` | `Val_int(0)` | `None` → `0x1` |
| `char` | `Val_int(c)` | `'A'` → `0x83` |

**Integer Range:**
- 32-bit: -2³⁰ to 2³⁰-1 (30-bit signed)
- 64-bit: -2⁶² to 2⁶²-1 (62-bit signed)

### 2.3 Heap Blocks

All non-immediate values are **blocks** (heap-allocated):

```
┌──────────────────────────────────────────┐
│           Block in Memory                 │
├──────────────────────────────────────────┤
│                                           │
│  Header:    [reserved][size][color][tag] │  ← 1 word
│             ─────────────────────────────  │
│  Field 0:   value                         │  ← N words
│  Field 1:   value                         │
│  ...                                      │
│  Field N-1: value                         │
│                                           │
└──────────────────────────────────────────┘
       ▲
       │
    Value points here (to first field, NOT header)
```

**Header Layout (64-bit):**

```
┌────────┬────────────┬─────────┬──────────┐
│Reserved│   Size     │ Color   │   Tag    │
│ 32 bits│  22 bits   │ 2 bits  │  8 bits  │
└────────┴────────────┴─────────┴──────────┘
```

- **Size (wosize)**: Number of fields (words), 0 to 2²²-1
- **Tag**: Block type (0-255), see [Section 2.4](#24-tag-values)
- **Color**: GC color (00=white, 01=gray, 10=black, 11=immobile)
- **Reserved**: Configurable bits for custom use (default 0)

**Rust Representation:**

```rust
#[repr(C)]
pub struct BlockHeader {
    header: AtomicUsize,
}

impl BlockHeader {
    const TAG_BITS: u8 = 8;
    const TAG_MASK: usize = (1 << Self::TAG_BITS) - 1;
    
    const COLOR_BITS: u8 = 2;
    const COLOR_SHIFT: u8 = Self::TAG_BITS;
    const COLOR_MASK: usize = ((1 << Self::COLOR_BITS) - 1) << Self::COLOR_SHIFT;
    
    const SIZE_BITS: u8 = 22;
    const SIZE_SHIFT: u8 = Self::COLOR_SHIFT + Self::COLOR_BITS;
    const SIZE_MASK: usize = ((1 << Self::SIZE_BITS) - 1) << Self::SIZE_SHIFT;
    
    pub fn new(size: usize, tag: u8, color: u8) -> usize {
        ((size << Self::SIZE_SHIFT) & Self::SIZE_MASK)
            | ((color as usize) << Self::COLOR_SHIFT)
            | (tag as usize)
    }
    
    pub fn tag(&self) -> u8 {
        (self.header.load(Ordering::Relaxed) & Self::TAG_MASK) as u8
    }
    
    pub fn size(&self) -> usize {
        (self.header.load(Ordering::Relaxed) & Self::SIZE_MASK) >> Self::SIZE_SHIFT
    }
    
    pub fn color(&self) -> u8 {
        ((self.header.load(Ordering::Relaxed) & Self::COLOR_MASK) >> Self::COLOR_SHIFT) as u8
    }
}

/// A heap-allocated block with fields
#[repr(C)]
pub struct Block {
    header: BlockHeader,
    fields: [Value],  // Dynamically sized
}
```

### 2.4 Tag Values

Tags determine block semantics:

| Tag Range | Meaning | Fields Contain |
|-----------|---------|----------------|
| 0-245 | Structured blocks | Traced by GC (pointers/ints) |
| 246 | `Lazy_tag` | Lazy value |
| 247 | `Closure_tag` | Function closure |
| 248 | `Object_tag` | Object instance |
| 249 | `Infix_tag` | Infix header (inside closure) |
| 250 | `Forward_tag` | Forwarding pointer (GC) |
| 251 | `Abstract_tag` | Opaque C data (not traced) |
| 252 | `String_tag` | String (bytes) |
| 253 | `Double_tag` | Float (8 bytes) |
| 254 | `Double_array_tag` | Float array |
| 255 | `Custom_tag` | Custom C type |

**Special Tags:**

- **Tag < 251 (`No_scan_tag`)**: Fields are scanned by GC
- **Tag ≥ 251**: Fields NOT scanned (raw bytes)
- **Tag 0**: Tuples, `Some`, list cons `(::)`
- **Tag 247**: Closures (first field is code pointer)

### 2.5 Special Values

```rust
pub const VAL_UNIT: Value = Value(1);        // ()
pub const VAL_FALSE: Value = Value(1);       // false
pub const VAL_TRUE: Value = Value(3);        // true
pub const VAL_EMPTY_LIST: Value = Value(1);  // []
pub const VAL_NONE: Value = Value(1);        // None

pub const TAG_CONS: u8 = 0;     // (::)
pub const TAG_SOME: u8 = 0;     // Some
```

---

## 3. Memory Layout

### 3.1 Heap Organization

RAML uses a **generational** garbage collector with two generations:

```
┌─────────────────────────────────────────────┐
│              Domain 0 (OS Thread)            │
├─────────────────────────────────────────────┤
│                                              │
│  ┌────────────────────────────────────────┐ │
│  │   Minor Heap (Young Generation)        │ │
│  │   - Bump allocator                     │ │
│  │   - 256KB-16MB per domain              │ │
│  │   - Fast allocation (no locks)         │ │
│  │   - Stop-and-copy GC                   │ │
│  └────────────────────────────────────────┘ │
│                     ▲                        │
│                     │ Promotes survivors     │
│                     ▼                        │
│  ┌────────────────────────────────────────┐ │
│  │   Major Heap (Old Generation)  SHARED  │ │
│  │   - Page-based allocator               │ │
│  │   - Concurrent mark-sweep              │ │
│  │   - Compaction (rare)                  │ │
│  └────────────────────────────────────────┘ │
│                                              │
│  Stack: [─────────────] (growable)          │
│                                              │
└─────────────────────────────────────────────┘
```

### 3.2 Minor Heap (Per-Domain)

**Properties:**
- **Size**: Default 1MB (256 words), configurable 64KB-16MB
- **Allocation**: Bump pointer (trivial: `ptr -= size; return ptr;`)
- **Collection**: Stop-and-copy when full
- **Frequency**: Very frequent (every ~1MB allocated)

**Structure:**

```rust
pub struct MinorHeap {
    start: *mut Value,       // Base address
    end: *mut Value,         // End of heap
    ptr: *mut Value,         // Current allocation pointer (grows down)
    limit: *mut Value,       // Limit for triggering GC
}

impl MinorHeap {
    /// Allocate a block (NO SAFEPOINTS - fast path)
    #[inline(always)]
    pub fn alloc(&mut self, size_words: usize) -> Option<*mut Block> {
        let new_ptr = unsafe { self.ptr.sub(size_words + 1) }; // +1 for header
        
        if new_ptr < self.limit {
            return None; // Trigger GC
        }
        
        self.ptr = new_ptr;
        Some(new_ptr as *mut Block)
    }
}
```

**Minor GC Cycle:**
1. Scan roots (stack, registers, global roots)
2. Copy live objects to major heap
3. Update pointers (forwarding)
4. Reset allocation pointer

### 3.3 Major Heap (Shared Across Domains)

**Properties:**
- **Size**: Grows dynamically, limited by `Percent_free`
- **Allocation**: Free-list or best-fit
- **Collection**: Concurrent mark-sweep (3 phases)
- **Frequency**: Incremental, concurrent with mutator

**Allocator:**

```rust
pub struct MajorHeap {
    pools: Vec<Pool>,           // Page-sized pools (4KB each)
    free_list: FreeList,        // Free blocks by size class
    mark_stack: Vec<*mut Block>,
    sweep_position: AtomicUsize,
}

impl MajorHeap {
    /// Allocate in major heap (slower path)
    pub fn alloc(&mut self, size_words: usize, tag: u8) -> *mut Block {
        // Try free list first
        if let Some(block) = self.free_list.find(size_words) {
            return block;
        }
        
        // Allocate new pool
        let pool = Pool::new();
        let block = pool.alloc(size_words);
        self.pools.push(pool);
        block
    }
}
```

### 3.4 Stack Layout

Each **domain** has an OCaml stack for function calls:

```
┌─────────────────────────────────────────┐
│              OCaml Stack                 │  (grows downward)
├─────────────────────────────────────────┤
│                                          │
│  Stack High  ← stack base                │
│       │                                  │
│       ├── Return address                 │
│       ├── Environment                    │
│       ├── Extra args                     │
│       ├── Local 0                        │
│       ├── Local 1                        │
│       │   ...                            │
│       ▼                                  │
│  Stack Ptr   ← current SP                │
│       │                                  │
│       ▼                                  │
│  Stack Low   ← red zone/threshold        │
│                                          │
└─────────────────────────────────────────┘
```

**Stack Frame (Function Call):**

```
┌────────────────┐
│  Extra args    │  ← # of args beyond arity
├────────────────┤
│  Environment   │  ← Closure pointer
├────────────────┤
│  Return addr   │  ← PC to return to
├────────────────┤
│  Local 0       │
│  Local 1       │
│  ...           │
└────────────────┘
```

**Implementation:**

```rust
pub struct Stack {
    base: *mut Value,       // High water mark
    sp: *mut Value,         // Current stack pointer
    threshold: *mut Value,  // Trigger realloc if SP drops below
    handler: *mut StackHandler, // Effect handler state
}

impl Stack {
    pub const INITIAL_SIZE: usize = 4096; // words
    pub const MAX_SIZE: usize = 128 * 1024 * 1024; // 128MB
    
    #[inline]
    pub fn push(&mut self, val: Value) {
        unsafe {
            self.sp = self.sp.sub(1);
            *self.sp = val;
        }
    }
    
    #[inline]
    pub fn pop(&mut self) -> Value {
        unsafe {
            let val = *self.sp;
            self.sp = self.sp.add(1);
            val
        }
    }
}
```

---

## 4. Garbage Collection

### 4.1 GC Overview

RAML uses a **hybrid generational + concurrent** GC:

- **Minor GC**: Per-domain, stop-the-world, copying
- **Major GC**: Shared, concurrent, mark-sweep(-compact)
- **Write Barrier**: Track old→young pointers

### 4.2 Minor GC Algorithm

**When:** Minor heap exhausted (bump pointer hits limit)

**Steps:**

1. **Stop domain**: Pause mutator (this domain only)
2. **Scan roots**:
   - Stack (local variables)
   - Registers (accumulator, environment, PC)
   - Global roots
   - Remembered set (old→young pointers)
3. **Copy live objects**: To major heap
4. **Update pointers**: Forwarding pointers in headers
5. **Reset minor heap**: `ptr = end`

**Rust Implementation:**

```rust
impl MinorGC {
    pub fn collect(&mut self, domain: &mut Domain) {
        let mut worklist = Vec::new();
        
        // 1. Scan roots
        self.scan_stack(&domain.stack, &mut worklist);
        self.scan_registers(&domain.regs, &mut worklist);
        self.scan_remembered_set(&domain.remembered_set, &mut worklist);
        
        // 2. Process worklist (Cheney scan)
        while let Some(ptr) = worklist.pop() {
            if self.is_young(ptr) {
                let new_ptr = self.promote_to_major(ptr);
                self.update_references(ptr, new_ptr);
                
                // Scan fields of promoted object
                let block = unsafe { &*new_ptr };
                if block.tag() < NO_SCAN_TAG {
                    for i in 0..block.size() {
                        worklist.push(block.field(i));
                    }
                }
            }
        }
        
        // 3. Reset minor heap
        domain.minor_heap.ptr = domain.minor_heap.end;
        domain.minor_heap.stats.collections += 1;
    }
    
    fn promote_to_major(&mut self, young_ptr: *mut Block) -> *mut Block {
        let block = unsafe { &*young_ptr };
        let size = block.size();
        let tag = block.tag();
        
        // Allocate in major heap
        let new_block = self.major_heap.alloc(size, tag);
        
        // Copy fields
        unsafe {
            std::ptr::copy_nonoverlapping(
                block.fields_ptr(),
                (*new_block).fields_ptr(),
                size,
            );
        }
        
        // Leave forwarding pointer
        block.set_forward(new_block);
        
        new_block
    }
}
```

### 4.3 Major GC Algorithm

**Type:** Concurrent mark-sweep (tri-color marking)

**Phases:**

1. **Phase_sweep_and_mark_main**: Concurrent sweep + marking
2. **Phase_mark_final**: Mark finalizers
3. **Phase_sweep_ephe**: Sweep ephemerons (weak references)

**Colors:**

- **White (unmarked)**: Not yet visited
- **Gray (marked)**: Visited, fields not scanned
- **Black (marked)**: Visited and scanned

**Mark Phase:**

```rust
impl MajorGC {
    pub fn mark_roots(&mut self) {
        // Mark from global roots
        for &root in &self.global_roots {
            self.mark(root);
        }
        
        // Mark from domain stacks
        for domain in &self.domains {
            self.scan_stack(&domain.stack);
        }
    }
    
    fn mark(&mut self, val: Value) {
        if !val.is_block() || self.is_young(val) {
            return;
        }
        
        let block = unsafe { val.as_block_ptr() };
        let header = unsafe { (*block).header() };
        
        // Change color: white → gray
        if header.color() == COLOR_WHITE {
            header.set_color(COLOR_GRAY);
            self.mark_stack.push(block as *mut Block);
        }
    }
    
    pub fn mark_loop(&mut self) {
        while let Some(block_ptr) = self.mark_stack.pop() {
            let block = unsafe { &*block_ptr };
            
            // Scan fields
            if block.tag() < NO_SCAN_TAG {
                for i in 0..block.size() {
                    self.mark(block.field(i));
                }
            }
            
            // Gray → black
            block.header().set_color(COLOR_BLACK);
        }
    }
}
```

**Sweep Phase:**

```rust
impl MajorGC {
    pub fn sweep(&mut self) {
        for pool in &mut self.pools {
            pool.sweep(&mut self.free_list);
        }
    }
}

impl Pool {
    fn sweep(&mut self, free_list: &mut FreeList) {
        let mut cursor = self.start;
        
        while cursor < self.end {
            let block = unsafe { &mut *(cursor as *mut Block) };
            let size = block.size();
            
            match block.header().color() {
                COLOR_WHITE => {
                    // Dead object - add to free list
                    free_list.add(block);
                }
                COLOR_BLACK | COLOR_GRAY => {
                    // Live object - reset to white for next cycle
                    block.header().set_color(COLOR_WHITE);
                }
                _ => unreachable!(),
            }
            
            cursor += size + 1; // +1 for header
        }
    }
}
```

### 4.4 Write Barriers

To maintain generational invariant (no old→young refs missed):

```rust
impl Domain {
    /// Store a value into a field (write barrier)
    pub fn set_field(&mut self, block: *mut Block, index: usize, value: Value) {
        unsafe {
            (*block).set_field(index, value);
        }
        
        // Write barrier: if old→young write, remember it
        if self.is_old(block) && self.is_young(value) {
            self.remembered_set.insert(block);
        }
    }
}
```

### 4.5 GC Statistics

Track allocation/collection metrics:

```rust
pub struct GCStats {
    pub minor_words: u64,        // Words allocated in minor heap
    pub minor_collections: u64,  // # of minor GCs
    pub major_words: u64,        // Words allocated in major heap
    pub major_collections: u64,  // # of major GC cycles
    pub compactions: u64,        // # of heap compactions
    pub heap_words: usize,       // Current heap size (words)
    pub heap_chunks: usize,      // # of contiguous heap chunks
    pub live_words: usize,       // Words in live blocks
    pub free_words: usize,       // Words in free list
    pub top_heap_words: usize,   // Max heap size reached
}
```

---

## 5. Bytecode Format

### 5.1 Bytecode File Structure

OCaml bytecode files (`.cmo`, `.cma`, executables) contain multiple sections:

```
┌──────────────────────────────────────┐
│   Bytecode Executable File            │
├──────────────────────────────────────┤
│                                       │
│  ┌────────────────────────────────┐  │
│  │  CODE Section                  │  │
│  │  (bytecode instructions)       │  │
│  └────────────────────────────────┘  │
│                                       │
│  ┌────────────────────────────────┐  │
│  │  DATA Section                  │  │
│  │  (marshaled constants)         │  │
│  └────────────────────────────────┘  │
│                                       │
│  ┌────────────────────────────────┐  │
│  │  SYMB Section                  │  │
│  │  (symbol table)                │  │
│  └────────────────────────────────┘  │
│                                       │
│  ┌────────────────────────────────┐  │
│  │  PRIM Section                  │  │
│  │  (primitive names)             │  │
│  └────────────────────────────────┘  │
│                                       │
│  ┌────────────────────────────────┐  │
│  │  DBUG Section (optional)       │  │
│  │  (debug events)                │  │
│  └────────────────────────────────┘  │
│                                       │
│  ┌────────────────────────────────┐  │
│  │  Trailer                       │  │
│  │  - Section offsets             │  │
│  │  - Magic number                │  │
│  └────────────────────────────────┘  │
│                                       │
└──────────────────────────────────────┘
```

### 5.2 Trailer Format

Located at **end of file**, fixed size (32 bytes):

```rust
pub struct ExecTrailer {
    pub num_sections: u32,       // Number of sections
    pub sections: [u32; 5],      // Offsets to each section (from EOF)
    pub magic: [u8; 12],         // Magic number: "Caml1999X033"
}

pub const EXEC_MAGIC: &[u8] = b"Caml1999X033";
pub const TRAILER_SIZE: usize = 32;
```

**Reading Trailer:**

```rust
impl BytecodeFile {
    pub fn read_trailer(&mut self) -> Result<ExecTrailer> {
        // Seek to end - 32 bytes
        self.file.seek(SeekFrom::End(-32))?;
        
        let mut buf = [0u8; 32];
        self.file.read_exact(&mut buf)?;
        
        let trailer = ExecTrailer::parse(&buf)?;
        
        if &trailer.magic != EXEC_MAGIC {
            return Err(Error::WrongMagic);
        }
        
        Ok(trailer)
    }
}
```

### 5.3 Section Formats

#### CODE Section

Raw bytecode instructions (array of `u8` or `u32` opcodes):

```rust
pub struct CodeSection {
    pub code: Vec<u32>,  // Opcodes (32-bit on 64-bit platform, 8-bit for some)
}
```

#### DATA Section

Marshaled OCaml values (constants, strings, floats):

```rust
pub struct DataSection {
    pub data: Value,  // Root of marshaled data structure
}
```

**Marshal Format:** Binary serialization of OCaml values
- Headers: `0x8495A6B*` (version-specific)
- Sharing: Back-references for structure sharing
- External: References to primitives by index

#### PRIM Section

Null-terminated strings of primitive names:

```
"caml_ml_string_length\0caml_ml_array_get\0...\0"
```

```rust
pub struct PrimSection {
    pub primitives: Vec<String>,  // ["caml_ml_string_length", ...]
}

impl PrimSection {
    pub fn parse(data: &[u8]) -> Result<Self> {
        let mut prims = Vec::new();
        let mut start = 0;
        
        for (i, &byte) in data.iter().enumerate() {
            if byte == 0 {
                let name = std::str::from_utf8(&data[start..i])?;
                prims.push(name.to_string());
                start = i + 1;
            }
        }
        
        Ok(PrimSection { primitives: prims })
    }
}
```

#### SYMB Section

Global symbol table (module names → code offsets):

```rust
pub struct SymbSection {
    pub symbols: HashMap<String, usize>,  // Module name → code offset
}
```

#### DBUG Section

Debug events (source locations):

```rust
pub struct DebugEvent {
    pub pos: usize,              // Bytecode position
    pub module: String,          // Module name
    pub loc: SourceLocation,     // File:line:col
    pub kind: EventKind,         // Before/After/Pseudo
}

pub enum EventKind {
    Before,
    After,
    Pseudo,
}
```

### 5.4 Loading Bytecode

```rust
pub struct BytecodeLoader {
    pub file: File,
    pub trailer: ExecTrailer,
}

impl BytecodeLoader {
    pub fn load(path: &Path) -> Result<LoadedBytecode> {
        let mut file = File::open(path)?;
        
        // Read trailer
        let trailer = Self::read_trailer(&mut file)?;
        
        // Read sections
        let code = Self::read_code_section(&mut file, &trailer)?;
        let data = Self::read_data_section(&mut file, &trailer)?;
        let prims = Self::read_prim_section(&mut file, &trailer)?;
        let symb = Self::read_symb_section(&mut file, &trailer)?;
        let debug = Self::read_debug_section(&mut file, &trailer)?;
        
        Ok(LoadedBytecode {
            code,
            data,
            primitives: prims,
            symbols: symb,
            debug,
        })
    }
}
```

---

## 6. Instruction Set

### 6.1 Instruction Encoding

Instructions are 1-4 bytes:

```
┌────────┬────────────────────────────────┐
│ Opcode │  Operands (0-3 bytes)          │
│ 1 byte │  (optional)                    │
└────────┴────────────────────────────────┘
```

**Operand Encoding:**
- Small integers: 1 byte unsigned (0-255)
- Larger integers: 2 or 4 bytes
- Labels: Relative offsets (signed)

### 6.2 Core Instructions (~100 total)

#### Stack Manipulation

| Opcode | Name | Operands | Effect |
|--------|------|----------|--------|
| 0 | `ACC0` | - | `accu ← sp[0]` |
| 1 | `ACC1` | - | `accu ← sp[1]` |
| 8 | `ACC` | n | `accu ← sp[n]` |
| 9 | `PUSH` | - | `*--sp ← accu` |
| 10 | `PUSHACC0` | - | `*--sp ← accu; accu ← sp[0]` |
| 18 | `POP` | n | `sp += n` |
| 19 | `ASSIGN` | n | `sp[n] ← accu; accu ← unit` |

#### Environment Access

| Opcode | Name | Operands | Effect |
|--------|------|----------|--------|
| 20 | `ENVACC1` | - | `accu ← env[1]` |
| 24 | `ENVACC` | n | `accu ← env[n]` |
| 25 | `PUSHENVACC1` | - | `*--sp ← accu; accu ← env[1]` |

#### Function Calls

| Opcode | Name | Operands | Effect |
|--------|------|----------|--------|
| 30 | `PUSH_RETADDR` | offset | Push return address on stack |
| 31 | `APPLY` | nargs | Apply closure to `nargs` arguments |
| 32 | `APPLY1` | - | Fast path: 1 argument |
| 33 | `APPLY2` | - | Fast path: 2 arguments |
| 34 | `APPLY3` | - | Fast path: 3 arguments |
| 35 | `APPTERM` | nargs, slotsize | Tail call with `nargs` |
| 39 | `RETURN` | n | Return from function, pop `n` |
| 40 | `RESTART` | - | Restart function (for currying) |
| 41 | `GRAB` | required | Grab required arguments (currying) |

#### Closures

| Opcode | Name | Operands | Effect |
|--------|------|----------|--------|
| 42 | `CLOSURE` | nvars, offset | Create closure with `nvars` free vars |
| 43 | `CLOSUREREC` | nfuncs, nvars, offsets... | Recursive closures |
| 44-48 | `OFFSETCLOSURE*` | offset | Access closure environment |

#### Global Access

| Opcode | Name | Operands | Effect |
|--------|------|----------|--------|
| 52 | `GETGLOBAL` | n | `accu ← globals[n]` |
| 53 | `PUSHGETGLOBAL` | n | `*--sp ← accu; accu ← globals[n]` |
| 54 | `GETGLOBALFIELD` | n, p | `accu ← globals[n][p]` |
| 56 | `SETGLOBAL` | n | `globals[n] ← accu` |

#### Constants

| Opcode | Name | Operands | Effect |
|--------|------|----------|--------|
| 57 | `ATOM0` | - | `accu ← Atom(0)` (pre-allocated) |
| 58 | `ATOM` | tag | `accu ← Atom(tag)` |
| 74 | `CONST0` | - | `accu ← Val_int(0)` |
| 78 | `CONSTINT` | n | `accu ← Val_int(n)` |

#### Block Allocation

| Opcode | Name | Operands | Effect |
|--------|------|----------|--------|
| 60 | `MAKEBLOCK` | size, tag | Allocate block, fill from stack |
| 61 | `MAKEBLOCK1` | tag | Fast path: size=1 |
| 62 | `MAKEBLOCK2` | tag | Fast path: size=2 |
| 63 | `MAKEBLOCK3` | tag | Fast path: size=3 |
| 64 | `MAKEFLOATBLOCK` | size | Allocate float array |

#### Field Access

| Opcode | Name | Operands | Effect |
|--------|------|----------|--------|
| 65 | `GETFIELD0` | - | `accu ← accu[0]` |
| 66 | `GETFIELD1` | - | `accu ← accu[1]` |
| 69 | `GETFIELD` | n | `accu ← accu[n]` |
| 71 | `SETFIELD0` | - | `accu[0] ← sp[0]; sp++` |
| 75 | `SETFIELD` | n | `accu[n] ← sp[0]; sp++` (write barrier!) |

#### Control Flow

| Opcode | Name | Operands | Effect |
|--------|------|----------|--------|
| 82 | `BRANCH` | offset | `pc += offset` |
| 83 | `BRANCHIF` | offset | `if accu != 0: pc += offset` |
| 84 | `BRANCHIFNOT` | offset | `if accu == 0: pc += offset` |
| 85 | `SWITCH` | n, offsets... | Multi-way branch on `accu` |

#### Integer Arithmetic

| Opcode | Name | Operands | Effect |
|--------|------|----------|--------|
| 88 | `NEGINT` | - | `accu ← -accu` (tagged int) |
| 89 | `ADDINT` | - | `accu ← accu + sp[0]; sp++` |
| 90 | `SUBINT` | - | `accu ← accu - sp[0]; sp++` |
| 91 | `MULINT` | - | `accu ← accu * sp[0]; sp++` |
| 92 | `DIVINT` | - | `accu ← accu / sp[0]; sp++` |
| 93 | `MODINT` | - | `accu ← accu % sp[0]; sp++` |

#### Comparisons

| Opcode | Name | Operands | Effect |
|--------|------|----------|--------|
| 100 | `EQ` | - | `accu ← (accu == sp[0])` |
| 101 | `NEQ` | - | `accu ← (accu != sp[0])` |
| 102 | `LTINT` | - | `accu ← (accu < sp[0])` |
| 103 | `LEINT` | - | `accu ← (accu <= sp[0])` |
| 104 | `GTINT` | - | `accu ← (accu > sp[0])` |
| 105 | `GEINT` | - | `accu ← (accu >= sp[0])` |

#### C Calls

| Opcode | Name | Operands | Effect |
|--------|------|----------|--------|
| 119 | `C_CALL1` | prim | Call C primitive with 1 arg |
| 120 | `C_CALL2` | prim | Call C primitive with 2 args |
| 121 | `C_CALL3` | prim | Call C primitive with 3 args |
| 122 | `C_CALL4` | prim | Call C primitive with 4 args |
| 123 | `C_CALL5` | prim | Call C primitive with 5 args |
| 124 | `C_CALLN` | nargs, prim | Call C primitive with n args |

#### Exceptions

| Opcode | Name | Operands | Effect |
|--------|------|----------|--------|
| 86 | `PUSHTRAP` | offset | Push exception handler |
| 87 | `POPTRAP` | - | Pop exception handler |
| 118 | `RAISE` | - | Raise exception (accu) |
| 144 | `RERAISE` | - | Re-raise exception |
| 145 | `RAISE_NOTRACE` | - | Raise without backtrace |

#### Effect Handlers

| Opcode | Name | Operands | Effect |
|--------|------|----------|--------|
| 147 | `PERFORM` | - | Perform effect (accu) |
| 148 | `RESUME` | - | Resume continuation |
| 149 | `RESUMETERM` | - | Resume continuation (tail) |
| 150 | `REPERFORMTERM` | - | Re-perform effect (tail) |

### 6.3 Interpreter Loop

```rust
pub struct Interpreter {
    pub pc: *const u32,         // Program counter
    pub accu: Value,            // Accumulator register
    pub env: Value,             // Environment (closure pointer)
    pub sp: *mut Value,         // Stack pointer
    pub extra_args: isize,      // Extra arguments
    pub domain: *mut Domain,    // Current domain
}

impl Interpreter {
    pub fn run(&mut self, code: &[u32]) -> Result<Value> {
        loop {
            let instr = unsafe { *self.pc };
            self.pc = unsafe { self.pc.add(1) };
            
            match instr {
                // ACC0
                0 => {
                    self.accu = unsafe { *self.sp };
                }
                
                // PUSH
                9 => {
                    self.sp = unsafe { self.sp.sub(1) };
                    unsafe { *self.sp = self.accu };
                }
                
                // MAKEBLOCK
                60 => {
                    let size = unsafe { *self.pc } as usize;
                    self.pc = unsafe { self.pc.add(1) };
                    let tag = unsafe { *self.pc } as u8;
                    self.pc = unsafe { self.pc.add(1) };
                    
                    // Allocate block
                    let block = self.alloc_block(size, tag)?;
                    
                    // Fill fields from stack
                    for i in 0..size {
                        unsafe {
                            (*block).set_field(i, *self.sp.add(i));
                        }
                    }
                    self.sp = unsafe { self.sp.add(size) };
                    self.accu = Value::from_block_ptr(block);
                }
                
                // APPLY
                31 => {
                    let nargs = unsafe { *self.pc } as isize;
                    self.pc = unsafe { self.pc.add(1) };
                    self.apply_closure(nargs)?;
                }
                
                // C_CALL1
                119 => {
                    let prim = unsafe { *self.pc } as usize;
                    self.pc = unsafe { self.pc.add(1) };
                    self.accu = self.call_primitive_1(prim, self.accu)?;
                }
                
                // ... 100+ more instructions
                
                _ => return Err(Error::InvalidOpcode(instr)),
            }
        }
    }
}
```

---

## 7. Calling Convention

### 7.1 Function Calls

OCaml uses a **stack-based** calling convention with an **accumulator** register.

**Caller:**
1. Push arguments onto stack (right-to-left)
2. Load closure into `accu`
3. Execute `APPLY n` (where n = # of args)

**Callee:**
1. Check arity (via `GRAB` instruction if needed)
2. Access arguments from stack: `sp[0]`, `sp[1]`, ...
3. Access free variables: `env[1]`, `env[2]`, ...
4. Return value in `accu`
5. Execute `RETURN n` (pop n locals)

**Example:**

OCaml:
```ocaml
let f x y = x + y in
f 1 2
```

Bytecode:
```
CONST1        ; accu = 1
PUSH          ; sp -= 1; *sp = accu
CONST2        ; accu = 2
PUSH          ; sp -= 1; *sp = accu
GETGLOBAL f   ; accu = closure
APPLY 2       ; call with 2 args
```

Inside `f`:
```
ACC 0         ; accu = sp[0] (x)
PUSH          ; save x
ACC 1         ; accu = sp[1] (y)
ADDINT        ; accu = accu + sp[0]
RETURN 2      ; pop 2 args, return
```

### 7.2 Tail Calls

Tail calls use `APPTERM` to avoid stack growth:

```
APPTERM nargs, slotsize
```

**Effect:**
1. Pop `slotsize` slots from stack
2. Move arguments to correct positions
3. Jump to closure (no return address pushed)

### 7.3 Closures

Closures bundle:
- **Code pointer** (first field)
- **Free variables** (remaining fields)

**Layout:**

```
┌─────────────────────────────────────┐
│  Closure Block (tag = 247)          │
├─────────────────────────────────────┤
│  Field 0: Code pointer              │
│  Field 1: Closure info (arity)      │
│  Field 2: Free variable 0           │
│  Field 3: Free variable 1           │
│  ...                                │
└─────────────────────────────────────┘
```

**Closure Info:**
- High 8 bits: Arity (signed)
- Low bits: Start of environment (field offset)

```rust
pub struct Closure {
    pub code: *const u32,       // Code pointer
    pub closinfo: usize,        // Arity + env offset
    pub env: Vec<Value>,        // Free variables
}

impl Closure {
    pub fn arity(&self) -> isize {
        (self.closinfo >> 56) as i8 as isize  // Sign-extend
    }
    
    pub fn start_env(&self) -> usize {
        (self.closinfo << 8) >> 9
    }
}
```

**Partial Application:**

If `nargs < arity`, create a new closure capturing provided args:

```rust
impl Interpreter {
    fn apply_closure(&mut self, nargs: isize) -> Result<()> {
        let closure = unsafe { self.accu.as_block_ptr() };
        let arity = Closure::arity(closure);
        
        if nargs == arity {
            // Exact application - jump to code
            self.env = self.accu;
            self.extra_args = 0;
            self.pc = Closure::code(closure);
        } else if nargs < arity {
            // Partial application - build new closure
            let new_closure = self.build_partial_closure(closure, nargs)?;
            self.accu = Value::from_block_ptr(new_closure);
        } else {
            // Over-application - call then apply remaining args
            let extra = nargs - arity;
            self.extra_args = extra;
            self.env = self.accu;
            self.pc = Closure::code(closure);
        }
        
        Ok(())
    }
}
```

---

## 8. Concurrency Model

### 8.1 Domains (OS Threads)

OCaml 5 supports **true parallelism** via domains.

**Properties:**
- Each domain = 1 OS thread
- Domains run in parallel
- Each domain has private minor heap
- Domains share major heap (concurrent GC)

**Domain State:**

```rust
pub struct Domain {
    pub id: usize,                   // Domain ID (0-based)
    pub thread: pthread_t,           // OS thread handle
    
    // Execution state
    pub pc: *const u32,              // Program counter
    pub accu: Value,                 // Accumulator
    pub env: Value,                  // Environment
    pub stack: Stack,                // OCaml stack
    pub extra_args: isize,
    
    // Memory
    pub minor_heap: MinorHeap,       // Per-domain young generation
    pub remembered_set: HashSet<*mut Block>, // Old→young refs
    
    // GC
    pub young_limit: AtomicUsize,    // Trigger GC/interrupt
    pub allocated_words: usize,      // Words allocated
    
    // Synchronization
    pub interrupt_pending: AtomicBool,
    pub backup_thread: Option<pthread_t>,
}
```

### 8.2 Stop-The-World (STW)

For GC phase changes, all domains must synchronize:

```rust
pub struct STWBarrier {
    pub num_participants: AtomicUsize,
    pub count: AtomicUsize,
    pub sense: AtomicBool,
}

impl STWBarrier {
    pub fn enter(&self, num_participants: usize) {
        let sense = self.sense.load(Ordering::Relaxed);
        
        // Arrive
        let count = self.count.fetch_add(1, Ordering::SeqCst);
        
        if count + 1 == num_participants {
            // Last to arrive - release others
            self.count.store(0, Ordering::SeqCst);
            self.sense.store(!sense, Ordering::Release);
        } else {
            // Spin until released
            while self.sense.load(Ordering::Acquire) == sense {
                std::hint::spin_loop();
            }
        }
    }
}
```

**STW Critical Sections:**

1. **Minor GC**: Each domain independently (no STW)
2. **Major GC phase change**: All domains sync
3. **Heap compaction**: All domains pause

### 8.3 Domain Spawning

```rust
impl Runtime {
    pub fn spawn_domain(&mut self, closure: Value) -> Result<DomainId> {
        let domain_id = self.next_domain_id();
        
        // Create domain state
        let mut domain = Domain::new(domain_id);
        domain.minor_heap = MinorHeap::new(MINOR_HEAP_SIZE);
        
        // Spawn OS thread
        let handle = std::thread::spawn(move || {
            domain.run(closure)
        });
        
        self.domains.insert(domain_id, handle);
        Ok(domain_id)
    }
}
```

### 8.4 Interrupts & Polling

Domains poll for interrupts at:
- **Allocation**: Check `young_limit` on minor heap exhaustion
- **Backwards branches**: Compiler inserts `CHECK_SIGNALS`
- **Function calls**: Check before entering callee

```rust
impl Interpreter {
    #[inline(always)]
    fn check_interrupt(&mut self) -> Result<()> {
        let domain = unsafe { &*self.domain };
        
        if domain.young_ptr < domain.young_limit {
            self.handle_interrupt()?;
        }
        
        Ok(())
    }
    
    fn handle_interrupt(&mut self) -> Result<()> {
        // Could be:
        // 1. Minor GC needed
        // 2. STW request
        // 3. Signal pending
        
        if self.minor_heap_full() {
            self.minor_gc()?;
        }
        
        if self.stw_pending() {
            self.participate_stw()?;
        }
        
        if self.signal_pending() {
            self.handle_signals()?;
        }
        
        Ok(())
    }
}
```

---

## 9. Effect Handlers

### 9.1 Delimited Continuations

OCaml 5 supports **effect handlers** (algebraic effects):

```ocaml
effect Yield : unit
effect Spawn : (unit -> unit) -> unit

let run f =
  match f () with
  | () -> ()
  | effect Yield k -> resume k ()
  | effect (Spawn f) k -> resume k (); run f
```

**Implementation:** Stack switching with fibers.

### 9.2 Stack (Fiber) Structure

Each continuation has its own stack:

```rust
pub struct StackInfo {
    pub sp: *mut Value,              // Stack pointer
    pub exception_ptr: *mut Value,   // Exception handler
    pub handler: *mut StackHandler,  // Effect handler
    pub cache_bucket: isize,         // For pooling
    pub size: usize,                 // Stack size
    pub id: i64,                     // Unique ID
}

pub struct StackHandler {
    pub handle_value: Value,      // Normal return continuation
    pub handle_exn: Value,        // Exception continuation
    pub handle_effect: Value,     // Effect handler
    pub parent: Option<Box<StackInfo>>, // Parent stack (when suspended)
}
```

### 9.3 Effect Operations

**PERFORM:**
1. Capture current stack (create continuation)
2. Switch to parent stack
3. Invoke effect handler

```rust
impl Interpreter {
    fn perform_effect(&mut self, effect: Value) -> Result<()> {
        let current_stack = self.current_stack();
        
        // Capture continuation
        let cont = Continuation {
            stack: current_stack,
            pc: self.pc,
            accu: self.accu,
            env: self.env,
        };
        
        // Switch to parent
        if let Some(parent) = current_stack.handler.parent {
            self.switch_stack(parent);
            
            // Call effect handler: handler(effect, cont)
            let handler = parent.handler.handle_effect;
            self.accu = effect;
            self.push(Value::from_cont(cont));
            self.apply_closure(handler, 2)?;
        } else {
            // No handler - raise Unhandled exception
            return Err(Error::UnhandledEffect(effect));
        }
        
        Ok(())
    }
}
```

**RESUME:**
1. Install continuation's stack as current
2. Set parent to current stack
3. Continue execution

```rust
impl Interpreter {
    fn resume_continuation(&mut self, cont: Continuation, value: Value) -> Result<()> {
        let current_stack = self.current_stack();
        let cont_stack = cont.stack;
        
        // Set parent link
        cont_stack.handler.parent = Some(current_stack);
        
        // Switch to continuation's stack
        self.pc = cont.pc;
        self.accu = value;
        self.env = cont.env;
        self.stack = cont_stack;
        
        Ok(())
    }
}
```

### 9.4 Stack Pool

To avoid allocation overhead, reuse stacks:

```rust
pub struct StackCache {
    buckets: Vec<Vec<Box<StackInfo>>>,  // By size class
}

impl StackCache {
    pub fn alloc(&mut self, size: usize) -> Box<StackInfo> {
        let bucket = self.size_to_bucket(size);
        
        if let Some(stack) = self.buckets[bucket].pop() {
            stack
        } else {
            StackInfo::new(size)
        }
    }
    
    pub fn free(&mut self, stack: Box<StackInfo>) {
        let bucket = self.size_to_bucket(stack.size);
        self.buckets[bucket].push(stack);
    }
}
```

---

## 10. Foreign Function Interface

### 10.1 Primitive Registration

C primitives are registered by name:

```rust
pub struct PrimitiveTable {
    pub names: Vec<String>,
    pub functions: Vec<PrimitiveFn>,
}

pub type PrimitiveFn = fn(&mut Interpreter, &[Value]) -> Result<Value>;

impl PrimitiveTable {
    pub fn register(&mut self, name: &str, func: PrimitiveFn) {
        self.names.push(name.to_string());
        self.functions.push(func);
    }
    
    pub fn lookup(&self, name: &str) -> Option<usize> {
        self.names.iter().position(|n| n == name)
    }
}
```

### 10.2 Primitive Calling

From bytecode:

```
C_CALL1 prim_index
```

```rust
impl Interpreter {
    fn call_primitive_1(&mut self, prim: usize, arg: Value) -> Result<Value> {
        let func = self.primitives.functions[prim];
        
        // Setup for GC (primitives may allocate)
        self.setup_for_c_call();
        
        let result = func(self, &[arg])?;
        
        self.restore_after_c_call();
        
        Ok(result)
    }
}
```

### 10.3 Root Tracking

C code must register GC roots:

```rust
// Rust equivalent of CAMLparam/CAMLlocal
pub struct RootFrame<'a> {
    prev: Option<&'a RootFrame<'a>>,
    roots: Vec<*mut Value>,
}

impl<'a> RootFrame<'a> {
    pub fn new(interpreter: &'a Interpreter) -> Self {
        RootFrame {
            prev: interpreter.local_roots,
            roots: Vec::new(),
        }
    }
    
    pub fn register(&mut self, root: *mut Value) {
        self.roots.push(root);
    }
}

// Use with RAII
pub fn primitive_example(interp: &mut Interpreter, args: &[Value]) -> Result<Value> {
    let mut frame = RootFrame::new(interp);
    
    let mut local1 = args[0];
    frame.register(&mut local1);
    
    let mut local2 = interp.alloc_block(10, 0)?;
    frame.register(&mut local2);
    
    // GC may happen here - local1, local2 are safe
    
    Ok(local2)
}
```

### 10.4 Standard Primitives

Must implement:

**String operations:**
- `caml_ml_string_length`
- `caml_string_get`
- `caml_string_set`
- `caml_string_compare`

**Array operations:**
- `caml_array_length`
- `caml_array_get`
- `caml_array_set`
- `caml_make_vect`

**I/O operations:**
- `caml_ml_open_descriptor_in`
- `caml_ml_open_descriptor_out`
- `caml_ml_input`
- `caml_ml_output`
- `caml_ml_close_channel`

**System operations:**
- `caml_sys_exit`
- `caml_sys_get_argv`
- `caml_sys_get_config`

---

## 11. Implementation Roadmap

### Phase 1: Foundation (Weeks 1-2)

**Goal:** Load and parse bytecode files

- [ ] Define `Value` type with tagged integers
- [ ] Implement `BlockHeader` and `Block` structures
- [ ] Parse bytecode file format (trailer, sections)
- [ ] Load CODE, DATA, PRIM sections
- [ ] Marshal/unmarshal OCaml values

**Deliverable:** Tool that prints bytecode file contents

### Phase 2: Simple Interpreter (Weeks 3-4)

**Goal:** Execute basic bytecode (no GC)

- [ ] Implement interpreter loop (fetch-decode-execute)
- [ ] Stack manipulation (PUSH, POP, ACC)
- [ ] Constants (CONST, ATOM)
- [ ] Integer arithmetic (ADD, SUB, MUL, DIV)
- [ ] Control flow (BRANCH, BRANCHIF)
- [ ] Implement primitive stubs (error on call)

**Deliverable:** Run simple programs (arithmetic, conditionals)

### Phase 3: Memory Management (Weeks 5-7)

**Goal:** Allocation + basic GC

- [ ] Implement minor heap (bump allocator)
- [ ] Block allocation (MAKEBLOCK)
- [ ] Field access (GETFIELD, SETFIELD)
- [ ] Minor GC (stop-and-copy)
- [ ] Root scanning (stack, registers)
- [ ] Write barriers

**Deliverable:** Run programs with allocation (lists, tuples)

### Phase 4: Function Calls (Weeks 8-9)

**Goal:** Closures and function application

- [ ] Closure creation (CLOSURE, CLOSUREREC)
- [ ] Function application (APPLY, APPTERM)
- [ ] Calling convention (stack frame setup)
- [ ] Currying (GRAB, partial application)
- [ ] Tail call optimization

**Deliverable:** Run programs with higher-order functions

### Phase 5: Major GC (Weeks 10-12)

**Goal:** Major heap + mark-sweep GC

- [ ] Major heap allocator (free lists, pools)
- [ ] Major GC mark phase (tri-color)
- [ ] Major GC sweep phase
- [ ] Incremental collection
- [ ] GC statistics

**Deliverable:** Long-running programs without OOM

### Phase 6: Exceptions (Week 13)

**Goal:** Exception handling

- [ ] Exception raising (RAISE)
- [ ] Exception handlers (PUSHTRAP, POPTRAP)
- [ ] Backtrace support

**Deliverable:** Programs with try/catch

### Phase 7: Primitives (Weeks 14-16)

**Goal:** Standard library primitives

- [ ] String operations
- [ ] Array operations
- [ ] I/O operations
- [ ] System operations
- [ ] Int32/Int64 operations

**Deliverable:** Run real OCaml programs

### Phase 8: Multicore (Weeks 17-20)

**Goal:** Parallel domains

- [ ] Domain spawning
- [ ] Domain-local state
- [ ] STW synchronization
- [ ] Concurrent major GC
- [ ] Load balancing

**Deliverable:** Parallel programs

### Phase 9: Effects (Weeks 21-24)

**Goal:** Effect handlers

- [ ] Stack structure (StackInfo)
- [ ] Stack switching (PERFORM, RESUME)
- [ ] Stack pool (caching)
- [ ] Continuation cloning

**Deliverable:** Programs with effects

### Phase 10: Optimization (Weeks 25+)

**Goal:** Performance tuning

- [ ] Profile interpreter bottlenecks
- [ ] Optimize hot paths (threaded code?)
- [ ] JIT compilation exploration
- [ ] Memory layout optimization

---

## Conclusion

RAML aims to be a **production-ready, Rust-native OCaml bytecode runtime** that matches OCaml's semantics while enabling new use cases (embedding, WebAssembly, etc.).

**Key Challenges:**
1. **Correctness**: OCaml runtime has 25+ years of bug fixes
2. **Performance**: Must not be significantly slower than C runtime
3. **Completeness**: Full primitive library is large

**Key Advantages:**
1. **Safety**: Rust prevents entire classes of runtime bugs
2. **Embedding**: First-class Rust integration
3. **Portability**: Easier cross-platform builds
4. **Maintainability**: Modern codebase with tests

**Next Steps:** Start with Phase 1 - get bytecode parsing working!
