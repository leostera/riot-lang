# OCaml Compiler and Runtime Architecture

## Overview

The OCaml system consists of two main parts that work together:

1. **The Compiler** (OCaml code in `./ocaml/compiler/`)
2. **The Runtime System** (C code in `./ocaml/compiler/runtime/`)

This document explains how they fit together.

---

## The Compilation Pipeline

### 1. Source → Bytecode

```
┌─────────────┐
│  OCaml      │
│  Source     │
│  (.ml)      │
└──────┬──────┘
       │
       ▼
┌─────────────────────┐
│  Parsing            │
│  (parsing/)         │
│  .ml → Parsetree    │
└──────┬──────────────┘
       │
       ▼
┌─────────────────────┐
│  Typing             │
│  (typing/)          │
│  Parsetree →        │
│  Typedtree          │
└──────┬──────────────┘
       │
       ▼
┌─────────────────────┐
│  Lambda             │
│  (lambda/)          │
│  Typedtree →        │
│  Lambda IR          │
└──────┬──────────────┘
       │
       ▼
┌─────────────────────┐
│  Bytecode Gen       │
│  (bytecomp/)        │
│  Lambda → Instruct  │
└──────┬──────────────┘
       │
       ▼
┌─────────────────────┐
│  Bytecode Emit      │
│  Instruct → .cmo    │
│  (bytecode file)    │
└─────────────────────┘
```

### 2. Bytecode File Format (.cmo / .cma / executable)

Bytecode executables contain several **sections**:

```
┌──────────────────────────┐
│  Bytecode Instructions   │  ← Actual VM opcodes
├──────────────────────────┤
│  Data Section            │  ← Constants, string literals
├──────────────────────────┤
│  Symbol Table            │  ← Global identifiers
├──────────────────────────┤
│  Primitive Table         │  ← References to C functions
├──────────────────────────┤
│  Debug Info              │  ← Source locations
├──────────────────────────┤
│  Trailer                 │  ← Metadata & magic number
└──────────────────────────┘
```

**Key Components:**

- **Instructions**: Encoded as bytes, each opcode is 1 byte + operands
- **Symbol Table**: Maps global module identifiers to bytecode positions
- **Primitive Table**: Maps primitive names (like `"caml_string_length"`) to indices
- **Trailer**: Contains section offsets and magic number `"Caml1999X033"` (version-dependent)

---

## The Runtime System Architecture

### Runtime Directory Structure

```
runtime/
├── caml/           ← Header files (public API)
│   ├── mlvalues.h  ← Value representation
│   ├── alloc.h     ← Allocation functions
│   ├── memory.h    ← GC interface
│   ├── domain.h    ← Multicore domains
│   ├── fiber.h     ← Effect handler stacks
│   └── ...
│
├── Core Runtime:
│   ├── interp.c         ← Bytecode interpreter (main loop)
│   ├── alloc.c          ← Allocation functions
│   ├── minor_gc.c       ← Minor (generational) GC
│   ├── major_gc.c       ← Major (concurrent) GC
│   ├── memory.c         ← Memory management
│   ├── roots.c          ← GC root tracking
│   └── startup_byt.c    ← Bytecode startup
│
├── Multicore:
│   ├── domain.c         ← Domain (OS thread) management
│   ├── shared_heap.c    ← Shared major heap
│   ├── fiber.c          ← Stack management for effects
│   └── signals.c        ← Signal & interrupt handling
│
├── Primitives (C functions callable from OCaml):
│   ├── array.c          ← Array operations
│   ├── floats.c         ← Floating point ops
│   ├── ints.c           ← Int32/Int64 operations
│   ├── io.c             ← I/O operations
│   ├── str.c            ← String operations
│   ├── sys.c            ← System operations
│   ├── unix.c           ← Unix-specific ops
│   └── ...
│
├── FFI & Dynamic Linking:
│   ├── callback.c       ← C → OCaml calls
│   ├── dynlink.c        ← Dynamic loading
│   └── custom.c         ← Custom C types
│
└── Architecture-specific:
    ├── amd64.S          ← x86-64 assembly helpers
    ├── arm64.S          ← ARM64 assembly helpers
    └── ...
```

---

## How Compiler and Runtime Connect

### 1. Bytecode Instructions → Runtime Interpreter

**Compiler Side** (`bytecomp/instruct.ml`):
```ocaml
type instruction =
  | Kacc of int              (* Access stack slot *)
  | Kpush                    (* Push accumulator *)
  | Kapply of int            (* Function application *)
  | Kmakeblock of int * int  (* Allocate block *)
  | Kgetfield of int         (* Get block field *)
  | ...
```

**Emitted as** (`bytecomp/opcodes.ml`):
```ocaml
let opACC0 = 0
let opACC1 = 1
let opPUSH = 9
let opAPPLY = 32
let opMAKEBLOCK = 50
...
```

**Runtime Side** (`runtime/interp.c`):
```c
enum instructions {
  ACC0, ACC1, ..., PUSH, ..., 
  APPLY, ..., MAKEBLOCK, ...
};

// Main interpreter loop
value caml_interprete(code_t prog) {
  while (1) {
    opcode_t instr = *pc++;  // Fetch instruction
    
    switch (instr) {
      case ACC0:
        accu = sp[0];
        break;
      
      case PUSH:
        *--sp = accu;
        break;
      
      case APPLY:
        // ... complex calling convention
        break;
      
      case MAKEBLOCK: {
        int size = *pc++;
        int tag = *pc++;
        accu = caml_alloc(size, tag);  // ← Calls into GC
        // ... initialize fields
        break;
      }
      // ...
    }
  }
}
```

### 2. Primitives → C Functions

**Compiler generates:** References to named C primitives

Example OCaml code:
```ocaml
external string_length : string -> int = "caml_ml_string_length"
```

**Compiler emits:**
- In primitive table: `"caml_ml_string_length"`
- In bytecode: `C_CALL1` instruction with primitive index

**Runtime provides** (`runtime/prims.h`):
```c
// Built-in primitive table
extern const c_primitive caml_builtin_cprim[];
extern const char * const caml_names_of_builtin_cprim[];

// Defined in generated file
const c_primitive caml_builtin_cprim[] = {
  caml_ml_string_length,
  caml_ml_array_get,
  // ... hundreds of primitives
};

const char * const caml_names_of_builtin_cprim[] = {
  "caml_ml_string_length",
  "caml_ml_array_get",
  // ...
};
```

**At runtime:**
1. Bytecode loader reads primitive table from .cmo file
2. Matches primitive names against `caml_names_of_builtin_cprim[]`
3. Creates `caml_prim_table` with function pointers
4. `C_CALL` instructions index into this table

### 3. Value Representation (Shared ABI)

Both compiler and runtime agree on value encoding:

**Compiler knows** (implicit in bytecode generation):
- Integers are tagged: `2*n + 1`
- Pointers to blocks are even (LSB = 0)
- Block headers contain: size, tag, GC color

**Runtime enforces** (`runtime/caml/mlvalues.h`):
```c
typedef intnat value;

#define Val_long(x) ((intnat)((uintnat)(x) << 1) + 1)
#define Long_val(x) ((x) >> 1)
#define Is_long(x) (((x) & 1) != 0)
#define Is_block(x) (((x) & 1) == 0)

// Block header layout (64-bit):
// [reserved:32][size:22][color:2][tag:8]
```

**Example:** Integer `42`
- Compiler emits: `CONST 42`
- Runtime represents: `42 << 1 | 1 = 85` (binary: `...0101010 1`)

### 4. Memory Management Contract

**Compiler responsibilities:**
- Generate write barriers for mutable operations
- Track local roots in C calls
- Use `CAMLparam`/`CAMLlocal`/`CAMLreturn` macros in C stubs

**Runtime responsibilities:**
- Provide fast minor heap allocation (bump pointer)
- Track young-to-old pointers (remembered set)
- Perform concurrent major GC
- Stop-the-world synchronization for phase changes

---

## Startup Sequence

When you run `./my_program.byte`:

### 1. Runtime Initialization (`startup_byt.c`)

```c
int main(int argc, char **argv) {
  // 1. Initialize runtime structures
  caml_init_domains(1, minor_heap_size);  // Single domain initially
  caml_init_domain_self(0);
  
  // 2. Open bytecode file
  int fd = caml_attempt_open(&exec_name, &trail);
  
  // 3. Read sections from bytecode
  caml_read_section_descriptors(fd, &trail);
  
  // 4. Load code section
  code = caml_stat_alloc(code_size);
  caml_seek_section(fd, &trail, "CODE");
  read(fd, code, code_size);
  
  // 5. Load data section (constants)
  caml_seek_section(fd, &trail, "DATA");
  caml_global_data = caml_input_value(fd);
  
  // 6. Load primitive table
  caml_seek_section(fd, &trail, "PRIM");
  // ... build caml_prim_table from names
  
  // 7. Initialize symbol table
  caml_init_symtable(code, code_size);
  
  // 8. Start interpreter!
  caml_interprete(code);
}
```

### 2. Interpreter Loop (`interp.c`)

```c
value caml_interprete(code_t prog) {
  register value accu;           // Accumulator register
  register value *sp;            // Stack pointer
  register code_t pc = prog;     // Program counter
  
  // Main dispatch loop
  while (1) {
    opcode_t instr = *pc++;
    
    switch (instr) {
      case MAKEBLOCK: {
        // Allocate from young generation
        int wosize = *pc++;
        int tag = *pc++;
        
        Alloc_small(accu, wosize, tag, Enter_gc);
        
        // Initialize fields from stack
        for (int i = 0; i < wosize; i++) {
          Field(accu, i) = sp[i];
        }
        sp += wosize;
        break;
      }
      
      case GETFIELD: {
        int field = *pc++;
        accu = Field(accu, field);
        break;
      }
      
      case APPLY: {
        int nargs = *pc++;
        // Push return address, set up closure environment
        // Jump to closure code...
        break;
      }
      
      case C_CALL1: {
        int prim_index = *pc++;
        Setup_for_c_call;
        accu = Primitive1(prim_index)(accu);
        Restore_after_c_call;
        break;
      }
      
      // ... 100+ more instructions
    }
  }
}
```

---

## Key Design Principles

### 1. **Separation of Concerns**
- **Compiler**: High-level transformations, optimization, code generation
- **Runtime**: Low-level execution, memory management, OS interaction

### 2. **Stable ABI**
- Value representation is fixed
- Instruction set changes rarely (backward compatible)
- Primitive interface is versioned

### 3. **Performance Critical Paths**
- **Fast allocation**: Bump pointer in minor heap (a few instructions)
- **Fast field access**: Direct pointer dereference
- **Fast function calls**: Minimal setup for known arity

### 4. **Safety Guarantees**
- **GC correctness**: All pointers tracked via root scanning
- **Memory safety**: No dangling pointers (GC handles liveness)
- **Concurrency safety**: Domain-local minor heaps, concurrent major GC

---

## Summary: How They Fit Together

```
┌──────────────────────────────────────────────────────┐
│                    OCaml Compiler                     │
│  (OCaml code - builds bytecode executables)          │
│                                                       │
│  • Parses, types, optimizes OCaml source             │
│  • Generates bytecode instructions                   │
│  • Emits .cmo/.cma files with:                       │
│    - Instruction stream                              │
│    - Symbol table                                    │
│    - Primitive references                            │
│    - Debug info                                      │
└────────────────────┬─────────────────────────────────┘
                     │
                     │ Produces
                     │
                     ▼
            ┌─────────────────┐
            │  Bytecode File   │
            │  (executable)    │
            └────────┬─────────┘
                     │
                     │ Loaded by
                     │
                     ▼
┌──────────────────────────────────────────────────────┐
│                   OCaml Runtime                       │
│  (C code - executes bytecode)                        │
│                                                       │
│  • Initializes domains, GC, allocators               │
│  • Loads bytecode sections                           │
│  • Resolves primitives → C functions                 │
│  • Runs interpreter loop                             │
│  • Manages memory (GC, domains, fibers)              │
│  • Handles signals, exceptions, effects              │
│  • Provides C primitives (I/O, arrays, etc.)         │
└──────────────────────────────────────────────────────┘
```

**Key Insight:** The compiler produces a **portable bytecode format**, and the runtime provides a **virtual machine** that executes it, with sophisticated memory management and concurrency support built-in.

---

## For RAML Implementation

When building RAML (Rust runtime for OCaml bytecode):

1. **Must match bytecode format**: Parse .cmo files exactly as OCaml does
2. **Must implement same instruction set**: All ~100 opcodes
3. **Must match value representation**: Tagged integers, block headers
4. **Must provide same primitives**: All C functions OCaml code expects
5. **Can innovate on internals**: GC algorithm, JIT compilation, optimization

The **interface** is fixed (bytecode + value repr + primitives), but the **implementation** has freedom.
