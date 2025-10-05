# Tusk Formatter Roadmap

## Current State
- Basic tokenization and token tree parsing via Syn library
- Simple token-to-string formatting
- Can handle basic `let` bindings and literals
- CLI with check mode support

## Phase 1: Core Language Constructs (Week 1-2)

### 1.1 Let Bindings & Functions
- [x] Basic `let x = value`
- [ ] `let rec` bindings
- [ ] Function definitions `let f x y = ...`
- [ ] Function application formatting
- [ ] Anonymous functions `fun x -> ...`
- [ ] Partial application spacing

### 1.2 Pattern Matching
- [ ] `match` expressions with proper indentation
- [ ] `when` guards
- [ ] Pattern formatting (tuples, lists, records)
- [ ] Or patterns `|`
- [ ] Exception patterns

### 1.3 Control Flow
- [ ] `if then else` expressions
- [ ] `try with` expressions
- [ ] `for` and `while` loops
- [ ] Sequence expressions `;`

## Phase 2: Type System (Week 2-3)

### 2.1 Type Definitions
- [ ] Type aliases `type t = string`
- [ ] Variant types with proper bar alignment
- [ ] Record types with field alignment
- [ ] Polymorphic variants
- [ ] Type parameters `'a, 'b`

### 2.2 Type Annotations
- [ ] Function type annotations
- [ ] Module signatures
- [ ] Constraint expressions

## Phase 3: Module System (Week 3-4)

### 3.1 Module Basics
- [ ] Module definitions `module M = struct ... end`
- [ ] Module signatures `module type S = sig ... end`
- [ ] Module includes `include M`
- [ ] Module opens `open M`

### 3.2 Advanced Modules
- [ ] Functors
- [ ] First-class modules
- [ ] Module constraints
- [ ] Nested modules

## Phase 4: Comments & Documentation (Week 4)

### 4.1 Comment Preservation
- [ ] Single-line comments
- [ ] Multi-line comments
- [ ] Documentation comments `(** *)`
- [ ] Inline comments
- [ ] Comment attachment heuristics

## Phase 5: Advanced Features (Week 5)

### 5.1 Objects & Classes
- [ ] Object expressions
- [ ] Class definitions
- [ ] Method calls
- [ ] Inheritance

### 5.2 Attributes & Extensions
- [ ] Attributes `[@...]`
- [ ] Extension nodes `[%...]`
- [ ] PPX annotations

## Phase 6: Configuration & Integration (Week 6)

### 6.1 Configuration System
- [ ] `.ocamlformat` compatibility mode
- [ ] Custom formatting profiles
- [ ] Per-file configuration
- [ ] Ignore directives

### 6.2 Build Integration
- [ ] Tusk build system integration
- [ ] Format-on-save support
- [ ] Incremental formatting
- [ ] Parallel file processing

## Phase 7: Testing & Validation (Ongoing)

### 7.1 Test Infrastructure
- [ ] Test harness for formatting
- [ ] Golden test files
- [ ] Property-based testing
- [ ] Regression tests

### 7.2 Real-World Testing
- [ ] Format all packages in this repo
- [ ] Compare with ocamlformat output
- [ ] Performance benchmarks
- [ ] Memory usage profiling

## Implementation Strategy

### Parsing Approach
Currently using token trees from Syn. We need to build proper AST structures on top:
1. Parse token trees into OCaml AST nodes
2. Apply formatting rules to AST
3. Pretty-print with proper indentation

### Key Data Structures Needed
```ocaml
type expr =
  | Let of { recursive: bool; bindings: binding list; body: expr }
  | Match of { expr: expr; cases: case list }
  | Apply of { fn: expr; args: expr list }
  | Lambda of { params: pattern list; body: expr }
  | Ident of string
  | Literal of literal
  | ...

type structure_item =
  | Value of binding
  | Type of type_decl
  | Module of module_decl
  | Open of string
  | ...
```

### Formatting Rules Engine
Need a configurable system for:
- Indentation (spaces vs tabs, width)
- Line breaking heuristics
- Alignment preferences
- Spacing rules

### Priority Order for Implementation

1. **Essential for dogfooding** (Week 1-2)
   - Let bindings with functions
   - Basic pattern matching
   - Type definitions
   - Module opens

2. **Common constructs** (Week 2-3)
   - If-then-else
   - Lists and arrays
   - Records
   - Comments preservation

3. **Module system** (Week 3-4)
   - Module definitions
   - Signatures
   - Includes

4. **Polish** (Week 5-6)
   - Configuration
   - Performance
   - Integration

## Success Metrics

1. Can format 80% of files in this codebase
2. Output compiles successfully
3. Formatting is idempotent
4. Performance: < 100ms for typical 500-line file
5. Memory: < 50MB for large files

## Current Blockers & Dependencies

1. Need proper AST representation (beyond token trees)
2. Need indentation/layout engine
3. Need comment attachment algorithm
4. Need configuration parser

## Next Immediate Steps

1. Build basic AST types for expressions
2. Parser from token trees to AST
3. Simple pretty-printer with indentation
4. Test on real files from codebase