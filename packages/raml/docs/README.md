# RAML Documentation

Comprehensive documentation for the RAML OCaml compiler.

## Getting Started

**New to RAML?** Start here:
1. [Main README](../README.md) - Overview and quick start
2. [Architecture Overview](./ARCHITECTURE.md) - System design and pipeline
3. [Design Principles](./DESIGN_PRINCIPLES.md) - Why we built it this way

## Core Documentation

### Architecture & Design
- **[ARCHITECTURE.md](./ARCHITECTURE.md)** - Complete system architecture
  - Module structure
  - Compilation pipeline  
  - Type system design
  - Backend organization
  - Context threading (NO global state!)

- **[COMPILER_PASSES.md](./COMPILER_PASSES.md)** - Detailed compilation pipeline
  - Each phase explained
  - Input/output for each pass
  - Based on OCaml compiler analysis

- **[DESIGN_PRINCIPLES.md](./DESIGN_PRINCIPLES.md)** - Core principles
  - NO cryptic abbreviations
  - NO global mutable state
  - Library-first design
  - Modern infrastructure

- **[ROADMAP.md](./ROADMAP.md)** - Implementation plan
  - Completed phases
  - Current work
  - Future milestones

## Type System

Located in `typechecker/`:

- **[TYPE_SYSTEM.md](./typechecker/TYPE_SYSTEM.md)** - Type inference algorithm
  - Hindley-Milner with let-polymorphism
  - Unification algorithm
  - Occurs check
  - Generalization and instantiation

- **[REMY_ALGORITHM.md](./typechecker/REMY_ALGORITHM.md)** - Efficient generalization
  - Didier Rémy's level-based approach
  - How we implement let-polymorphism
  - Performance benefits

## Backends

Located in `backends/`:

- **[WEBASSEMBLY.md](./backends/WEBASSEMBLY.md)** - WebAssembly backend
  - Custom binary encoder (no external dependencies!)
  - LEB128 and IEEE 754 encoding
  - Memory management strategy
  - Stack-based IR
  - Examples and testing

## Reference Material

- **[how-the-ocaml-type-checker-works.html](./how-the-ocaml-type-checker-works.html)** - OCaml type checker deep dive
  - Archived for reference
  - Explains OCaml's implementation

## Documentation Structure

```
docs/
├── README.md                    # This file
├── ARCHITECTURE.md              # System architecture
├── COMPILER_PASSES.md           # Compilation pipeline
├── DESIGN_PRINCIPLES.md         # Design philosophy
├── ROADMAP.md                   # Implementation plan
│
├── typechecker/                 # Type system docs
│   ├── TYPE_SYSTEM.md          # Type inference algorithm
│   └── REMY_ALGORITHM.md       # Efficient generalization
│
└── backends/                    # Backend-specific docs
    └── WEBASSEMBLY.md          # WebAssembly backend
```

## Quick Links by Topic

### I want to understand...

**...the overall architecture**
→ [ARCHITECTURE.md](./ARCHITECTURE.md)

**...how type checking works**
→ [typechecker/TYPE_SYSTEM.md](./typechecker/TYPE_SYSTEM.md)

**...the compilation pipeline**
→ [COMPILER_PASSES.md](./COMPILER_PASSES.md)

**...why things are designed this way**
→ [DESIGN_PRINCIPLES.md](./DESIGN_PRINCIPLES.md)

**...WebAssembly code generation**
→ [backends/WEBASSEMBLY.md](./backends/WEBASSEMBLY.md)

**...what's implemented and what's next**
→ [ROADMAP.md](./ROADMAP.md)

### I want to contribute...

1. Read [Design Principles](./DESIGN_PRINCIPLES.md)
2. Check the [Roadmap](./ROADMAP.md) for open tasks
3. Study the relevant architecture docs
4. Look at existing code for patterns
5. NO CRYPTIC NAMES! Use clear, descriptive identifiers

## External Resources

### OCaml Compiler
- **Source:** https://github.com/ocaml/ocaml
- **Typing:** `ocaml/compiler/typing/` directory
- **Lambda IR:** `ocaml/compiler/lambda/` directory

### Type Theory
- **Hindley-Milner:** Damas & Milner (1982) "Principal type-schemes for functional programs"
- **Rémy's Algorithm:** Didier Rémy (1992) "Extension of ML Type System with a Sorted Equational Theory on Types"
- **MLF:** Didier Rémy (2005) "Simple, partial type-inference for System F based on type-containment"

### WebAssembly
- **Spec:** https://webassembly.github.io/spec/core/
- **Binary format:** https://webassembly.github.io/spec/core/binary/
- **Instructions:** https://webassembly.github.io/spec/core/appendix/index-instructions.html

### Compiler Design
- **Modern Compiler Implementation in ML** - Andrew Appel
- **Types and Programming Languages** - Benjamin Pierce
- **Advanced Topics in Types and Programming Languages** - Benjamin Pierce (ed.)

## Contributing to Documentation

When adding or updating documentation:

1. **Be clear** - Explain concepts simply
2. **Use examples** - Show, don't just tell
3. **Link liberally** - Connect related docs
4. **Keep updated** - Docs should match code
5. **Follow structure** - Use existing patterns

### Documentation Style

```markdown
# Module Name

## Overview
Brief description (1-2 paragraphs)

## Quick Example
```ocaml
(* Show, don't tell *)
```

## Detailed Explanation
In-depth coverage

## Implementation Details
For maintainers

## References
External links
```

### Where to Add New Docs

- **New backend?** → `backends/YOUR_BACKEND.md`
- **New compiler pass?** → Update `COMPILER_PASSES.md`
- **New algorithm?** → Relevant subdirectory
- **General architecture change?** → Update `ARCHITECTURE.md`

## Questions?

- Check existing docs first
- Look at code examples
- Read OCaml compiler source for reference
- Open an issue if something is unclear

**Remember:** Good documentation is as important as good code!
