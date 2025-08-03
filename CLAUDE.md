# Learning OCaml and Riot ML Development

## Lessons Learned

- Document critical code insights and development strategies in this file to avoid repeating mistakes and capture institutional knowledge
- Recognize that the lack of comprehensive testing in critical infrastructure is a significant risk that must be addressed systematically
- Develop a test-driven development (TDD) approach that builds test coverage incrementally, starting with the most critical components
- Build test suites that validate not just correctness, but also performance, concurrent behavior, and cross-platform compatibility

## OCaml Build System Best Practices

### Dune Commands
- **Use `-p <package>` to limit builds to specific packages**: `dune build -p gluon` avoids building unrelated packages
- **Build specific targets**: `dune build gluon.install` builds just the install file
- **Run tests for specific package**: `dune test -p gluon`
- **Check build in package directory**: `cd packages/gluon && dune build`
- **Do NOT use `-p <pkg>` when not needed**: This can cause unexpected build limitations

### Common Build Issues and Solutions

1. **Type Mismatches with Interfaces**
   - When an .mli file defines types, the .ml implementation must match exactly
   - Error types in particular must be consistent (e.g., `Error `Noop` vs `Error (`System_error _)`)
   - Solution: Either update the interface or map all errors to the expected type

2. **Pattern Matching Exhaustiveness**
   - OCaml requires exhaustive pattern matching
   - When changing error types, update ALL match expressions
   - Use `| Error _ ->` as a catch-all when you don't care about specific error types

3. **Printf vs Format.printf**
   - For formatter functions (`pp`), use `Format.fprintf` not `Printf.printf`
   - `Format.printf` is for stdout, `Format.fprintf` is for formatters

4. **Unused Variables and Fields**
   - Prefix with underscore: `let _unused = ...`
   - For record fields: `field_name [@warning "-unused-field"]`
   - For mutable fields that appear unused: same attribute

5. **C FFI Compilation**
   - Include necessary headers: `#include <caml/mlvalues.h>`, etc.
   - Use `CAMLprim` for C functions exposed to OCaml
   - Handle GC roots properly with `CAMLparam` and `CAMLlocal`
   - Platform-specific code needs proper guards (e.g., `#ifdef __APPLE__`)

6. **Syntax Errors in Tests**
   - Deep nesting with multiple match expressions can be error-prone
   - Consider flattening logic or using helper functions
   - Each `begin` needs a matching `end`
   - Match expressions need proper closing

### Package Organization
- Each package should have its own dune-project file at the monorepo root
- Tests go in `test/` subdirectory with their own dune file
- Use `(package <name>)` in dune stanzas when ambiguous
- Library modules are automatically namespaced by package name