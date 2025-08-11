# Learning OCaml and Riot ML Development

## Lessons Learned

- Document critical code insights and development strategies in this file to avoid repeating mistakes and capture institutional knowledge
- Recognize that the lack of comprehensive testing in critical infrastructure is a significant risk that must be addressed systematically
- Develop a test-driven development (TDD) approach that builds test coverage incrementally, starting with the most critical components
- Build test suites that validate not just correctness, but also performance, concurrent behavior, and cross-platform compatibility
- Prefer MyModule.{ record... } over { MyModule.field = ... } 

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

## Tusk Build System Development Roadmap

### Completed Features ✅
- `tusk build` - Build all packages in workspace
- `tusk build -p <package>` - Build specific package and dependencies with dependency graph filtering
- `tusk run` - Run binaries with auto-detection of multiple binaries
- `tusk run -b <binary>` - Run specific binary with auto-build if missing
- `tusk clean` - Clean build artifacts
- `tusk help` - Show help information
- Binary promotion to `target/debug/` for easy access
- Multi-worker parallel builds with proper dependency ordering
- Workspace scanning and package discovery
- Dependency graph construction and topological sorting

### Core Features Roadmap 🚧

**Week 1 (High Priority)**
1. **`tusk new [opts] path`** - Create new package at path
   - Initialize package structure with src/, dune files
   - Generate basic lib.ml/main.ml based on package type
   - Add to workspace configuration

2. **`tusk version`** - Show tusk version
   - Display version, commit hash, build date
   - Useful for debugging and support

3. **`tusk test`** - Run tests 
   - Discover and run test files/packages
   - Integration with existing test frameworks
   - Parallel test execution

**Week 2 (Package Management)**
4. **`tusk add <pkg[@vsn]>`** - Add dependency to workspace/package
   - Add to top-level workspace.toml by default
   - Use `-p <pkg>` flag to add to specific package
   - Version resolution and compatibility checking

5. **`tusk install -b <bin>`** - Build and install binary to ~/.tusk/bin
   - Global binary installation for tools
   - Similar to `cargo install`
   - PATH management integration

**Week 3 (Developer Experience)**
6. **`tusk fmt` / `tusk format`** - Code formatting
   - Integration with ocamlformat
   - Workspace-wide formatting with consistent configuration

7. **`tusk doc`** - Generate documentation
   - Integration with odoc
   - Cross-package documentation linking

8. **`tusk check`** - Type checking without building
   - Fast feedback for development
   - IDE integration support

**Week 4 (Advanced Features)**
9. **Incremental rebuilds** - Content-based hashing for smart rebuilds
   - Hash-based caching: `<hash> -> outputs`
   - Build node graph analysis for minimal rebuilds
   - Major performance improvement for large projects

### Advanced Features (Future)

**Automatic OCaml Improvements**
- **Folder-based namespacing**: `pkg/src/a/b/c.ml` → `Pkg__A__B__C.ml`
- **Auto CamelCase conversion**: `hello_world.ml` → `HelloWorld` module
- **Smart dependency inference**: Analyze imports to suggest missing dependencies

**Additional Commands**
- `tusk publish` - Publish packages to registry
- `tusk update` - Update dependencies to latest compatible versions  
- `tusk tree` - Show dependency tree visualization
- `tusk clean --all` - Clean all artifacts including dependencies
- `tusk bench` - Run benchmarks
- `tusk watch` - Watch files and auto-rebuild
- `tusk shell` - Open shell with package environment

### Implementation Notes
- Maintain cargo-like UX for familiar developer experience
- Focus on OCaml-specific intelligence and optimizations
- Keep the actor-based parallel build system for performance
- Preserve existing dependency graph filtering for `-p` builds