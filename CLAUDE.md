# Learning OCaml and Riot ML Development

## System Prompt

- always explain your reasoning in steps
- if you think you're overcomplicating things, ask me
- scrutinize my inputs and corrections
- always go back to the project root after `cd`-ing somewhere
- never call `ocamlc` directly

when working on `tusk`:
- never call `tusk`, only call `./minitusk`
- NEVER MODIFY OTHER PACKAGE SOURCES TO FIX BUILD LOGIC ERRORS
- never call `tusk clean`

when workgin on any other package:
- always build through `./target/debug/tusk`
  - and if that's not available then use `./target/bootstrap/tusk`
  - and if that's not available then use `./minitusk` 
  - and if that's not available then use `./bootstarp.py`

## Lessons Learned

- ALWAYS BE ON THE PROJECT ROOT
- Document critical code insights and development strategies in this file to avoid repeating mistakes and capture institutional knowledge
- Recognize that the lack of comprehensive testing in critical infrastructure is a significant risk that must be addressed systematically
- Develop a test-driven development (TDD) approach that builds test coverage incrementally, starting with the most critical components
- Build test suites that validate not just correctness, but also performance, concurrent behavior, and cross-platform compatibility

## OCaml Best Practices

- Avoid shadowing top-level bindings locally
- Prefer verbose names
- Prefer `next_state` or `new_state` over `state'` 
- Prefer MyModule.{ record... } over { MyModule.field = ... }

## Future Refactorings

### Runtime-level integer types (i8, u8, i16, u16, etc.)
Consider implementing proper fixed-width integer types at the OCaml runtime level:
- Provide C-level support in packages/kernel for u8/i8, u16/i16, u32/i32, u64/i64
- Proper overflow semantics (wrapping vs checked)
- Type-safe operations that prevent mixing with regular ints
- Important for systems programming, binary protocols, and hash functions
- Would enable type aliases like `type u8` with actual 8-bit semantics
- For now, using int64 for hash functions and int for smaller values

### Build_node.ml spec type refactoring
Refactor the `spec` type to make the planned/unplanned distinction type-safe:

```ocaml
type planned_node = { 
  toolchain; 
  package; 
  srcs; 
  deps; 
  outs; 
  actions; 
  hash 
}
type spec = 
  | Unplanned 
  | Planned of planned_node
```

Benefits:
- Functions can take `Build_node.planned_node` instead of just `Build_node.t`
- Forces explicit handling of the unplanned case at the beginning
- Everything downstream works with `planned_node` which has all the necessary build information
- Makes invalid states unrepresentable 

## Tusk Build System Architecture

### Persistent Server Architecture
Tusk uses a persistent background server that manages all build operations, providing consistent state and intelligent caching across multiple client interfaces.

```
                    ┌→ ocamllsp ←→ tusk ocaml-merlin ←┐
                    │                                  │
Editor/AI → MCP → tusk mcp ←━━━━━━━━━━━━━━━━━━━━━━━━━┥ tusk server (persistent)
                    │                                  │
                    └→ tusk cli ←━━━━━━━━━━━━━━━━━━━━━┘
```

**Key Components:**
- **tusk server**: Persistent process managing build graph, compilation, and workspace state
- **tusk cli**: Human-friendly command interface (`tusk build`, `tusk run`, etc.)
- **tusk ocaml-merlin**: Bridge providing Merlin protocol for ocaml-lsp-server integration
- **tusk mcp**: Model Context Protocol server for AI agent integration (Claude, etc.)

**Benefits:**
- **Live Build Integration**: Auto-build on demand when LSP needs type information
- **Unified State**: Single source of truth for build configuration across all clients
- **Intelligent Caching**: Persistent server maintains build artifacts and dependency graph
- **AI-Friendly**: Direct structured access to build system for AI agents via MCP

**Implementation Notes:**
- The server auto-builds modules on demand when referenced by LSP
- No .merlin files needed - configuration served dynamically via ocaml-merlin protocol
- MCP interface provides richer operations than LSP (bulk refactors, project generation, etc.)

## Bug Fixes and Improvements (2024-08-19)

### Completed Fixes ✅
1. **Fixed BuildComplete not showing in Cargo-style output**
   - Issue: "Finished" message wasn't appearing in Cargo-style builds
   - Cause: Log.BuildComplete events weren't being forwarded to RPC clients before Rpc.BuildComplete response
   - Fix: Send Log.BuildComplete as Log.Event directly to client before sending Rpc.BuildComplete

2. **Replaced (bool * string) tuples with proper variant types**
   - Issue: Sandbox.run_actions returned (bool * string) which was unclear
   - Fix: Created proper build_result variant with Success, Failed, and Cached cases

3. **Added bold green/red colors to Cargo-style output**
   - Added ANSI color codes for better visibility
   - "Compiling" and "Finished" are bold green
   - Errors are bold red

### Pending Bug Fixes 🔧
1. **Check target folder AND build_results for cache** - rebuild if missing from target
2. **Implement crypto module with C bindings** - std-sys/crypto should use OpenSSL for SHA256/SHA512 instead of OCaml's MD5 Digest
3. **Add stdlib config option to tusk.toml** (ocaml vs std)

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
   - Initialize package structure with src/
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

## MCP Commands Roadmap for Tusk

### Essential IDE-like Commands (Priority 1)
1. **`find_definition`** - Jump to definition of a module/function/type
   - Args: `symbol_name`, `file_path`
   - Returns: File path and line number where defined

2. **`find_usages`** - Find all references to a symbol across the workspace
   - Args: `symbol_name`, `scope` (package/workspace)
   - Returns: List of locations using the symbol

3. **`add_dependency`** - Add a dependency to a package
   - Args: `package`, `dependency`, `version` (optional)
   - Updates the tusk.toml and rebuilds if needed

4. **`create_package`** - Scaffold a new package in the workspace
   - Args: `name`, `type` (library/binary), `dependencies`
   - Creates directory structure, basic files, updates workspace

5. **`explain_build_failure`** - Get detailed error analysis
   - Args: `package` (optional)
   - Returns: Parsed errors with suggestions, missing modules, type mismatches

6. **`incremental_typecheck`** - Fast type-check without full build
   - Args: `file_path` or `package`
   - Returns: Type errors only, much faster than full build

7. **`suggest_imports`** - Find and add missing module opens/imports
   - Args: `file_path`, `unbound_symbol`
   - Returns: Suggested modules to open/import with auto-fix option

8. **`refactor_rename`** - Rename symbol across entire workspace
   - Args: `old_name`, `new_name`, `kind` (function/type/module)
   - Updates all references maintaining consistency

9. **`test_runner`** - Run tests with filtering and watch mode
   - Args: `pattern`, `package`, `watch` (bool)
   - Returns: Test results with inline failure details

10. **`generate_interface`** - Auto-generate .mli from .ml file
    - Args: `ml_file_path`, `expose_all` (bool)
    - Creates interface file with inferred types, respects privacy

### Power-User Commands (Priority 2)

11. **`module_graph`** - Visualize module dependencies within a package
    - Args: `package`, `format` (text/dot/json)
    - Returns: Module dependency graph showing internal structure

12. **`benchmark_compare`** - Run and compare benchmarks between versions
    - Args: `baseline_ref`, `current_ref`, `package`
    - Returns: Performance regression/improvement analysis

13. **`dead_code_analysis`** - Find unused functions, types, and modules
    - Args: `scope` (file/package/workspace), `include_private`
    - Returns: List of potentially dead code with confidence scores

14. **`extract_module`** - Extract functions/types into a new module
    - Args: `source_file`, `symbols[]`, `target_module`
    - Refactors code into new module with proper imports

15. **`inline_module`** - Opposite of extract - inline a module's contents
    - Args: `module_path`, `target_file`
    - Merges module contents into target, updates references

16. **`type_hover`** - Get type information at specific position
    - Args: `file_path`, `line`, `column`
    - Returns: Inferred type, documentation, signature

17. **`auto_derive`** - Generate boilerplate for common patterns
    - Args: `type_name`, `derivations[]` (show/eq/ord/sexp/json)
    - Creates comparison, serialization, pretty-printing functions

18. **`upgrade_syntax`** - Modernize OCaml syntax patterns
    - Args: `package`, `patterns[]` (e.g., "fun-arrows", "let-operators")
    - Updates code to use newer OCaml syntax features

19. **`profile_build`** - Analyze build performance bottlenecks
    - Args: `package`, `detail_level`
    - Returns: Timing breakdown, slowest modules, parallelization opportunities

20. **`workspace_stats`** - Get comprehensive codebase metrics
    - Args: `include_tests`, `include_docs`
    - Returns: LOC, complexity, test coverage, doc coverage, tech debt indicators

## Enhanced MCP Command Responses (Current Commands)

### `build` - PRIORITY ENHANCEMENT
**Current:** "Build started successfully"
**Required:**
- Build duration (total and per package)
- Success/failure status
- Packages built successfully (list)
- Packages failed (list with reasons)
- Detailed failure information:
  - File path and line number
  - Error type (syntax/type/linking)
  - Error message
  - Suggested fixes
- Build statistics:
  - Total modules compiled
  - Cache hits/misses
  - Parallelism used

### `build_graph`
**Current:** Basic dependency graph
**Should add:**
- Build order sequence
- Cycle detection warnings
- Module count per package
- Estimated build time per package
- Critical path (longest dependency chain)
- Cache status per package

### `workspace_info`
**Current:** Root, toolchain, packages
**Should add:**
- Total LOC
- Last successful build timestamp
- Cache size and location
- Workspace health status
- Active configuration

### `clean`
**Current:** Basic success/failure
**Should add:**
- Space freed
- Artifacts removed count
- Impact on next build

### `run`
**Current:** "Would run binary: X"
**Should add:**
- Binary path
- Build status (needs rebuild?)
- Last built timestamp
- Available arguments

## Logging Architecture Refactoring

### Need: Structured Logging Module (`log.ml`)
**Rationale:** Current `printf`-based logging is unstructured and hard to parse for MCP

**Design:**
1. All log messages as typed ADTs
2. JSON serialization support
3. Multiple output formats (human/json/quiet)
4. Log levels (error/warn/info/debug/trace)
5. Contextual information (timestamps, package, phase)

**Benefits:**
- MCP can parse structured responses
- Consistent error formatting
- Machine-readable build logs
- Better debugging and monitoring
- Enables proper build analytics


- When writing OCaml, if I make a change in an interface file (.mli), you will follow the new interface and refactor accordingly.
