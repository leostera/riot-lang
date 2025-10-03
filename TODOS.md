# Tusk & Riot Development Todos

> This file consolidates all todos from various documentation files into a single source of truth.
> These will be converted into Linear issues with appropriate tags and metadata.
> 
> **Last Updated**: 2025-10-03
> **Status**: Active tracking - regularly review completed items and move to archive

## Legend

- **Priority**: `P0` (Critical), `P1` (High), `P2` (Medium), `P3` (Low)
- **Size**: `XS` (< 1 day), `S` (1-3 days), `M` (3-7 days), `L` (1-2 weeks), `XL` (2+ weeks)
- **Tags**: `#mcp`, `#test`, `#package`, `#format`, `#std`, `#plugin`, `#cross-compile`, `#bugfix`, `#feature`, `#infra`, `#docs`, `#cli`, `#argparse`
- **Status**: Items marked with ✅ are implemented and should be moved to archive

---

## Quick Status Overview

### ✅ Recently Completed (Move to Archive)
- Std.Fs - Non-blocking filesystem operations
- Std.Path - Path manipulation
- Std.Crypto.Hash - Cryptographic hashing
- Std.Json - JSON parsing/generation
- Std.Toml - TOML parsing
- Std.Net.Http.* - Complete RFC 7231-compliant HTTP/1.1 types
- Std.Command - System command execution
- Std.Collections.* - Full suite of data structures
- Std.Time.* - Time types (Duration, Instant, SystemTime)
- Std.WorkerPool - Parallel task execution
- Std.ArgParser - Declarative CLI argument parsing with Clap-style API
- `tusk build` - Package building with dependency graph
- `tusk clean` - Build artifact cleanup
- `tusk new` - Package scaffolding
- `tusk install` - Binary installation
- `tusk run` - Binary execution
- `tusk version` - Version display (basic)
- `tusk rpc *` - Complete RPC command suite

### 🚧 High Priority Next Steps
1. **Std.Test** - Test runtime module
2. **tusk test** - Test framework with convention-based discovery
3. **tusk fmt** - Code formatting (mentioned in help but missing)
4. **tusk doc** - Documentation generation (mentioned in help but missing)
5. **Std.Http.Client** - HTTP client for network requests

### 📦 Major Systems Not Yet Started
- Package Management (PubGrub resolver, registry, publish/install)
- Plugin System (workspace commands, dependency plugins)
- Cross-Compilation (multi-target builds)
- Format System (beyond basic fmt command)
- Advanced MCP Tools (most IDE-like features)

---

## Critical Bugs

### [P0] Fix BuildComplete not being sent after cache hit
- **Size**: XS
- **Tags**: #bugfix #server
- **Description**: When all packages are cached, BuildComplete message is never sent to client
- **Location**: `packages/tusk/src/server/tusk_server.ml` around line 900
- **Fix**: Send `Rpc.BuildComplete` to client after `check_build_complete` returns true
- **Issue**: Build hangs when minitusk is cached on third build

### [P1] Fix rebuild trigger when missing from target folder
- **Size**: S
- **Tags**: #bugfix #cache
- **Description**: Check both build_results cache AND target folder - rebuild if missing from target
- **Impact**: Prevents "cached but not promoted" bugs

---

## Phase 1: Core MCP Tools (Priority 1)

### Type & Error Analysis

#### [P1] Implement `typecheck` MCP tool
- **Size**: M
- **Tags**: #mcp #feature #type-analysis
- **Description**: Fast incremental type checking without full build
- **Benefits**: Critical for fast feedback during development
- **API**: `typecheck { file_path?, package? }`
- **Dependencies**: LSP integration

#### [P1] Implement `explain_error` MCP tool
- **Size**: M
- **Tags**: #mcp #feature #type-analysis
- **Description**: Deep dive into compilation errors with suggestions
- **Features**:
  - Detailed type mismatch explanations
  - Suggest fixes based on common patterns
  - Show type inference chain
- **API**: `explain_error { error_id, context }`

#### [P2] Implement `infer_type` MCP tool
- **Size**: S
- **Tags**: #mcp #feature #type-analysis
- **Description**: Get inferred type at any position in code
- **Features**:
  - Show type of expression at cursor
  - Display module signatures
  - Show variant/record field types
- **API**: `infer_type { file_path, line, column }`

### Navigation & Search

#### [P1] Implement `find_definition` MCP tool
- **Size**: M
- **Tags**: #mcp #feature #navigation
- **Description**: Jump to definition of modules/functions/types
- **Features**:
  - Handle local and external modules
  - Support ppx-generated code
  - Navigate through functors
- **API**: `find_definition { symbol, file_path }`

#### [P1] Implement `find_references` MCP tool
- **Size**: M
- **Tags**: #mcp #feature #navigation
- **Description**: Find all usages of a symbol
- **Features**:
  - Scope to file/package/workspace
  - Include type occurrences
  - Show context around usage
- **API**: `find_references { symbol, scope }`

#### [P2] Implement `find_implementations` MCP tool
- **Size**: M
- **Tags**: #mcp #feature #navigation
- **Description**: Find module implementations for interfaces
- **API**: `find_implementations { module_type }`

### Code Generation & Scaffolding

#### [P1] Implement `generate_interface` MCP tool
- **Size**: M
- **Tags**: #mcp #feature #codegen
- **Description**: Auto-generate .mli from .ml file
- **Features**:
  - Infer minimal public interface
  - Option to expose all or selective exports
  - Preserve documentation comments
- **API**: `generate_interface { ml_file, expose_all? }`

#### [P2] Implement `scaffold_module` MCP tool
- **Size**: S
- **Tags**: #mcp #feature #codegen
- **Description**: Create new module with boilerplate
- **Features**:
  - Generate matching .ml/.mli pair
  - Add common patterns (functors, module types)
  - Auto-add to build configuration
- **API**: `scaffold_module { name, type, package }`

#### [P3] Implement `derive` MCP tool
- **Size**: L
- **Tags**: #mcp #feature #codegen
- **Description**: Generate boilerplate for types
- **Features**:
  - Equality, comparison, show, serialization
  - Custom derivers for project patterns
  - Update when type changes
- **API**: `derive { type_name, derivations[] }`

### Refactoring Tools

#### [P1] Implement `rename_symbol` MCP tool
- **Size**: L
- **Tags**: #mcp #feature #refactor
- **Description**: Rename across entire codebase
- **Features**:
  - Handle modules, types, values, fields
  - Update all references including .mli files
  - Preserve formatting and comments
- **API**: `rename_symbol { old_name, new_name, kind, scope }`

#### [P2] Implement `extract_function` MCP tool
- **Size**: M
- **Tags**: #mcp #feature #refactor
- **Description**: Extract code into new function
- **Features**:
  - Infer parameters and return type
  - Handle closures and free variables
  - Update call sites
- **API**: `extract_function { file_path, start_line, end_line, new_name }`

#### [P2] Implement `inline_function` MCP tool
- **Size**: M
- **Tags**: #mcp #feature #refactor
- **Description**: Inline function at call sites
- **API**: `inline_function { function_name, location? }`

#### [P2] Implement `change_signature` MCP tool
- **Size**: L
- **Tags**: #mcp #feature #refactor
- **Description**: Modify function signatures and update call sites
- **API**: `change_signature { function_name, new_params }`

### Testing & Quality

#### [P2] Implement `run_tests` MCP tool
- **Size**: M
- **Tags**: #mcp #feature #test
- **Description**: Smart test execution with watch mode
- **Features**:
  - Run tests for changed modules only
  - Support watch mode
  - Filter by test name patterns
- **API**: `run_tests { pattern?, package?, watch? }`

#### [P2] Implement `coverage_report` MCP tool
- **Size**: L
- **Tags**: #mcp #feature #test
- **Description**: Code coverage analysis
- **API**: `coverage_report { package? }`

#### [P3] Implement `suggest_tests` MCP tool
- **Size**: L
- **Tags**: #mcp #feature #test
- **Description**: Generate test cases based on function signatures
- **API**: `suggest_tests { module_name }`

### Performance & Optimization

#### [P2] Implement `profile_build` MCP tool
- **Size**: M
- **Tags**: #mcp #feature #performance
- **Description**: Build performance analysis
- **Features**:
  - Identify slow modules
  - Show parallelization opportunities
  - Cache hit rates
- **API**: `profile_build { package? }`

#### [P2] Implement `optimize_imports` MCP tool
- **Size**: M
- **Tags**: #mcp #feature #refactor
- **Description**: Clean up module opens/includes
- **API**: `optimize_imports { file_path }`

#### [P2] Implement `dead_code_analysis` MCP tool
- **Size**: L
- **Tags**: #mcp #feature #analysis
- **Description**: Find unused code
- **Features**:
  - Unused functions, types, modules
  - Confidence scores
  - Safe removal suggestions
- **API**: `dead_code_analysis { scope, include_private? }`

---

## Phase 2: Testing Infrastructure

### Design Decision: Convention-Based Tests

**Approach**: Start with `tests/` folder convention, later add inline `[@test]` support

**Test Location**: `packages/<name>/tests/*.ml`
- Each test file becomes a separate test binary
- Tests use the public API of their parent package
- Test files are NOT included in library builds
- Clear separation between library code and tests

**Test Discovery**: Convention-based with `test_*` function patterns
- Files in `tests/` folder
- Simple regex to find `let test_* () = ...` patterns
- Generate test runner that calls all discovered test functions

### Phase 2a: Std.Test Module

#### [P1] Implement Std.Test - Standard test runtime
- **Size**: S
- **Tags**: #std #test #feature
- **Module**: `packages/std/src/test.ml`
- **Description**: Standard test runtime for all Tusk tests
- **API Design**:
```ocaml
module Test : sig
  type result = Pass | Fail of string | Error of exn
  type test_case = { name : string; fn : unit -> unit }
  
  val case : string -> (unit -> unit) -> test_case
  val run : test_case list -> unit  (* exits 0 or 1 *)
  
  (* Assert helpers *)
  val assert_equal : expected:'a -> actual:'a -> unit
  val assert_ok : ('a, 'b) result -> unit
  val assert_error : ('a, 'b) result -> unit
  val assert_true : bool -> unit
  val assert_false : bool -> unit
end
```
- **Features**:
  - Clean pass/fail reporting with ✓/✗ symbols
  - Captures exceptions as test errors
  - Summary with passed/failed/total counts
  - Exits with code 1 if any tests fail
  - Type-safe assert helpers for common patterns

### Phase 2b: Core Test Framework

#### [P1] Implement test discovery in Module_graph
- **Size**: M
- **Tags**: #test #feature
- **Description**: Scan `tests/` folders and find test functions
- **Module**: `packages/tusk/src/core/module_graph.ml`
- **Features**:
  - Scan package `tests/` directories for `*.ml` files
  - Exclude test files from library/executable builds
  - Parse test files with regex to find `let test_* () = ...` functions
  - Build mapping: test_file -> [test_function_names]
  - Skip test files when creating alias modules and library interfaces

#### [P1] Implement test runner generation
- **Size**: M
- **Tags**: #test #feature
- **Description**: Generate runner code for each test file
- **Module**: `packages/tusk/src/test/test_generator.ml` (new)
- **Features**:
  - For each `tests/foo_test.ml`, generate `tests/.tusk/foo_test_runner.ml`
  - Generated code:
    ```ocaml
    open Std
    open Foo_test
    
    let () = 
      Test.run [
        Test.case "test_addition" test_addition;
        Test.case "test_subtraction" test_subtraction;
      ]
    ```
  - Inject discovered test function names into runner
  - Handle module namespacing correctly

#### [P1] Implement test build nodes
- **Size**: S
- **Tags**: #test #feature
- **Description**: Create executable build nodes for test runners
- **Module**: `packages/tusk/src/core/build_graph.ml`
- **Features**:
  - Each test runner becomes a binary (e.g., `foo_test`)
  - Link against parent package library
  - Place binaries in `target/debug/tests/` or `target/release/tests/`
  - Test executables depend on parent package being built first

#### [P1] Implement test execution
- **Size**: M
- **Tags**: #test #feature
- **Description**: Run test executables and collect results
- **Module**: `packages/tusk/src/test/test_executor.ml` (new)
- **Features**:
  - Execute each test binary in parallel
  - Capture stdout/stderr
  - Collect exit codes (0 = success, 1 = failure)
  - Aggregate results across all test binaries
  - Pretty-print summary

#### [P1] Add `tusk test` CLI command
- **Size**: S
- **Tags**: #test #feature #cli
- **Description**: Add test subcommand to CLI
- **Module**: `packages/tusk/src/cli/cli.ml`
- **Features**:
  - `tusk test` - run all tests in workspace
  - `tusk test -p <package>` - run tests for specific package
  - Parse test-specific options
  - Invoke test discovery -> generation -> build -> execution pipeline
  - Return appropriate exit code for CI

### Advanced Test Features

#### [P2] Implement parallel test execution
- **Size**: M
- **Tags**: #test #feature
- **Description**: Run tests concurrently across packages

#### [P2] Add test filtering by name pattern
- **Size**: S
- **Tags**: #test #feature
- **Description**: `tusk test --filter "test_addition*"`

#### [P2] Add timing information to test output
- **Size**: XS
- **Tags**: #test #feature
- **Description**: Show test execution times

#### [P2] Better error reporting with source locations
- **Size**: M
- **Tags**: #test #feature
- **Description**: Show file:line for test failures

#### [P3] Add setup/teardown hooks
- **Size**: M
- **Tags**: #test #feature
- **Description**: Module-level test setup/teardown

#### [P3] Property-based testing support
- **Size**: L
- **Tags**: #test #feature
- **Description**: Integration with property-based testing

#### [P3] Coverage reporting
- **Size**: L
- **Tags**: #test #feature
- **Description**: Generate coverage reports

#### [P3] Watch mode for continuous testing
- **Size**: M
- **Tags**: #test #feature
- **Description**: Auto-run tests on file changes

### Alternative Approach: Inline Tests (Future)

**For reference, the inline `[@test]` approach from TUSK_TEST.md:**

- Test signature: `unit -> (unit, string) result`
- Tests return `Ok ()` for pass, `Error msg` for fail
- Discovery via regex: `\[@test\]\s+let\s+(\w+)`
- Generated files use `include struct ... end` to bring in original code
- `Tusk_test.run_module` embedded runtime for test execution
- Process isolation per test module

This approach may be implemented later after convention-based tests are stable.

---

## Phase 3: Package Management System

### Core Principles
1. **Workspace-unified versions**: One version per dependency across entire workspace
2. **Registry-first**: All packages go through Tusk package registry
3. **Source distribution**: Packages distributed as source tarballs
4. **No OPAM compatibility**: Clean break for simplicity

### Core Package Management

#### [P1] Implement PubGrub resolver
- **Size**: XL
- **Tags**: #package #feature #resolver
- **Description**: Dependency resolution using PubGrub algorithm for sound, complete resolution
- **Module**: `packages/tusk/src/resolver.ml`
- **Algorithm**:
  ```ocaml
  module Resolution : sig
    type incompatibility = {
      package : string;
      constraint1 : Constraint.t;
      constraint2 : Constraint.t;
      source1 : string;
      source2 : string;
    }
    type result = 
      | Success of (string * Version.t) list
      | Conflict of incompatibility
  end
  ```
- **Features**:
  - Sound, complete dependency resolution
  - Clear conflict error messages with sources
  - Version constraint handling
  - Workspace-wide unified resolution

#### [P1] Implement local package cache
- **Size**: M
- **Tags**: #package #feature #cache
- **Description**: Cache downloaded packages locally
- **Location**: `~/.tusk/cache/`
- **Structure**:
  ```
  ~/.tusk/
  ├── cache/
  │   ├── riot-2.1.0/
  │   │   ├── tusk.toml
  │   │   └── src/
  │   └── gluon-1.0.0/
  ├── registry.json
  └── checksums.json
  ```
- **Features**:
  - Store in `~/.tusk/cache/<pkg>-<version>/`
  - Cache registry metadata
  - Track checksums for verification

#### [P1] Implement `tusk add` command
- **Size**: M
- **Tags**: #package #feature #cli
- **Description**: Add dependency to workspace/package
- **Usage**:
  - `tusk add riot` - add to workspace
  - `tusk add riot@2.0.0` - specific version
  - `tusk add -p mypackage riot` - add to package
- **Implementation**:
  - Update tusk.toml with new dependency
  - Trigger dependency resolution
  - Download if not cached
  - Update lock file

#### [P1] Implement `tusk rm` command
- **Size**: S
- **Tags**: #package #feature #cli
- **Description**: Remove dependency from workspace/package
- **Features**:
  - Remove from tusk.toml
  - Re-resolve dependencies
  - Optionally clean cache

#### [P1] Implement lock file generation
- **Size**: M
- **Tags**: #package #feature
- **Description**: Generate tusk.lock for reproducible builds
- **Format**: TOML with package versions and checksums
- **Structure**:
  ```toml
  [[package]]
  name = "riot"
  version = "2.1.0"
  checksum = "sha256:abc123..."
  dependencies = ["gluon@1.0.0", "miniriot@0.1.0"]
  ```

### Package Registry

#### [P1] Implement HTTP API server
- **Size**: L
- **Tags**: #package #feature #registry
- **Description**: Package registry service
- **Module**: `packages/package-registry/src/main.ml`
- **Endpoints**:
  - `GET /api/v1/packages` - list packages
  - `GET /api/v1/packages/:name` - get package metadata
  - `GET /api/v1/packages/:name/:version` - get specific version
  - `GET /api/v1/packages/:name/:version/tarball` - download
  - `POST /api/v1/packages/publish` - publish package
  - `GET /api/v1/search?q=:query` - search packages
- **Response Format**:
  ```json
  {
    "name": "riot",
    "versions": ["1.0.0", "2.0.0", "2.1.0"],
    "latest": "2.1.0",
    "description": "Actor-model concurrency",
    "homepage": "https://github.com/riot-ml/riot"
  }
  ```

#### [P1] Implement package storage backend
- **Size**: M
- **Tags**: #package #feature #registry
- **Description**: Filesystem-based package storage for MVP
- **Structure**:
  ```
  registry-data/
  ├── packages/
  │   ├── riot/
  │   │   ├── metadata.json
  │   │   └── versions/
  │   │       ├── 2.0.0/
  │   │       │   ├── manifest.json
  │   │       │   └── tarball.tar.gz
  │   │       └── 2.1.0/
  │   └── gluon/
  └── index.json
  ```

#### [P1] Implement `tusk publish` command
- **Size**: M
- **Tags**: #package #feature #cli
- **Description**: Publish packages to registry
- **Process**:
  1. Load and validate tusk.toml
  2. Check version doesn't exist
  3. Create source tarball
  4. Calculate checksum
  5. Upload to registry
- **Features**:
  - Version validation
  - Tarball creation
  - Checksum generation (SHA256)
  - Upload with authentication

#### [P1] Implement package downloading
- **Size**: M
- **Tags**: #package #feature
- **Description**: Download and verify packages from registry
- **Process**:
  1. Query registry for package version
  2. Download tarball
  3. Verify checksum
  4. Extract to cache
  5. Update local metadata

#### [P1] Implement checksum verification
- **Size**: S
- **Tags**: #package #feature #security
- **Description**: Verify downloaded packages against checksums
- **Algorithm**: SHA256
- **Failure**: Abort install on mismatch

### Advanced Package Features

#### [P2] Version ranges and constraints
- **Size**: M
- **Tags**: #package #feature
- **Description**: Support `>=1.0.0, <2.0.0`, `~1.5.0` version specs
- **Syntax**:
  - `"1.0.0"` - exact version
  - `">=1.0.0, <2.0.0"` - range
  - `"~1.5.0"` - compatible (>= 1.5.0, < 1.6.0)
  - `"*"` - latest

#### [P2] Authentication and authorization
- **Size**: L
- **Tags**: #package #feature #security
- **Description**: Token-based auth for publishing
- **Implementation**:
  ```ocaml
  type auth_token = {
    token : string;
    user : string;
    expires : float;
    scopes : string list;
  }
  ```

#### [P2] Package search
- **Size**: M
- **Tags**: #package #feature
- **Description**: Search packages by name/description

#### [P2] Private registries
- **Size**: L
- **Tags**: #package #feature
- **Description**: Support for private package registries
- **Configuration**:
  ```toml
  [registry]
  url = "https://packages.mycompany.com"
  ```

#### [P2] Git dependencies
- **Size**: L
- **Tags**: #package #feature
- **Description**: Support dependencies from git repos
- **Syntax**:
  ```toml
  [dependencies]
  experimental = { git = "https://github.com/user/repo", branch = "main" }
  ```

#### [P3] Web UI for registry
- **Size**: XL
- **Tags**: #package #feature #ui
- **Description**: Browse and search packages via web interface

#### [P3] Package documentation hosting
- **Size**: L
- **Tags**: #package #feature #docs
- **Description**: Auto-generate and host package docs

#### [P3] Download statistics
- **Size**: M
- **Tags**: #package #feature
- **Description**: Track package download counts

#### [P3] Security advisories
- **Size**: L
- **Tags**: #package #feature #security
- **Description**: Security vulnerability tracking

### Error Messages

**Dependency Conflicts**:
```
Error: Dependency conflict detected

Package 'myapp' requires riot@2.0.0
Package 'mylib' requires riot@3.0.0

These constraints are incompatible. Consider:
- Updating myapp to support riot@3.0.0
- Downgrading mylib to use riot@2.0.0
```

**Missing Dependencies**:
```
Error: Package 'riot' not found in registry

Did you mean one of these?
- riot-core
- riot-testing
```

---

## Phase 4: Format System

### Core Components

#### [P1] Implement Format_manager
- **Size**: M
- **Tags**: #format #feature
- **Description**: Central coordinator for formatting operations
- **Module**: `packages/tusk/src/format/format_manager.ml`
- **Features**:
  - Format cache management
  - Worker pool management
  - Error collection and aggregation
  - Unified API for CLI/RPC/MCP

#### [P1] Implement Format_worker
- **Size**: M
- **Tags**: #format #feature
- **Description**: Individual worker with pluggable backends
- **Module**: `packages/tusk/src/format/format_worker.ml`
- **Backends**:
  - Ocamlformat_binary - Call ocamlformat binary
  - Ocamlformat_rpc - Use RPC for better performance
  - Tusk_formatter - Future zero-config formatter

#### [P1] Implement Format_cache
- **Size**: S
- **Tags**: #format #feature #cache
- **Description**: Disk-based cache for formatted files
- **Module**: `packages/tusk/src/format/format_cache.ml`
- **Location**: `./target/<profile>/fmt/`
- **Strategy**:
  1. Compute SHA256 of file content
  2. Check if `./target/<profile>/fmt/<hash>` exists
  3. If exists, file is already formatted (skip)
  4. If not, format the file
  5. After format, compute new hash
  6. If hash unchanged, create marker file
- **Benefits**:
  - Zero overhead (just file existence check)
  - Persists across server restarts
  - Self-validating (content hash)

#### [P1] Add `tusk fmt` CLI support
- **Size**: S
- **Tags**: #format #feature #cli
- **Description**: Format command line interface
- **Usage**:
  - `tusk fmt` - format workspace
  - `tusk fmt -p mypackage` - format package
  - `tusk fmt src/main.ml` - format specific files
  - `tusk fmt --check` - check formatting
  - `tusk fmt --diff` - show diffs
  - `tusk fmt --jobs 8` - parallel workers

#### [P1] Add RPC interface for formatting
- **Size**: M
- **Tags**: #format #feature #rpc
- **Description**: Structured format requests/responses via RPC
- **Request**:
  ```json
  {
    "method": "format",
    "params": {
      "paths": ["src/main.ml"],
      "options": { "check": false, "diff": false, "jobs": 4 }
    }
  }
  ```
- **Response**:
  ```json
  {
    "results": [
      { "path": "src/main.ml", "status": "success", "time_ms": 45.2 }
    ],
    "summary": {
      "total": 1,
      "successful": 1,
      "failed": 0,
      "skipped": 0,
      "duration_ms": 45.2
    }
  }
  ```

#### [P2] Add MCP tools for formatting
- **Size**: S
- **Tags**: #format #feature #mcp
- **Description**: `formatFile`, `formatPackage`, `checkFormatting` tools

### Advanced Formatting Features

#### [P2] Implement concurrent formatting with worker pool
- **Size**: M
- **Tags**: #format #feature #performance
- **Description**: Parallel formatting across multiple files
- **Implementation**:
  - Worker pool of size N (default: CPU count)
  - Queue of files to format
  - Workers process files concurrently
  - Results collected as workers complete

#### [P2] Implement ocamlformat-rpc backend
- **Size**: M
- **Tags**: #format #feature
- **Description**: Use RPC for better performance
- **Benefits**:
  - Persistent process (no startup overhead)
  - Connection pooling
  - Amortize initialization cost

#### [P2] Implement incremental formatting
- **Size**: S
- **Tags**: #format #feature
- **Description**: Only format changed files
- **Integration**: Use git to find modified files

#### [P3] Diff generation
- **Size**: S
- **Tags**: #format #feature
- **Description**: Show unified diffs of formatting changes

#### [P3] Partial formatting
- **Size**: M
- **Tags**: #format #feature
- **Description**: Format only specific regions of files

#### [P3] Format on save (IDE integration)
- **Size**: M
- **Tags**: #format #feature #ide
- **Description**: Auto-format on file save

#### [P3] Pre-commit hooks for formatting
- **Size**: S
- **Tags**: #format #feature #git
- **Description**: Git hooks for format checking

### Future: Tusk Formatter

#### [P3] Design Tusk formatter (zero config)
- **Size**: XL
- **Tags**: #format #feature #formatter
- **Description**: Custom formatter with no configuration
- **Philosophy**:
  - Zero configuration (like gofmt)
  - Fast (10x faster than ocamlformat)
  - Deterministic output
  - Opinionated style
  - No .ocamlformat files
- **Target Performance**: 10x faster than ocamlformat
- **Implementation**: Native OCaml, hardcoded style
- **Migration**: Opt-in initially, default later

---

## Phase 5: Standard Library Development

### ✅ Completed Standard Library Modules

The following modules are already implemented:
- ✅ **Std.Fs** - Non-blocking filesystem operations
- ✅ **Std.Path** - Path manipulation and normalization
- ✅ **Std.Crypto.Hash** - Cryptographic hashing (SHA256, SHA512, etc.)
- ✅ **Std.Json** - JSON parsing and serialization
- ✅ **Std.Toml** - TOML parsing
- ✅ **Std.Net.Uri** - URI parsing and manipulation
- ✅ **Std.Net.Http.*** - Complete HTTP/1.1 types (RFC 7231 compliant)
- ✅ **Std.Command** - System command execution
- ✅ **Std.Env** - Environment variable access
- ✅ **Std.Time.*** - Time types (Duration, Instant, SystemTime)
- ✅ **Std.Collections.*** - Data structures (Vector, HashMap, HashSet, Queue, Deque)
- ✅ **Std.Graph.*** - Graph utilities (Dot, Mermaid)
- ✅ **Std.Log** - Structured logging
- ✅ **Std.DateTime** - Date and time handling
- ✅ **Std.WorkerPool** - Parallel task execution
- ✅ **Std.ArgParser** - Declarative CLI argument parsing

### CLI & Application Support

#### [P1] Std.Test - Test runtime (see Phase 2a above)
- Already documented in Phase 2

### Network & HTTP

#### [P1] Implement Std.Http.Client - HTTP client
- **Size**: L
- **Tags**: #std #feature #network
- **Module**: `packages/std/src/http/client.ml`
- **Built on**: Gluon's non-blocking TCP
- **Features**:
  - GET, POST, PUT, DELETE
  - JSON helpers (get_json, post_json)
  - Connection pooling per actor
  - Streaming responses
  - File downloads with progress
  - Non-blocking, yields to scheduler
- **API**:
  ```ocaml
  val get : string -> (response, error) result
  val post : string -> body:string -> (response, error) result
  val get_json : string -> (Json.t, error) result
  val download : string -> dest:string -> (unit, error) result
  ```

#### [P2] Implement Std.Net - Core networking
- **Size**: L
- **Tags**: #std #feature #network
- **Module**: `packages/std/src/net.ml`
- **Features**:
  - Socket operations (TCP/UDP)
  - TCP streams with non-blocking I/O
  - UDP sockets
  - Built on Gluon

#### [P2] Implement Std.Http.Server - HTTP server
- **Size**: XL
- **Tags**: #std #feature #network
- **Module**: `packages/std/src/http/server.ml`
- **Features**:
  - Request handling with routes
  - Middleware support (CORS, logging, gzip, rate limiting)
  - WebSocket support
  - Built on actor model

### Data Formats

#### [P2] Implement Std.Xml - XML parsing
- **Size**: M
- **Tags**: #std #feature #data
- **Module**: `packages/std/src/xml.ml`
- **Features**:
  - DOM parsing
  - XPath support (basic)
  - XML generation
  - Pretty printing

#### [P2] Implement Std.Csv - CSV handling
- **Size**: S
- **Tags**: #std #feature #data
- **Module**: `packages/std/src/csv.ml`
- **Features**:
  - CSV parsing and writing
  - Custom delimiters
  - Header handling
  - Quote escaping

### Actor System Extensions

#### [P2] Implement Std.Supervisor - Dynamic supervision
- **Size**: L
- **Tags**: #std #feature #actor
- **Module**: `packages/std/src/supervisor.ml`
- **Features**:
  - One-for-one, one-for-all, rest-for-one strategies
  - Dynamic child management (add/remove/restart)
  - Restart policies (permanent/temporary/transient)
  - Shutdown timeouts
- **API**:
  ```ocaml
  type strategy = [`One_for_one | `One_for_all | `Rest_for_one]
  type restart = [`Permanent | `Temporary | `Transient]
  type child_spec = {
    id : string;
    start : unit -> Process.t;
    restart : restart;
    shutdown : [`Timeout of float | `Brutal_kill];
  }
  val start : strategy -> child_spec list -> (t, error) result
  val add_child : t -> child_spec -> (Process.t, error) result
  ```

#### [P2] Implement Std.Agent - Stateful actors
- **Size**: M
- **Tags**: #std #feature #actor
- **Module**: `packages/std/src/agent.ml`
- **Features**:
  - Get, update, cast, call operations
  - Simple state management
  - Synchronous and asynchronous updates
- **API**:
  ```ocaml
  val start : 'a -> ('a t, error) result
  val get : 'a t -> ('a -> 'b) -> 'b
  val update : 'a t -> ('a -> 'a) -> unit
  val cast : 'a t -> ('a -> 'a) -> unit  (* async *)
  val call : 'a t -> ('a -> 'a * 'b) -> 'b  (* sync with return *)
  ```

#### [P2] Implement Std.Registry - Process discovery
- **Size**: M
- **Tags**: #std #feature #actor
- **Module**: `packages/std/src/registry.ml`
- **Features**:
  - Named process registration
  - Global and local registries
  - Lookup by name
  - List all registered processes

### Application Support

#### [P2] Implement Std.Config - Configuration management
- **Size**: M
- **Tags**: #std #feature #app
- **Module**: `packages/std/src/config.ml`
- **Features**:
  - Load from files (TOML, JSON)
  - Environment variable fallback
  - Nested configuration access
  - Type-safe extraction (get_string, get_int, etc.)
- **API**:
  ```ocaml
  val load : string -> (t, error) result
  val from_env : unit -> t
  val merge : t -> t -> t
  val get_string : t -> key:string -> default:string -> string
  val get_nested : t -> path:string list -> string option
  ```

#### [P3] Implement Std.Sql - Database interface
- **Size**: XL
- **Tags**: #std #feature #app
- **Module**: `packages/std/src/sql.ml`
- **Features**:
  - Connection management
  - Query execution
  - Transactions
  - Prepared statements
- **Backends**: SQLite, PostgreSQL, MySQL

### Cryptography Extensions

#### [P2] Implement Std.Crypto.Cipher - Symmetric encryption
- **Size**: M
- **Tags**: #std #feature #crypto
- **Module**: `packages/std/src/crypto/cipher.ml`
- **Algorithms**: AES, ChaCha20
- **Bindings**: Use OpenSSL or libsodium

#### [P2] Implement Std.Crypto.Signature - Digital signatures
- **Size**: M
- **Tags**: #std #feature #crypto
- **Module**: `packages/std/src/crypto/signature.ml`
- **Algorithms**: RSA, Ed25519, ECDSA
- **Features**: Key generation, sign, verify

#### [P3] Implement Std.Crypto.Random - Secure random
- **Size**: S
- **Tags**: #std #feature #crypto
- **Module**: `packages/std/src/crypto/random.ml`
- **Features**: Random bytes, ints, strings, UUIDs

### Compression

#### [P2] Implement Std.Archive - Archive handling
- **Size**: L
- **Tags**: #std #feature
- **Module**: `packages/std/src/archive.ml`
- **Features**:
  - Tar creation and extraction
  - Gzip compression/decompression
  - Streaming APIs for large files
- **Use Case**: Replace all `tar` and `gzip` command calls

---

## Phase 6: Plugin System

### Philosophy
1. **Zero Installation Friction**: Commands from dependencies work after `tusk build`
2. **Type-Safe Integration**: All plugins use OCaml, not shell scripts
3. **RPC-First**: Full access to Tusk RPC protocol
4. **Workspace-Aware**: Understand project structure
5. **Composable**: Commands can call other commands

### Use Cases
- Web frameworks: `tusk dream scaffold --api users`
- Testing libraries: `tusk alcotest run --watch`
- Database tools: `tusk db migrate --version 42`
- Deployment: `tusk deploy staging --env production`

### Core Plugin Infrastructure

#### [P2] Extract packages/tusk-rpc library
- **Size**: M
- **Tags**: #plugin #feature #infra
- **Description**: Separate RPC library for plugins to use
- **Module**: `packages/tusk-rpc/`
- **Exports**: RPC client for build operations, workspace queries, file operations

#### [P2] Create packages/tusk-lib with command interface
- **Size**: M
- **Tags**: #plugin #feature #infra
- **Description**: Command interface for plugins
- **Module**: `packages/tusk-lib/command.mli`
- **Interface**:
  ```ocaml
  module type S = sig
    val name : string
    val description : string
    val arguments : Arg.spec list
    val run : Workspace.t -> Args.t -> (int, string) result
  end
  ```

#### [P2] Implement plugin discovery and registration
- **Size**: M
- **Tags**: #plugin #feature
- **Description**: Scan workspace and dependencies for commands
- **Process**:
  1. Scan workspace.toml for `[extensions]`
  2. Scan dependencies for `[[command]]` in package.toml
  3. Build plugin modules as .cmxs
  4. Register in command registry
  5. Load dynamically when invoked

### Workspace Commands

#### [P2] Support workspace.toml [extensions]
- **Size**: M
- **Tags**: #plugin #feature
- **Description**: Workspace-local command definitions
- **Configuration**:
  ```toml
  [extensions]
  commands = [
    { name = "deploy", module = "Tools.Deploy_command" },
    { name = "db", module = "Tools.Database_command" }
  ]
  ```

#### [P2] Dynamic compilation of workspace plugins
- **Size**: M
- **Tags**: #plugin #feature
- **Description**: Compile plugins as .cmxs during build
- **Location**: `target/debug/plugins/`

### Dependency Commands

#### [P2] Support package.toml [[command]]
- **Size**: M
- **Tags**: #plugin #feature
- **Description**: Dependencies provide commands
- **Configuration**:
  ```toml
  [[command]]
  name = "dream"
  description = "Dream web framework tools"
  module = "Dream_tusk.Command"
  ```

#### [P2] Dependency plugin compilation during build
- **Size**: M
- **Tags**: #plugin #feature
- **Description**: Build dependency plugins automatically

#### [P2] Plugin loading and execution
- **Size**: M
- **Tags**: #plugin #feature
- **Description**: Dynamic loading of plugin modules
- **Implementation**: Use `Dynlink` to load .cmxs files

#### [P2] Security permissions framework
- **Size**: L
- **Tags**: #plugin #feature #security
- **Description**: Sandbox plugin capabilities
- **Permissions**:
  - File read/write paths
  - Network access
  - Process spawning
  - RPC endpoint access

### Script Aliases

#### [P2] Script configuration parsing
- **Size**: S
- **Tags**: #plugin #feature
- **Description**: Parse [scripts] section in workspace.toml
- **Format**:
  ```toml
  [scripts]
  setup = ["clean", "build", "db.migrate", "test"]
  ci = ["lint", "test", "build --release"]
  ```

#### [P2] Command sequence execution
- **Size**: M
- **Tags**: #plugin #feature
- **Description**: Execute script command sequences
- **Features**: Error handling, rollback on failure

#### [P2] Argument passing and interpolation
- **Size**: S
- **Tags**: #plugin #feature
- **Description**: Pass arguments to scripts
- **Syntax**: `test-watch = ["test --watch $@"]`

### Advanced Plugin Features

#### [P3] Plugin hot reloading
- **Size**: M
- **Tags**: #plugin #feature
- **Description**: Reload plugins during development

#### [P3] Command completion generation
- **Size**: M
- **Tags**: #plugin #feature
- **Description**: Generate shell completions for plugins

#### [P3] Plugin marketplace/registry
- **Size**: XL
- **Tags**: #plugin #feature
- **Description**: Central plugin discovery and installation

---

## Phase 7: Cross-Compilation System

### Vision
Make OCaml development platform-agnostic like Go/Rust:
- Develop on macOS, deploy to Linux
- Share code between native and JavaScript
- Build desktop apps for Windows/macOS/Linux
- Deploy to embedded systems
- Target WebAssembly

### Target Management

#### [P3] Implement target configuration system
- **Size**: L
- **Tags**: #cross-compile #feature
- **Description**: Configure compilation targets in workspace.toml
- **Configuration**:
  ```toml
  [targets.linux-x64]
  triple = "x86_64-linux-gnu"
  container = "ubuntu:22.04"
  
  [targets.web-backend]
  backend = "melange"
  runtime = "node"
  
  [targets.wasm-web]
  backend = "wasm"
  runtime = "browser"
  ```
- **Targets**:
  - Native (Linux, macOS, Windows)
  - JavaScript (Node.js, Browser, React Native)
  - WebAssembly (WASI, Browser)
  - Embedded (ARM, RISC-V)

#### [P3] Target-specific dependency resolution
- **Size**: M
- **Tags**: #cross-compile #feature
- **Description**: Resolve dependencies per target
- **Format**:
  ```toml
  [dependencies.native]
  unix-support = "2.0"
  
  [dependencies.web]
  js-bindings = "3.0"
  ```

### Cross-Compilation Infrastructure

#### [P3] Implement cross-toolchain setup
- **Size**: XL
- **Tags**: #cross-compile #feature #infra
- **Description**: Build OCaml cross-compilers for different targets
- **Process**:
  1. Build host compiler
  2. Build target cross-compiler
  3. Setup sysroot
  4. Configure toolchain prefix
- **Implementation Details**:
  ```ocaml
  let build_ocaml_cross_compiler ~host_triple ~target_triple ~sysroot =
    (* Step 1: Build host compiler *)
    let* host_compiler = build_host_compiler ocaml_src host_build_dir in
    
    (* Step 2: Configure cross-compiler *)
    let configure_args = [
      Printf.sprintf "--host=%s" target_triple;
      Printf.sprintf "--target=%s" target_triple;
      Printf.sprintf "CC=%sgcc" toolchain_prefix;
      Printf.sprintf "CAMLRUN=%s/bin/ocamlrun" host_compiler;
    ] in
    
    (* Step 3: Build and install *)
    run_configure_and_make ocaml_src configure_args
  ```

#### [P3] Container-based cross-compilation
- **Size**: L
- **Tags**: #cross-compile #feature #docker
- **Description**: Use Docker for isolated cross-compilation
- **Dockerfile**:
  ```dockerfile
  FROM ubuntu:22.04
  RUN apt-get update && apt-get install -y \
      gcc-x86-64-linux-gnu \
      gcc-aarch64-linux-gnu
  ```

#### [P3] Target-specific code compilation
- **Size**: M
- **Tags**: #cross-compile #feature
- **Description**: Compile with target-specific flags
- **Conditional Compilation**:
  ```ocaml
  [%target_match
    | "native" -> Native_impl.http_get
    | "web-*" -> Web_impl.http_get
    | "wasm-*" -> Wasm_impl.http_get
  ]
  ```

### JavaScript/Melange Integration

#### [P3] Melange configuration and compilation
- **Size**: L
- **Tags**: #cross-compile #feature #js
- **Description**: Compile OCaml to JavaScript via Melange
- **Runtimes**: Node.js, Browser, React Native
- **Output**: ES6 modules or CommonJS

#### [P3] JavaScript interop
- **Size**: M
- **Tags**: #cross-compile #feature #js
- **Description**: Shared code between native and JS
- **Patterns**: Conditional module loading, external bindings

### WebAssembly Support

#### [P3] WASM compilation pipeline
- **Size**: XL
- **Tags**: #cross-compile #feature #wasm
- **Description**: Compile OCaml to WebAssembly
- **Runtimes**: Browser, WASI (WebAssembly System Interface)

#### [P3] WASI runtime integration
- **Size**: L
- **Tags**: #cross-compile #feature #wasm
- **Description**: WASM with system interface support
- **Features**: Filesystem access (sandboxed), HTTP requests

### Embedded Systems

#### [P3] Bare-metal compilation
- **Size**: XL
- **Tags**: #cross-compile #feature #embedded
- **Description**: Compile for microcontrollers
- **Targets**: ARM Cortex-M, RISC-V
- **Features**: Minimal runtime, no GC, manual memory

#### [P3] Embedded runtime development
- **Size**: XL
- **Tags**: #cross-compile #feature #embedded
- **Description**: Minimal runtime for embedded
- **Features**: No garbage collector, GPIO access, real-time timers

### Build Commands

```bash
# Single target build
tusk build --target linux-x64

# Multi-target build
tusk build --target all
tusk build --target "linux-*"

# Target-specific testing
tusk test --target native
tusk test --target linux-x64  # Run in container
```

---

## Infrastructure & Tooling

### CLI Improvements

#### [P1] Implement `tusk fmt` command
- **Size**: M
- **Tags**: #cli #feature #format
- **Description**: Format OCaml code (in help but not implemented)
- **Status**: Depends on Phase 4 Format System
- **Features**: Format workspace, packages, or files

#### [P1] Implement `tusk doc` command
- **Size**: M
- **Tags**: #cli #feature #docs
- **Description**: Generate documentation (in help but not implemented)
- **Features**:
  - Integration with odoc
  - Cross-package documentation
  - HTML output

#### [P2] Improve `tusk version` command
- **Size**: XS
- **Tags**: #cli #feature
- **Description**: Show actual version, commit hash, build date
- **Current**: Shows "dev"
- **Target**: "tusk 0.1.0 (abc123def 2024-01-15)"

#### [P2] Enhance `tusk clean` command
- **Size**: S
- **Tags**: #cli #feature
- **Description**: Report space freed, artifacts removed count

### Build System Improvements

#### [P1] Implement incremental rebuilds with content hashing
- **Size**: XL
- **Tags**: #build #feature #performance
- **Description**: Hash-based caching for minimal rebuilds
- **Strategy**: `<content_hash> -> outputs`
- **Impact**: Major performance improvement

#### [P2] Enhance build graph visualization
- **Size**: M
- **Tags**: #build #feature
- **Description**: Show build order, cycles, critical path

#### [P2] Better build error reporting
- **Size**: M
- **Tags**: #build #feature
- **Description**: File:line locations, suggested fixes

### MCP Response Enhancements

#### [P1] Enhance `build` MCP response
- **Size**: S
- **Tags**: #mcp #feature
- **Description**: Include duration, success/failure, detailed errors
- **Current**: "Build started successfully"
- **Needed**:
  - Build duration (total and per package)
  - Packages built/failed with details
  - Error messages with file:line
  - Suggested fixes
  - Build statistics (modules compiled, cache hits)

#### [P2] Enhance `build_graph` MCP response
- **Size**: S
- **Tags**: #mcp #feature
- **Description**: Add build order, cycle detection, module counts

#### [P2] Enhance `workspace_info` MCP response
- **Size**: S
- **Tags**: #mcp #feature
- **Description**: Add LOC, last build timestamp, workspace health

---

## Documentation

### User Documentation

#### [P2] Write user guide for tusk commands
- **Size**: M
- **Tags**: #docs
- **Description**: Comprehensive CLI documentation

#### [P2] Write MCP tools reference
- **Size**: M
- **Tags**: #docs #mcp
- **Description**: Complete MCP API documentation

#### [P2] Write plugin development guide
- **Size**: M
- **Tags**: #docs #plugin
- **Description**: How to create plugins

#### [P2] Write cross-compilation guide
- **Size**: M
- **Tags**: #docs #cross-compile
- **Description**: Setting up cross-compilation

### Developer Documentation

#### [P2] Document build system architecture
- **Size**: M
- **Tags**: #docs #infra
- **Description**: Internal architecture docs

#### [P2] Document module namespacing system
- **Size**: S
- **Tags**: #docs
- **Description**: Already documented in TUSK_NAMESPACING.md

#### [P2] Write contributor guide
- **Size**: M
- **Tags**: #docs
- **Description**: How to contribute to Tusk

---

## Future Enhancements (P3 - Long Term)

### Advanced Features
- Hot code reloading during development
- Distributed build system
- Remote build caching
- Binary package cache
- LSP server improvements
- REPL with project context
- Time-travel debugging
- Memory profiling
- Performance profiling across targets

### Ecosystem
- Package marketplace/registry
- Community plugins
- Template system
- Project generators
- CI/CD integrations
- Editor plugins (VSCode, Emacs, Vim)
- Documentation hosting
- Package statistics

---

## Migration Tasks

### Cleanup

#### [P2] Remove documentation .md files after migration to Linear
- **Size**: XS
- **Tags**: #infra #cleanup
- **Description**: Delete docs/*.md after converting to Linear issues
- **Files to remove**:
  - docs/TUSK_TEST.md
  - docs/TUSK_PACKAGE_MANAGEMENT.md
  - docs/TUSK_FMT.md
  - docs/TUSK_STD.md
  - docs/STD.md
  - docs/TUSK_PLUGINS.md
  - docs/TUSK_CROSS_COMPILATION.md
  - docs/build-flow-swimlanes.md
  - Keep: docs/TUSK_NAMESPACING.md (reference)

---

## Notes

- This file is the single source of truth for all planned work
- Update as new todos are discovered or completed
- Todos will be synced to Linear with appropriate metadata
- Archive completed todos in TODOS_ARCHIVE.md
- All implementation details from docs/*.md are now consolidated here
