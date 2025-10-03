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
- `tusk build` - Package building with dependency graph
- `tusk clean` - Build artifact cleanup
- `tusk new` - Package scaffolding
- `tusk install` - Binary installation
- `tusk run` - Binary execution
- `tusk version` - Version display (basic)

### 🚧 High Priority Next Steps
1. **Std.ArgParser** - Declarative CLI parsing (design complete, ready to implement)
2. **Std.Http.Client** - HTTP client for network requests
3. **tusk test** - Test framework with [@test] attributes
4. **tusk fmt** - Code formatting (mentioned in help but missing)
5. **tusk doc** - Documentation generation (mentioned in help but missing)

### 📦 Major Systems Not Yet Started
- Package Management (PubGrub resolver, registry, publish/install)
- Plugin System (workspace commands, dependency plugins)
- Cross-Compilation (multi-target builds)
- Format System (beyond basic fmt command)
- Advanced MCP Tools (most IDE-like features)

---

## Critical Bugs =4

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

## Phase 1: Core MCP Tools (Priority =%)

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

### Core Test Framework

#### [P1] Implement test discovery via `[@test]` attribute
- **Size**: M
- **Tags**: #test #feature
- **Description**: Scan workspace for .ml files with [@test] attributes
- **Module**: `test_discovery.ml`
- **Features**:
  - Regex-based scanning for `[@test]` pattern
  - Build package -> file -> tests mapping
  - Skip generated *_test.ml files

#### [P1] Implement test runner generation
- **Size**: M
- **Tags**: #test #feature
- **Description**: Generate *_test.ml files with test runners
- **Module**: `test_generator.ml`
- **Features**:
  - Include original source via `include struct`
  - Add test runner invocation
  - Handle module paths correctly

#### [P1] Implement test building
- **Size**: S
- **Tags**: #test #feature
- **Description**: Compile test files to executables
- **Module**: `test_builder.ml`
- **Features**:
  - Link with dependencies
  - Place in target/test/

#### [P1] Implement test execution
- **Size**: M
- **Tags**: #test #feature
- **Description**: Run test executables and collect results
- **Module**: `test_executor.ml`
- **Features**:
  - Capture output and exit codes
  - Report results
  - Return appropriate exit code

#### [P1] Add `tusk test` CLI integration
- **Size**: S
- **Tags**: #test #feature #cli
- **Description**: Add test subcommand to CLI
- **Features**:
  - Parse test-specific options
  - Invoke test pipeline

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

---

## Phase 3: Package Management System

### Core Package Management

#### [P1] Implement PubGrub resolver
- **Size**: XL
- **Tags**: #package #feature #resolver
- **Description**: Dependency resolution using PubGrub algorithm
- **Module**: `resolver.ml`
- **Features**:
  - Sound, complete dependency resolution
  - Clear conflict error messages
  - Version constraint handling

#### [P1] Implement local package cache
- **Size**: M
- **Tags**: #package #feature #cache
- **Description**: Cache downloaded packages locally
- **Location**: `~/.tusk/cache/`
- **Features**:
  - Store in `~/.tusk/cache/<pkg>-<version>/`
  - Cache registry metadata
  - Track checksums

#### [P1] Implement `tusk add` command
- **Size**: M
- **Tags**: #package #feature #cli
- **Description**: Add dependency to workspace/package
- **Usage**:
  - `tusk add riot` - add to workspace
  - `tusk add riot@2.0.0` - specific version
  - `tusk add -p mypackage riot` - add to package

#### [P1] Implement `tusk rm` command
- **Size**: S
- **Tags**: #package #feature #cli
- **Description**: Remove dependency from workspace/package

#### [P1] Implement lock file generation
- **Size**: M
- **Tags**: #package #feature
- **Description**: Generate tusk.lock for reproducible builds
- **Format**: TOML with package versions and checksums

### Package Registry

#### [P1] Implement HTTP API server
- **Size**: L
- **Tags**: #package #feature #registry
- **Description**: Package registry service
- **Endpoints**:
  - `GET /api/v1/packages` - list packages
  - `GET /api/v1/packages/:name` - get package metadata
  - `GET /api/v1/packages/:name/:version` - get specific version
  - `GET /api/v1/packages/:name/:version/tarball` - download
  - `POST /api/v1/packages/publish` - publish package

#### [P1] Implement package storage backend
- **Size**: M
- **Tags**: #package #feature #registry
- **Description**: Filesystem-based package storage for MVP
- **Structure**: `registry-data/packages/`

#### [P1] Implement `tusk publish` command
- **Size**: M
- **Tags**: #package #feature #cli
- **Description**: Publish packages to registry
- **Features**:
  - Load and validate tusk.toml
  - Create source tarball
  - Upload to registry with checksum

#### [P1] Implement package downloading
- **Size**: M
- **Tags**: #package #feature
- **Description**: Download and verify packages from registry

#### [P1] Implement checksum verification
- **Size**: S
- **Tags**: #package #feature #security
- **Description**: Verify downloaded packages against checksums

### Advanced Package Features

#### [P2] Version ranges and constraints
- **Size**: M
- **Tags**: #package #feature
- **Description**: Support `>=1.0.0, <2.0.0`, `~1.5.0` version specs

#### [P2] Authentication and authorization
- **Size**: L
- **Tags**: #package #feature #security
- **Description**: Token-based auth for publishing

#### [P2] Package search
- **Size**: M
- **Tags**: #package #feature
- **Description**: Search packages by name/description

#### [P2] Private registries
- **Size**: L
- **Tags**: #package #feature
- **Description**: Support for private package registries

#### [P2] Git dependencies
- **Size**: L
- **Tags**: #package #feature
- **Description**: Support dependencies from git repos

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

---

## Phase 4: Format System

### Core Formatting

#### [P1] Implement Format_manager
- **Size**: M
- **Tags**: #format #feature
- **Description**: Central coordinator for formatting operations
- **Module**: `format_manager.ml`
- **Features**:
  - Format cache management
  - Worker pool management
  - Error collection

#### [P1] Implement Format_worker
- **Size**: M
- **Tags**: #format #feature
- **Description**: Individual worker for formatting with pluggable backends
- **Module**: `format_worker.ml`
- **Backends**:
  - Ocamlformat_binary
  - Ocamlformat_rpc
  - Tusk_formatter (future)

#### [P1] Implement Format_cache
- **Size**: S
- **Tags**: #format #feature #cache
- **Description**: Disk-based cache for formatted files
- **Module**: `format_cache.ml`
- **Location**: `./target/<profile>/fmt/`
- **Features**:
  - Content hash-based caching
  - Persistent across restarts

#### [P1] Add `tusk fmt` CLI support
- **Size**: S
- **Tags**: #format #feature #cli
- **Description**: Format command line interface
- **Usage**:
  - `tusk fmt` - format workspace
  - `tusk fmt -p mypackage` - format package
  - `tusk fmt src/main.ml` - format specific files
  - `tusk fmt --check` - check formatting

#### [P1] Add RPC interface for formatting
- **Size**: M
- **Tags**: #format #feature #rpc
- **Description**: Structured format requests/responses via RPC

#### [P2] Add MCP tools for formatting
- **Size**: S
- **Tags**: #format #feature #mcp
- **Description**: `formatFile`, `formatPackage`, `checkFormatting` tools

### Advanced Formatting Features

#### [P2] Implement concurrent formatting with worker pool
- **Size**: M
- **Tags**: #format #feature #performance
- **Description**: Parallel formatting across multiple files

#### [P2] Implement ocamlformat-rpc backend
- **Size**: M
- **Tags**: #format #feature
- **Description**: Use RPC for better performance

#### [P2] Implement incremental formatting
- **Size**: S
- **Tags**: #format #feature
- **Description**: Only format changed files

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
  - Method, Status, Header, Request, Response, Version
- ✅ **Std.Command** - System command execution
- ✅ **Std.Env** - Environment variable access
- ✅ **Std.Time.*** - Time types (Duration, Instant, SystemTime)
- ✅ **Std.Collections.*** - Data structures (Vector, HashMap, HashSet, Queue, Deque)
- ✅ **Std.Graph.*** - Graph utilities (Dot, Mermaid)
- ✅ **Std.Log** - Structured logging
- ✅ **Std.DateTime** - Date and time handling
- ✅ **Std.WorkerPool** - Parallel task execution

### CLI & Application Support

#### [P1] Implement Std.ArgParser - Declarative CLI argument parsing
- **Size**: L
- **Tags**: #std #feature #cli #argparse
- **Module**: `packages/std/src/arg_parser.ml`
- **Status**: Not implemented - design complete
- **Inspiration**: Rust's Clap library - declarative builder pattern with type-safe extraction
- **Philosophy**:
  - Declarative schema definition using builder pattern
  - Auto-generate help text from schema
  - Type-safe argument extraction
  - Support flags, options, positional args, and subcommands
  - Clap-style extraction API for ergonomic pattern matching

**Core API Design**:
```ocaml
module Std.ArgParser : sig
  type command
  type matches
  
  (* Command construction *)
  val command : string -> command
  val version : string -> command -> command
  val about : string -> command -> command
  val author : string -> command -> command
  
  (* Adding arguments and subcommands *)
  val arg : 'a Arg.t -> command -> command
  val subcommand : command -> command -> command
  
  (* Parsing *)
  val get_matches : command -> string list -> (matches, error) result
  
  (* Extracting values - Clap style! *)
  val get_one : matches -> string -> string option
  val get_flag : matches -> string -> bool
  val get_count : matches -> string -> int  (* for -vvv *)
  val get_many : matches -> string -> string list
  val get_int : matches -> string -> int option
  val get_float : matches -> string -> float option
  val get_path : matches -> string -> Path.t option
  
  (* Subcommand matching *)
  val subcommand : matches -> (string * matches) option
  val subcommand_name : matches -> string option
  val subcommand_matches : matches -> string -> matches option
  
  (* Argument builders *)
  module Arg : sig
    type 'a t
    
    val flag : string -> bool t
    val option : string -> string t
    val positional : string -> string t
    val trailing : string -> string list t
    
    (* Chainable modifiers *)
    val short : char -> 'a t -> 'a t
    val long : string -> 'a t -> 'a t
    val help : string -> 'a t -> 'a t
    val value_name : string -> 'a t -> 'a t
    val required : bool -> 'a t -> 'a t
    val default : string -> 'a t -> 'a t
    val env : string -> 'a t -> 'a t
    val action : action -> 'a t -> 'a t
    val multiple : 'a t -> 'a t
    val count : bool t -> bool t  (* -vvv = 3 *)
    val possible_values : string list -> 'a t -> 'a t
    val conflicts_with : string -> 'a t -> 'a t
    val requires : string -> 'a t -> 'a t
  end
  
  type action = Set | SetTrue | SetFalse | Append | Count
  type error = (* ... *)
end
```

**Example Usage**:
```ocaml
let cli =
  ArgParser.command "tusk"
  |> ArgParser.version "0.1.0"
  |> ArgParser.about "OCaml build system"
  |> ArgParser.arg Arg.(
      flag "verbose"
      |> short 'v'
      |> long "verbose"
      |> help "Enable verbose output"
      |> count  (* supports -vvv *)
  )
  |> ArgParser.subcommand (
      ArgParser.command "build"
      |> ArgParser.about "Build packages"
      |> ArgParser.arg Arg.(
          option "package"
          |> short 'p'
          |> long "package"
          |> help "Build specific package"
      )
      |> ArgParser.arg Arg.(
          flag "release"
          |> long "release"
          |> help "Build in release mode"
      )
  )

match ArgParser.get_matches cli Env.args with
| Ok matches ->
    let verbose_level = ArgParser.get_count matches "verbose" in
    (match ArgParser.subcommand matches with
    | Some ("build", build_matches) ->
        let package = ArgParser.get_one build_matches "package" in
        let release = ArgParser.get_flag build_matches "release" in
        Build.run ~verbose_level ~release ?package ()
    | _ -> ())
| Error err ->
    ArgParser.print_error err;
    exit 1
```

**Features**:
- Declarative builder pattern with `|>` chaining
- Auto-generated help text (`--help`, `-h`)
- Auto-generated version flag if version is set
- Support for short (`-v`) and long (`--verbose`) flags
- Count repeated flags (`-vvv` = verbosity level 3)
- Required vs optional arguments
- Default values
- Environment variable fallback
- Argument validation (possible_values, conflicts_with, requires)
- Positional arguments
- Trailing arguments (after `--`)
- Multiple subcommand levels
- Type-safe extraction with proper error handling
- Clean pattern matching on subcommands

**Implementation Notes**:
- Use string-based argument names for simplicity (like Clap)
- Separate getters for different types (get_flag, get_one, get_int, etc.)
- Builder pattern returns new immutable values
- Parse returns Result for proper error handling
- Auto-generate usage strings from schema
- Support both `subcommand` (returns tuple) and `subcommand_matches` (returns matches option)

**Dependencies**:
- None - pure OCaml implementation
- Uses existing Std.String, Std.List, Std.Result

**Future Enhancements**:
- Shell completion generation (bash, zsh, fish)
- Custom value parsers
- Argument groups for help organization
- Colored help output using existing color support
- PPX for deriving CLI from record types

### Network & HTTP

#### [P1] Implement Std.Http.Client - HTTP client
- **Size**: L
- **Tags**: #std #feature #network
- **Module**: `packages/std/src/http.ml`
- **Features**:
  - GET, POST, PUT, DELETE
  - JSON helpers
  - Connection pooling
  - Non-blocking with Gluon

#### [P2] Implement Std.Net - Core networking
- **Size**: L
- **Tags**: #std #feature #network
- **Module**: `packages/std/src/net.ml`
- **Features**:
  - Socket operations
  - TCP streams
  - UDP sockets

#### [P2] Implement Std.Http.Server - HTTP server
- **Size**: XL
- **Tags**: #std #feature #network
- **Module**: `packages/std/src/http.ml`
- **Features**:
  - Request handling
  - Middleware support
  - WebSocket support

### Data Formats

#### [P2] Implement Std.Xml - XML parsing
- **Size**: M
- **Tags**: #std #feature #data
- **Module**: `packages/std/src/xml.ml`
- **Status**: Not implemented
- **Features**:
  - DOM and SAX parsing
  - XPath support
  - XML generation

#### [P2] Implement Std.Csv - CSV handling
- **Size**: S
- **Tags**: #std #feature #data
- **Module**: `packages/std/src/csv.ml`
- **Status**: Not implemented
- **Features**:
  - CSV parsing and writing
  - Custom delimiters
  - Header handling

### Actor System Extensions

#### [P2] Implement Std.Supervisor - Dynamic supervision
- **Size**: L
- **Tags**: #std #feature #actor
- **Module**: `packages/std/src/supervisor.ml`
- **Features**:
  - One-for-one, one-for-all, rest-for-one strategies
  - Dynamic child management
  - Restart policies

#### [P2] Implement Std.Agent - Stateful actors
- **Size**: M
- **Tags**: #std #feature #actor
- **Module**: `packages/std/src/agent.ml`
- **Features**:
  - Get, update, cast, call operations
  - Simple state management

#### [P2] Implement Std.Registry - Process discovery
- **Size**: M
- **Tags**: #std #feature #actor
- **Module**: `packages/std/src/registry.ml`
- **Features**:
  - Named process registration
  - Global registry

### Application Support

#### [P2] Implement Std.Config - Configuration management
- **Size**: M
- **Tags**: #std #feature #app
- **Module**: `packages/std/src/config.ml`
- **Status**: Not implemented
- **Features**:
  - Load from files and environment
  - Nested configuration access
  - Type-safe config extraction

#### [P3] Implement Std.Sql - Database interface
- **Size**: XL
- **Tags**: #std #feature #app
- **Module**: `packages/std/src/sql.ml`
- **Features**:
  - Connection management
  - Query execution
  - Transactions

---

## Phase 6: Plugin System

### Core Plugin Infrastructure

#### [P2] Extract packages/tusk-rpc library
- **Size**: M
- **Tags**: #plugin #feature #infra
- **Description**: Separate RPC library for plugins to use

#### [P2] Create packages/tusk-lib with command interface
- **Size**: M
- **Tags**: #plugin #feature #infra
- **Description**: Command interface for plugins
- **Module**: `packages/tusk-lib/command.mli`

#### [P2] Implement plugin discovery and registration
- **Size**: M
- **Tags**: #plugin #feature
- **Description**: Scan workspace and dependencies for commands

### Workspace Commands

#### [P2] Support workspace.toml [extensions]
- **Size**: M
- **Tags**: #plugin #feature
- **Description**: Allow defining workspace-local commands

#### [P2] Dynamic compilation of workspace plugins
- **Size**: M
- **Tags**: #plugin #feature
- **Description**: Compile plugins as .cmxs dynamic libraries

### Dependency Commands

#### [P2] Support package.toml [[command]]
- **Size**: M
- **Tags**: #plugin #feature
- **Description**: Dependencies can provide commands

#### [P2] Dependency plugin compilation during build
- **Size**: M
- **Tags**: #plugin #feature
- **Description**: Build dependency plugins automatically

#### [P2] Plugin loading and execution
- **Size**: M
- **Tags**: #plugin #feature
- **Description**: Dynamic loading of plugin modules

#### [P2] Security permissions framework
- **Size**: L
- **Tags**: #plugin #feature #security
- **Description**: Sandbox plugin capabilities

### Script Aliases

#### [P2] Script configuration parsing
- **Size**: S
- **Tags**: #plugin #feature
- **Description**: Parse [scripts] section in workspace.toml

#### [P2] Command sequence execution
- **Size**: M
- **Tags**: #plugin #feature
- **Description**: Execute script command sequences

#### [P2] Argument passing and interpolation
- **Size**: S
- **Tags**: #plugin #feature
- **Description**: Pass arguments to scripts

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

### Target Management

#### [P3] Implement target configuration system
- **Size**: L
- **Tags**: #cross-compile #feature
- **Description**: Configure compilation targets in workspace.toml
- **Targets**:
  - Native (Linux, macOS, Windows)
  - JavaScript (Melange/Node/Browser)
  - WebAssembly (WASI, Browser)
  - Embedded (ARM, RISC-V)

#### [P3] Target-specific dependency resolution
- **Size**: M
- **Tags**: #cross-compile #feature
- **Description**: Resolve dependencies per target

### Cross-Compilation Infrastructure

#### [P3] Implement cross-toolchain setup
- **Size**: XL
- **Tags**: #cross-compile #feature #infra
- **Description**: Build OCaml cross-compilers for different targets
- **Process**:
  - Build host compiler
  - Build target cross-compiler
  - Setup sysroot

#### [P3] Container-based cross-compilation
- **Size**: L
- **Tags**: #cross-compile #feature #docker
- **Description**: Use Docker for isolated cross-compilation

#### [P3] Target-specific code compilation
- **Size**: M
- **Tags**: #cross-compile #feature
- **Description**: Compile with target-specific flags and backends

### JavaScript/Melange Integration

#### [P3] Melange configuration and compilation
- **Size**: L
- **Tags**: #cross-compile #feature #js
- **Description**: Compile OCaml to JavaScript via Melange

#### [P3] JavaScript interop
- **Size**: M
- **Tags**: #cross-compile #feature #js
- **Description**: Shared code between native and JS

### WebAssembly Support

#### [P3] WASM compilation pipeline
- **Size**: XL
- **Tags**: #cross-compile #feature #wasm
- **Description**: Compile OCaml to WebAssembly

#### [P3] WASI runtime integration
- **Size**: L
- **Tags**: #cross-compile #feature #wasm
- **Description**: WASM with system interface support

### Embedded Systems

#### [P3] Bare-metal compilation
- **Size**: XL
- **Tags**: #cross-compile #feature #embedded
- **Description**: Compile for microcontrollers

#### [P3] Embedded runtime development
- **Size**: XL
- **Tags**: #cross-compile #feature #embedded
- **Description**: Minimal runtime for embedded systems

---

## Infrastructure & Tooling

### CLI Improvements

#### [P1] Implement `tusk fmt` command
- **Size**: M
- **Tags**: #cli #feature #format
- **Description**: Format OCaml code (currently in help text but not implemented)
- **Status**: Mentioned in CLI help but handler missing
- **Depends on**: Phase 4 Format System

#### [P1] Implement `tusk doc` command
- **Size**: M
- **Tags**: #cli #feature #docs
- **Description**: Generate documentation (currently in help text but not implemented)
- **Status**: Mentioned in CLI help but handler missing
- **Features**:
  - Integration with odoc
  - Cross-package documentation

#### [P2] Improve `tusk version` command
- **Size**: XS
- **Tags**: #cli #feature
- **Description**: Currently shows "dev", should show actual version, commit hash, build date
- **Status**: Basic implementation exists

#### [P2] Enhance `tusk clean` command improvements
- **Size**: S
- **Tags**: #cli #feature
- **Description**: Report space freed, artifacts removed count

### Build System Improvements

#### [P1] Implement incremental rebuilds with content hashing
- **Size**: XL
- **Tags**: #build #feature #performance
- **Description**: Hash-based caching for minimal rebuilds
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

#### [P2] Remove all documentation .md files after migration to Linear
- **Size**: XS
- **Tags**: #infra #cleanup
- **Description**: Delete docs/*.md and MCP_TOOLS_ROADMAP.md after converting to Linear issues
- **Files to remove**:
  - docs/TUSK_TEST.md
  - docs/TUSK_PACKAGE_MANAGEMENT.md
  - docs/TUSK_FMT.md
  - docs/TUSK_STD.md
  - docs/STD.md
  - docs/TUSK_PLUGINS.md
  - docs/TUSK_CROSS_COMPILATION.md
  - docs/build-flow-swimlanes.md
  - docs/TUSK_NAMESPACING.md (keep as reference?)
  - MCP_TOOLS_ROADMAP.md

---

## Notes

- This file should be the single source of truth for all planned work
- Update this file as new todos are discovered or completed
- Todos will be synced to Linear with appropriate metadata
- Archive completed todos in TODOS_ARCHIVE.md
