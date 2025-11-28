# RIOT MONOREPO: COMPREHENSIVE PACKAGE TAXONOMY & ARCHITECTURE

## META-INFORMATION
- Repository: /Users/ostera/Developer/github.com/riot-ml/riot
- Language: OCaml (custom runtime, nostdlib mode)
- Paradigm: Actor model (Erlang-inspired) + functional programming
- Build system: Tusk (custom, cargo-like)
- Total source files: ~2513 .ml/.mli files
- Active workspace members: 51 packages
- Platform: darwin (macOS focus with CoreFoundation/CoreServices integration)
- Native components: Rust (raml-rt runtime, FFI bindings)

## CRITICAL ARCHITECTURAL INVARIANTS (ABSOLUTE REQUIREMENTS)

1. **Stdlib replacement**: ALL code MUST `open Std` at top of file, NEVER import Stdlib/Unix/Sys/Obj modules
2. **Mutable values**: Use `Std.Cell.t` for mutable values, NEVER use OCaml's built-in `ref` type
3. **Mutable record fields**: Use `mutable field_name : type` syntax in records, NOT Cell for record fields
4. **Build execution**: `tusk` command always operates from workspace root regardless of current directory
5. **File editing**: Use Edit tool exclusively, NEVER sed/awk/python/bash for file modifications
6. **Interface design**: Prefer abstract types in .mli files for encapsulation
7. **Test philosophy**: Aggressively use Option.expect/Result.expect in tests for happy-path focus, NOT graceful None/Error handling
8. **Toolchain isolation**: NEVER use ocamlc/opam/dune/ocamldoc directly
9. **Long-running commands**: Wrap with `timeout T <cmd>` to prevent infinite hangs
10. **Command discovery**: Use `tusk completions --binaries/--tests/--packages` to discover available targets
11. **Config module conflicts**: When a package defines its own Config module, alias Std.Config with `module Conf = Config` or `module StdConfig = Config`
12. **Error types**: Prefer structured variant types over strings for better error handling and pattern matching

## CANONICAL ACTOR PATTERN (coordinator.ml reference)
```ocaml
(* Type definitions at top *)
type state = { immutable_field : T.t; mutable_field : T.t Cell.t }

(* Mutually recursive helper functions *)
let rec helper_function state = (* processing logic *) loop state

and loop state =
  if termination_condition then ()
  else match receive ~selector () with
  | Case1 x -> handle_case1 state x
  | Case2 y -> handle_case2 state y

and handle_case1 state x =
  (* perform work *)
  loop state  (* tail-recursive call *)

(* Initialization function *)
let init ~params =
  let state = { immutable_field = ...; mutable_field = Cell.make ... } in
  loop state

Key patterns: mutually recursive with let rec...and..., tail-recursive loops, selector functions for message filtering, dedicated handler per pattern (handle_X naming), scoped opens with let open Module in

---

## LAYER 0: RUNTIME PRIMITIVES & EVENT LOOP

### kernel (FFI foundation)

• Purpose: Low-level FFI to C, platform event loop (epoll/kqueue), system primitives
• Key modules:
 • Env (environment vars)
 • Fd (file descriptors)
 • IO (async I/O with Reader/Writer traits, map_err for error type transformation)
 • Async (event loop)
 • Collections (Vector, HashMap, HashSet, Queue)
 • Crypto (Sha256, FFI.hmac_sha256 via CommonCrypto on macOS)
 • Sync (synchronization primitives)
 • Terminal (ANSI control)
 • Time (Duration, Instant)
 • System, Fs, Iter
 • Net (error type changed: System_error of IO.error instead of string)
 • Ops (all operators extracted from Global)
 • Global (includes Ops, Type, Process, Pid primitives)
 • Types
 • Effect (reexport Stdlib.Effect)
• Platform specifics: macOS integration via CoreFoundation/CoreServices frameworks
• Dependencies: stdlib, unix ONLY (no other riot packages)
• Critical: This is the ONLY package allowed to depend on stdlib/unix

### miniriot (single-core actor runtime)

• Purpose: Minimal actor runtime with cooperative scheduling, message passing, process lifecycle
• Key modules:
 • Process (spawn, state machine: Uninitialized→Runnable→Waiting_message/Waiting_io→Running→Exited→Finalized)
 • Pid (process identifiers)
 • Message (extensible message type)
 • Mailbox (message queue per process)
 • Scheduler (cooperative scheduler with reduction counting)
 • Timer (timer wheel for delayed/periodic messages)
 • Config (runtime configuration with timer_resolution: Millisecond|Microsecond|Nanosecond)
 • Effects (Process effects: yield, receive, receive_any with optional timeout)
• Process communication: send/receive with selector functions, monitors, links
• Exception handling: EXIT messages, DOWN messages, monitor_ref tracking
• Dependencies: kernel
• Architecture: Single-threaded event loop, reduction-based preemption, mailbox per process

### std (MANDATORY standard library)

• Purpose: Complete stdlib replacement providing modern, safe APIs for ALL packages
• Dependency rule: ALL packages (except kernel, miniriot) depend on std, MUST open Std
• Size: ~80+ modules across multiple domains
• Dependencies: miniriot + kernel

#### std: Core Types & Primitives

• Result: Result.t with map, bind, expect, unwrap, is_ok, is_error
• Option: Option.t with map, bind, expect, unwrap, is_some, is_none
• Cell: Mutable cell type (replacement for ref), Cell.make, Cell.get, Cell.set
• Bool, Int, Int32, Int64, Float, String, Char, Uchar: Enhanced versions with useful utilities
• Ptr: Pointer utilities

#### std: Collections (Std.Collections.*)

• HashMap: Hash table, O(1) average operations, create/insert/find/remove/iter
• HashSet: Set of unique values, of_list/insert/contains/remove/union/intersection
• Vector: Growable array, O(1) indexed access, push/pop/get/set/of_list
• Queue: FIFO queue, enqueue/dequeue
• Deque: Double-ended queue, push_front/push_back/pop_front/pop_back
• Heap: Binary heap for priority queues, insert/extract_min/peek

#### std: Iterator (Std.Iter.*)

• Iterator: Lazy iteration protocol, map/filter/fold/take/drop/zip/chain/collect

#### std: Filesystem (Std.Fs., Std.Path.)

• Path: Type-safe path handling, v/join/parent/extension/with_extension/is_absolute
• Fs: Filesystem operations, read/write/exists/create_dir/remove/copy/rename
• File: File handle with read/write/seek
• Fd: File descriptor operations
• Metadata: File metadata (size, modified time, permissions)
• Permissions: Unix-style permissions
• ReadDir: Directory iteration
• FileWatcher: Inotify/FSEvents-based file watching for hot reload

#### std: I/O (Std.IO.*)

• IO: Read/write traits, buffered I/O
• Reader: Read trait with map_err for error type transformation
• Writer: Write trait with map_err for error type transformation

#### std: Time (Std.Time.*)

• Duration: Time span representation (from_secs, from_millis, to_nanos, add, sub)
• Instant: Monotonic timestamp for measuring elapsed time (now, elapsed, duration_since)
• SystemTime: Wall-clock time (now, unix_timestamp)
• Datetime: Date/time parsing and formatting
• Timer: Delayed execution, periodic callbacks

#### std: Concurrent Primitives (Actor runtime integration)

• Process: spawn, spawn_link, self, send, receive, receive_any, exit, monitor, demonitor
• Pid: Process identifier type
• Message: Extensible message type (Message.t += CustomMsg of ...)
• Agent: Lightweight parametric state server (start, start_link, get, update, get_and_update, cast, stop), generic over state type 'state Agent.t
• GenStage: Generic stage for stream processing (producer/consumer patterns)
• Supervisor: Supervision trees with restart strategies (one_for_one, one_for_all, rest_for_one)
• Task: Async task execution (async, await), higher-level than spawn
• WorkerPool: Fixed-size worker pool with two modes:
 • SimpleWorkerPool: Parallel map (run ~concurrency ~tasks ~fn), blocks until all complete, ordered results
 • DynamicWorkerPool: Manual task assignment (start ~concurrency ~owner ~worker_fn), receives WorkerReady messages, dynamic task dispatch
 • Architecture: coordinator process + N worker processes, work-stealing queue
• Sync: Synchronization (Mutex, RwLock, Condvar, Barrier, Once)
• Global: Global process registry (register, whereis, unregister)

#### std: Networking (Std.Net.*)

• Net.Uri: URI parsing/construction (parse, make, scheme, host, port, path, query)
• Net.Addr: Network addresses (IPv4, IPv6, Unix domain sockets)
• TcpListener: TCP server socket (bind, accept, local_addr)
• TcpStream: TCP client connection (connect, read, write, shutdown)
• TcpClient: High-level TCP client
• TcpServer: High-level TCP server with connection handling
• Net.Http: HTTP types (Request, Response, Status, Headers, Method, Version)
 • Method: GET, POST, PUT, DELETE, PATCH, HEAD, OPTIONS, TRACE, CONNECT
 • Status: 200 OK, 404 Not Found, 500 Internal Server Error, etc.
 • Header: key-value pairs, case-insensitive lookup
 • Version: HTTP/1.0, HTTP/1.1, HTTP/2, HTTP/3


#### std: Data Formats (Std.Data.*)

• Json: JSON parsing/serialization (parse, to_string, Object, Array, String, Number, Bool, Null)
• Toml: TOML parsing (parse, to_string), compliant with v1.0.0 spec
• Sexp: S-expression parsing/printing
• Xml: XML parsing/generation
• Csv: CSV parsing/writing
• Base16, Base32, Base64, Base85: Encoding/decoding schemes

#### std: Graph (Std.Graph.*)

• SimpleGraph: Directed/undirected graph, add_node/add_edge/neighbors/topo_sort
• Dot: Graphviz DOT format export
• Mermaid: Mermaid diagram format export

#### std: Crypto (Std.Crypto.*)

• Sha256: SHA-256 hashing (create, update, finalize, hash)
• Hmac: HMAC-SHA256 via FFI to CommonCrypto (macOS)
• Digest: Generic digest interface
• Hasher: Hasher trait

#### std: System (Std.*)

• Command: Process execution (run, output, spawn, stdin/stdout/stderr piping)
• Env: Environment variables (var, set_var, remove_var, vars)
• System: System information (os, arch, hostname, num_cpus)
• Random: Random number generation (int, float, bytes, uuid)
• Uuid: UUID generation (v4, v7, parse, to_string)
• Ops: All operators (arithmetic, comparison, logical) - available via Ops module or through Global
• Telemetry: Event instrumentation (attach, execute, span)
• Exception: Exception handling utilities
• Panic: Panic with traceback
• Application: Application lifecycle, supervision tree management with dependency resolution
• ArgParser: Command-line argument parsing
• Version: Semantic versioning (parse, compare, to_string)
• Type: Type utilities and RTTI
• CharCursor: Character stream cursor for parsing
• Diff: Diff algorithms (Myers, patience)

#### std: Configuration (Std.Config.*)

• Purpose: Type-safe configuration management with TOML support
• Config.Spec: DSL for typed configuration schemas with validation rules
• Config.Loader: Environment-aware file loading (dev/test/prod environments)
• Config.Validator: Type-safe validation with default values and constraints
• Namespaced sections: Support for `[myapp]`, `[otherapp]` in TOML files
• Use cases: Application config, library settings, environment-specific overrides
• Pattern: When packages define their own Config module, alias Std.Config as `module Conf = Config`

#### std: Logging (Std.Log.*)

• Purpose: Structured logging with flexible handler system
• Architecture: Direct handler calls (no actor overhead for performance)
• Submodules:
 • Level: Debug, Info, Warn, Error, Trace log levels
 • Metadata: Contextual metadata (timestamps, module names, PIDs)
 • Event: Log event type with level, message, metadata
 • Handler: Handler interface for log output
• StdoutHandler: Supervised process for stdout logging
• Configuration: Via `[[log.handler]]` TOML sections for handler setup
• API: error, warn, info, debug, trace, set_level functions

#### std: Unicode

• Unicode: UTF-8/UTF-16 encoding/decoding, normalization

#### std: Test

• Test: Testing utilities, assertions

---

## LAYER 1: BUILD SYSTEM (TUSK - cargo-like for OCaml)

### tusk-model (Type definitions for build system)

• Purpose: AST/data model for entire build system, shared types across all tusk-* packages
• Key modules:
 • Package: package definition (name, version, path, lib, bins, tests, dependencies, targets, profiles)
 • Workspace: workspace.toml representation (members, shared dependencies)
 • Workspace_manager: Load workspace from disk, resolve member paths
 • Module: OCaml module representation (name, path, interface, implementation, dependencies)
 • Module_name: Module name handling (Main, Lib, Bin, Test, to_string, of_string)
 • Dependency: Dependency specification (path, version constraints)
 • Node_id: Unique identifiers for graph nodes (Package_id, Module_id, Action_id)
 • Session_id: Build session identifiers for tracking
 • Build_ctx: Build context (workspace, package, toolchain, platform, profile, session, target directories)
 • Profile: Build profiles (debug, release, custom), optimization flags, debug_info, assertions
 • Platform: Platform detection/representation (target_platform_name, host_platform_name, os, arch)
 • Toolchain_config: Toolchain paths and configuration
 • Namespace: Module namespace handling
 • Event: Build events (CompilationStarted, CompilationCompleted, TestPassed, etc.)
 • Error: Error types (build_error, build_result with skip_reason, error_kind)
 • Ocaml_compiler: Compiler invocation (ocamlc, ocamlopt flags, output paths)
 • Tusk_dirs: Standard directory layout (_build, .tusk, cache, artifacts)
 • Worker_id: Worker process identifiers
• Hash integration: Build_ctx.hash for incremental compilation, feeds into Crypto.Sha256
• Dependencies: std

### tusk-planner (Incremental build planning with Merkle DAGs)

• Purpose: Converts workspace→packages→modules→actions into executable DAGs with content-based hashing
• Architecture: Four-level planning hierarchy
 1. Workspace_planner: Resolves package dependency graph, determines build order, handles cyclic deps
  • Types: build_target (AllPackages | Package of string | Binary of string), plan_error (CyclicDependency | UnknownPackage | LoadError)
  • Output: Ordered list of packages to build, package dependency graph
 2. Package_planner: Plans single package with dependency-aware hashing (checks if deps planned, computes Merkle hash from dep hashes + package sources)
  • Detects MissingDependencies if deps not ready
  • Returns Planned with content hash or cache hit
 3. Module_planner: Scans filesystem for .ml/.mli files, builds module dependency graph via ocamldep, handles circular deps
  • Module_scanner: Filesystem scanning for discovering modules
  • Module_registry: Tracks discovered modules
  • Module_graph: DAG of module dependencies
  • Module_node: Individual module with deps, sources, artifact paths
 4. Action_graph: Converts module graph to executable actions (CompileCMI, CompileCMO, CompileCMX, LinkExecutable, RunTest)
  • Action_node: Individual compilation/link action with inputs/outputs/command
  • Action: Executable build step (compile/link/test)
  • hash_action_node: Merkle tree hashing of action graph for incremental builds

• Key modules:
 • Dependency: Dependency resolution logic
 • Library_definition: Library target configuration
 • Library_interface: Exposed library interface
 • Alias_module: Module aliasing for namespacing
 • Planning_error: Error types for plan failures
• Output: to_action_list returns ordered Action.t list for executor, to_json/from_json for plan serialization/comparison
• Dependencies: std, tusk-model

### tusk-executor (Parallel build execution)

• Purpose: Executes action graphs in parallel using WorkerPool, handles caching, retries
• Key types:
 • action_error: Compilation failed, Link failed, Test failed, Cache miss, Dependency failed
 • action_status: Success, Cached, Skipped, Failed
 • execution_result: { action: Action.t; status: action_status; stdout: string; stderr: string; duration: Duration.t }
 • workspace_result: Collection of execution results across all packages
• Functions:
 • execute: Sequential execution for debugging/testing (no parallelism)
 • build_workspace: Parallel package builds using WorkerPool, respects dependency order
• Architecture: Uses Std.WorkerPool for parallel compilation, coordinator distributes actions to workers, workers report completion
• Caching: Checks tusk-store before executing, stores artifacts on success
• Dependencies: std, tusk-model, tusk-planner, tusk-store

### tusk-store (Content-addressable artifact cache)

• Purpose: Stores compiled artifacts by content hash, enables incremental builds across machines
• Key modules:
 • Store: Main interface (create ~workspace, lookup ~hash, insert ~hash ~files)
 • Artifact: Representation of stored artifact (hash + file list)
 • Manifest: Metadata for artifacts (V0 version, file_entry with path/hash/size)
• Storage layout: .tusk/store/<first 2 hash chars>//manifest.json + files
• Operations: Content-addressed lookup by hash, atomic insertion, garbage collection
• Dependencies: std, tusk-model

### tusk-protocol (JSON-RPC wire protocol)

• Purpose: Defines JSON-RPC API for tusk-server ↔ tusk-client communication (LSP-inspired)
• Protocol: JSON-RPC 2.0 (request/response/notification)
• Methods: build, test, clean, watch, get_diagnostics, get_build_graph
• Events: BuildStarted, BuildProgress, BuildCompleted, DiagnosticPublished
• Dependencies: std, tusk-model

### tusk-server (Build daemon)

• Purpose: Long-running build server for incremental compilation, watch mode, editor integration
• Key modules:
 • Jsonrpc_server: JSON-RPC request handling (666 lines - main logic)
 • Server_manager: Server lifecycle management
 • Protocol: Protocol implementation
• Features: Persistent process cache, file watching for rebuild triggers, concurrent build sessions
• Communication: Listens on Unix socket or TCP port, accepts JSON-RPC requests
• Dependencies: std, tusk-model, tusk-planner, tusk-executor, tusk-store, tusk-protocol

### tusk-client (Build client library)

• Purpose: Library for connecting to tusk-server via JSON-RPC
• Features: Async request/response, streaming build events, connection pooling
• Use cases: Editor plugins, CI/CD integration, programmatic builds
• Dependencies: std, tusk-protocol

### tusk-cli (Command-line interface)

• Purpose: Main tusk binary, user-facing CLI
• Commands: build, test, run, clean, init, new, fmt, fix, repl, completions, server (start/stop/status)
• Flags: --release, --verbose, --jobs N, --watch
• Architecture: Parses args with Std.ArgParser, delegates to tusk-client for build operations
• Dependencies: std, tusk-client, tusk-model, tusk-protocol

### tusk-toolchain (Compiler wrapper)

• Purpose: Abstracts OCaml compiler invocations (ocamlc/ocamlopt/ocamldep)
• Key modules:
 • Ocamlc: ocamlc wrapper with flag generation
 • Ocamldep: Dependency analysis
 • Ocamlformat: Code formatting
 • Tusk_toolchain: Unified toolchain interface
• Toolchain discovery: Searches PATH, checks versions, validates installations
• Dependencies: std, tusk-model

### tusk-repl (Interactive toplevel)

• Purpose: REPL for interactive OCaml evaluation in tusk projects
• Features: Load project modules, incremental compilation, history
• Dependencies: std, tusk-model

### tusk-eval (Expression evaluator)

• Purpose: Evaluate OCaml expressions programmatically
• Dependencies: std, tusk-model

### tusk-mcp (Model Context Protocol integration)

• Purpose: MCP server/client for AI tooling integration
• Dependencies: std, tusk-model, mcp

### tusk-tests (Test suite)

• Purpose: Integration tests for entire tusk build system
• Dependencies: All tusk-* packages

### minitusk (Bootstrap builder)

• Purpose: Minimal build system to bootstrap real tusk (chicken-egg problem)
• Scope: Just enough to compile tusk-cli from sources without requiring tusk itself
• Philosophy: Throwaway code, not maintained for features
• Dependencies: std, tusk-model (minimal subset)

---

## LAYER 2: LANGUAGE TOOLING

### syn (Lossless OCaml parser)

• Purpose: Concrete Syntax Tree parser preserving ALL tokens (whitespace, comments) for refactoring tools
• Architecture: Lexer → Token stream → Parser → CST
• Key modules:
 • Lexer: Tokenization (keyword detection, operators, literals)
 • Parser: Recursive descent parser producing CST with extension syntax support
 • Token: Token representation with span information
 • Token_cursor: Stream cursor for parser
 • Cursor: Character cursor for lexer
 • Syntax_kind: AST node kinds (Expr, Pattern, Type, Module, etc.)
 • Diagnostic: Structured error reporting with spans
 • Diagnostic_reporter: Pretty-printing diagnostics with source context
 • Error: Error types
 • Keyword: OCaml keyword recognition
 • Ceibo: Span tracking for error reporting
• Extension syntax: Support for PPX-style extensions (`let%foo`, `and%bar`, etc.)
 • parse_extension_name: Parses extension names after keywords, before attributes
• Output: CST (not AST) - preserves formatting for code transformation
• Use cases: IDE refactoring, linters, formatters, code generators
• Dependencies: std

### raml (RAML compiler - OCaml→Native)

• Purpose: Riot Advanced Meta Language - alternative OCaml→native compiler
• Architecture: OCaml AST → Lambda IR → Backend (native/WASM/bytecode)
• Components:
 • lambda/: Lambda intermediate representation
 • typechecker/: Type inference and checking
 • backends/: Code generation (native, WASM targets)
• Dependencies: std, syn
• Status: Experimental compiler backend

### tusk-fix (Linter + auto-fixer)

• Purpose: Pipeline-based linter with automatic fixes (like clippy/eslint --fix)
• Architecture: syn CST → Lint rules → Diagnostics + Fix suggestions → Apply fixes
• Lint rules: Unused variables, deprecated APIs, style violations, type errors
• Dependencies: std, syn
• Status: Active development

### tusk-fmt (Code formatter)

• Purpose: Opinionated OCaml formatter (like rustfmt/prettier)
• Status: Stub implementation
• Dependencies: std, syn

### macro (Compile-time code generation)

• Purpose: Macro system for DSL embedding (like Rust procedural macros)
• Mechanism: Compile-time expansion of embedded DSLs in OCaml code
• Use cases: Derive implementations, inline SQL, regex compilation, configuration DSLs
• Dependencies: std, syn

---

## LAYER 3: PARSERS & DATA FORMATS

### markdown (Markdown parser + renderer)

• Purpose: CommonMark-compliant markdown parsing and HTML/terminal rendering
• Features: Spec tests for compliance, extensions (tables, strikethrough, task lists)
• Output: AST, HTML, ANSI-styled terminal output
• Dependencies: std

### protobuf (Protocol Buffers)

• Purpose: protobuf serialization/deserialization for OCaml
• Components:
 • Protobuf: Main module
 • wireFormat: Binary wire format encoding/decoding
 • protofileFormat: .proto file parsing
 • debugFormat: Debug printing
• Files: WIRE_FORMAT.md, proto3_2024 EBNF grammar
• Dependencies: std

### email (RFC-compliant email parsing)

• Purpose: Parse/generate email messages per IETF RFCs
• RFCs implemented:
 • 2045: MIME Part One (format)
 • 2047: MIME Part Three (header encoding)
 • 2231: MIME parameter encoding
 • 3501: IMAP4rev1
 • 4155: Mbox format
 • 5322: Internet Message Format
 • 6532: Internationalized email headers
 • 6854: Update to 5322
• Key modules:
 • Address: Email address parsing (local-part, domain, display-name)
 • Message: Email message structure (headers, body, MIME multipart)
 • Encoding: Transfer encodings (7bit, 8bit, binary, quoted-printable, base64)
 • Mbox: Unix mbox format parsing/writing
 • Query: Email querying/filtering
• Dependencies: std

### datalog (Datalog engine)

• Purpose: Relational query engine for logic programming
• Key module: Parser (datalog query parsing)
• Use cases: Build system queries, dependency analysis, graph queries
• Dependencies: std

### poneglyph (EAV graph database)

• Purpose: Entity-Attribute-Value in-memory graph store for build metadata and semantic code information
• Schema: EAV triples (entity, attribute, value) with temporal/versioning support
• Backends:
 • InMemory: Fast in-memory storage for development and testing
 • LSM: Log-Structured Merge-tree for persistent storage
 • Note: Simple_file backend removed for simplified API surface
• Use cases: Build graph storage, code structure indexing, incremental analysis
• Queries: Pattern matching over EAV triples
• Dependencies: std
• Status: Active workspace member

---

## LAYER 4: NETWORKING & PROTOCOLS

### http (HTTP protocol implementation)

• Purpose: HTTP/1.1, HTTP/2, HTTP/3, WebSocket protocol support
• Modules:
 • Http1: HTTP/1.1 (request, response, chunk encoding, SSE)
 • Http2: HTTP/2 (frames, streams, HPACK)
 • Http3: HTTP/3 over QUIC
 • Ws: WebSocket (RFC 6455)
• http1 components:
 • Request: Parse/serialize HTTP requests
 • Response: Parse/serialize HTTP responses
 • Chunk: Chunked transfer encoding
 • Common: Shared utilities
 • Sse: Server-Sent Events
• RFCs: 7231 (HTTP/1.1), 9113 (HTTP/2), 9114 (HTTP/3)
• Dependencies: std

### blink (Streaming HTTP client)

• Purpose: Lightweight HTTP client with incremental response processing built on miniriot actors
• Architecture: One actor per connection, streaming responses via messages
• Key modules:
 • Connection: HTTP/1.1 connection management (connect, request, stream, messages, await, close)
 • WebSocket: WebSocket client
• Message types:
 • StatusLine: Initial HTTP response
 • Headers: Response headers
 • BodyChunk: Incremental body data
 • Complete: Response complete
 • Error: Connection/protocol errors
• API levels:
 • Low-level: stream() - manual message pulling
 • Mid-level: messages(~on_message) - batch processing with callback
 • High-level: await(~on_message) - blocks until complete response
• Features: Chunked transfer encoding, progress monitoring, streaming uploads/downloads
• Dependencies: std, http

### jsonrpc (JSON-RPC 2.0)

• Purpose: JSON-RPC client + server implementation
• Modules:
 • Client: Send requests, handle responses
 • Server: Register methods, dispatch requests
 • Common: Shared types (Request, Response, Error, Notification)
• Spec: JSON-RPC 2.0 (spec_v2.0.html included)
• Dependencies: std

### grpc (gRPC implementation)

• Purpose: gRPC client/server for OCaml
• Protocol: HTTP/2 + protobuf
• Dependencies: std, http, protobuf

### suri (Web framework)

• Purpose: High-performance web framework for HTTP servers, WebSocket servers, real-time web apps
• Architecture: Built on std + miniriot for concurrent request handling
• Key modules:
 • SocketPool: TCP connection pool with concurrent acceptors, protocol switching
 • WebServer: HTTP/1.1 server with request parsing, response handling, routing
 • Middleware: Composable middleware (logging, CORS, compression, auth)
 • Channel: WebSocket handler abstraction for real-time bidirectional communication
 • Component: React-style type-safe UI component system (115+ HTML5 elements)
  • API convention: Tags without `_` (title, style, script), attributes with `_` (title_, style_, class_)
  • script and style tags take string directly (not `[text ...]` arrays)
 • Conn: HTTP connection handling with enhanced utilities
  • query_params: Parse URL query strings with percent decoding
  • render_text: Plain text response helper
  • redirect: 302 redirect with optional headers
  • request: Access to raw HTTP request object
 • LiveView: Server-rendered components with live DOM updates over WebSocket (Phoenix LiveView inspired)
  • Args serialization: Session tokens passed via `data-lv-session` attribute
  • JavaScript client reads session and appends as `?session=<token>` query parameter
  • Enables stateful LiveView components with secure session management
 • Session: Session management for LiveView with token generation and validation
• Features: Actor-per-connection, middleware pipeline, WebSocket channels, server-side rendering, type-safe components, LiveView real-time updates
• Dependencies: std, miniriot, http

### mcp (Model Context Protocol)

• Purpose: MCP client + server for AI tool integration
• Protocol: JSON-RPC-based protocol for AI assistants
• Dependencies: std, jsonrpc

---

## LAYER 5: DATABASE DRIVERS

### sqlite (SQLite bindings)

• Purpose: SQLite3 FFI bindings for embedded database
• Features: Prepared statements, transactions, parameter binding
• Dependencies: std

### postgres (PostgreSQL client)

• Purpose: PostgreSQL wire protocol client (no libpq dependency)
• Protocol: Pure OCaml implementation of PostgreSQL protocol
• Key modules:
 • Protocol.Sqlstate: 100+ typed SQLSTATE error codes (success, warning, error classes)
 • Protocol.Error: Structured error type with all PostgreSQL error fields
  • severity, code (sqlstate), message, detail, hint, position, internal_position, internal_query, where, schema, table, column, datatype, constraint
• Error handling: Replaced string errors with typed variants for better pattern matching
• Dependencies: std

### sqlx (Type-safe SQL)

• Purpose: Compile-time checked SQL queries with type inference
• Features: SQL parsing, type checking against schema, code generation
• Architecture: Macro-based, validates queries at compile time
• Dependencies: std, macro, sqlx-driver

### sqlx-driver (Database driver abstraction)

• Purpose: Unified interface for multiple databases (SQLite, PostgreSQL, MySQL)
• Drivers: Plugin architecture for database backends
• Error handling:
 • Sqlx_driver.Error: Database-agnostic structured error type
 • Pool errors: Exhausted (no connections), ConnectionError (failed to connect), Timeout (acquire timeout) with contextual information
 • Better error propagation: Errors include connection pool state, retry counts, underlying database errors
• Dependencies: std

### sqltool (SQL CLI)

• Purpose: Interactive SQL shell (like psql/sqlite3)
• Dependencies: std, sqlx

### codedb (Code database)

• Purpose: Database for storing and querying code structure and metadata
• Use cases: IDE features, code search, refactoring tools
• Dependencies: std

---

## LAYER 6: TERMINAL UI

### tty (Terminal control library)

• Purpose: Low-level terminal manipulation (ANSI, cursor, input, styling, raw mode)
• Features:
 • ANSI escape codes (colors, cursor movement, screen clearing)
 • Raw mode (non-canonical input)
 • Terminal size detection
 • Mouse input
 • Signal handling (SIGWINCH)
• Native bindings: FFI to termios/ioctl for raw mode
• Dependencies: std, kernel

### colors (Color science library)

• Purpose: Perceptually uniform color blending and color space conversions
• Problem: Naive RGB blending produces muddy colors (purple + yellow = gray instead of red)
• Solution: Convert to perceptual color space (LUV) for blending
• Color spaces:
 • RGB: Standard RGB (0-255)
 • LinearRGB: Gamma-corrected RGB for math
 • XYZ: CIE 1931 color space (device-independent)
 • LUV: CIE LUV (perceptually uniform)
 • ANSI: Terminal 256-color palette
• Pipeline: RGB → LinearRGB → XYZ → LUV (for blending) → XYZ → LinearRGB → RGB
• White point: D65 reference for color accuracy
• Use cases: Terminal gradients, smooth color interpolation, color picking
• Dependencies: std

### gooey (UI primitives)

• Purpose: Layout engine and UI components for terminal applications
• Key modules:
 • Element: Abstract UI element type
 • Layout: Flexbox-like layout engine (row, column, alignment, spacing)
 • Geometry: Rect, Point, Size
 • Style: Styling (colors, borders, padding, margin)
 • Render: Rendering to ANSI output
 • Viewport: Scrollable regions
 • AnsiFormatter: ANSI code generation
 • Config: Terminal configuration
 • TerminalRenderer: Fullscreen/inline rendering modes
• Dependencies: std, tty, colors

### minttea (TUI framework - Elm architecture)

• Purpose: High-level TUI framework with Model-View-Update (Elm) architecture
• Architecture:
 • Model: Application state
 • View: Render model to UI
 • Update: Handle events, return new model + commands
 • Commands: Side effects (HTTP, timers, etc.)
• Key modules:
 • App: Application definition (make ~init ~update ~view)
 • Event: Terminal events (KeyPress, MouseClick, Resize, Tick)
 • Command: Command type for side effects
 • Config: Application config (render_mode: Clear|Persist, output_target: Stdout|Stderr)
 • Program: Event loop runner
 • Renderer: ANSI rendering
 • IoLoop: Terminal input/output loop
 • AnsiParser: Parse ANSI input sequences
• Components (prebuilt widgets):
 • Cursor: Blinking cursor
 • Forms: Input forms
 • Fps: FPS counter
 • Listbox: Selectable list
 • Paginator: Pagination
 • Progress: Progress bar
 • Spinner: Loading spinner
 • Sprite: Animated sprites
 • Table: Data table
 • Textarea: Multi-line input
 • Textinput: Single-line input
 • Viewport: Scrollable content
• Style: Border styles, gradients, formatters
• Event types:
 • Key events with modifiers (Ctrl, Alt, Shift)
 • Mouse events (click, drag, scroll)
 • Resize events
 • Custom timer ticks
• Dependencies: std, tty, colors, gooey

---

## LAYER 7: UTILITIES

### pubgrub (Version solver)

• Purpose: Dependency resolution using PubGrub algorithm (Dart's pub solver)
• Algorithm: SAT-based version solving with conflict-driven clause learning
• Key modules:
 • Version: Semantic version parsing/comparison
 • Ranges: Version range operations (intersection, union, complement)
 • Term: Positive/negative package constraints
 • Incompatibility: Conflict clauses (external causes, derived causes)
 • Partial_solution: Current solver state with decisions and derivations
 • Provider: Package/dependency provider interface (offline mode for testing)
 • New_solver: Main solver algorithm
• Types:
 • version: SemVer (major.minor.patch)
 • ranges: Version ranges (bounds: Unbounded | Included | Excluded)
 • package: String identifier
 • solve_result: Success of (package * version) list | Failure of incompatibility
• Offline mode: Preload packages for deterministic testing
• Use cases: tusk dependency resolution, package managers
• Dependencies: std

### mime (MIME type handling)

• Purpose: MIME type parsing, detection, database lookup
• Features: File extension → MIME type, MIME type parsing
• Dependencies: std

### lol (Little utilities)

• Purpose: Small CLI utilities for testing/debugging
• Contents: Miscellaneous throwaway tools
• Dependencies: std

### propane (Property-based testing)

• Purpose: Property-based testing framework inspired by PropEr (Erlang) and QuickCheck
• Key modules:
 • Generator: Random value generation for all standard types
 • Arbitrary: Built-in generators (int, string, list, option, result, etc.)
 • Shrinker: Intelligent shrinking to find minimal counter-examples
 • Property: Property definition and testing (property, assume, for_all)
 • Printer: Pretty-printing test failures
• Features: Automatic test case generation, shrinking on failure, assumption-based testing, custom generators
• Use cases: Testing invariants, finding edge cases, regression testing
• Dependencies: std

### ceibo (Unknown)

• Purpose: Not documented
• Status: Active workspace member
• Dependencies: std

### hello-foreign (FFI example)

• Purpose: Demonstration of calling Rust/C from OCaml
• Shows: FFI binding generation, cross-language types, calling conventions
• Dependencies: std, kernel

---

## NATIVE COMPONENTS (Rust - /native)

### raml-rt (RAML runtime in Rust)

• Purpose: OCaml runtime reimplementation in Rust for RAML compiler
• Components:
 • runtime/: GC, memory management, fiber scheduler, bytecode interpreter, primitives, marshal
 • native/: C API compatibility layer
 • value.rs: OCaml value representation
• Crate type: cdylib (C-compatible shared library) + rlib (Rust library)
• Targets: Native + WASM (wasm32 with wasm-bindgen)
• Examples: load_cmo (load OCaml bytecode), test_bytecode (interpreter tests)
• Dependencies: raml-core, wasm-bindgen, web-sys

### raml-core

• Purpose: Core RAML compiler types shared between OCaml and Rust
• Dependencies: Rust stdlib

### raml-derive

• Purpose: Procedural macros for RAML code generation
• Dependencies: syn (Rust parser), quote

### raml-ffi

• Purpose: FFI utilities and bindings
• Examples: Multiple FFI examples showing cross-language calls

### raml-kernel

• Purpose: Low-level kernel primitives in Rust

### raml-bindgen

• Purpose: Automatic binding generation between OCaml and Rust
• Dependencies: raml-core

### example-lib

• Purpose: Example Rust library with OCaml bindings

### hello-rust

• Purpose: Simple Rust "hello world" with OCaml interop

---

## BUILD SYSTEM MENTAL MODEL

1. Discovery phase (workspace-level):
 • Load workspace.toml
 • Discover all member packages
 • Parse all tusk.toml files
 • Build package dependency graph
2. Planning phase (package-level):
 • For each package in dependency order:
  • Scan filesystem for .ml/.mli files
  • Run ocamldep to get module dependencies
  • Build module graph (Module_graph)
  • Convert to action graph (Action_graph)
  • Compute content hash (Merkle tree from sources + dep hashes)
  • Check cache in tusk-store

3. Execution phase (parallel):
 • WorkerPool with N workers
 • Distribute actions respecting dependencies
 • Execute: ocamlc/ocamlopt with appropriate flags
 • Cache successful outputs
 • Aggregate results
4. Incremental builds:
 • Hash-based: Recompile only if source/dep hashes changed
 • Content-addressable cache: Artifacts shared across branches/machines
 • Action graph diffing: Minimal rebuild set


---

## TYPICAL WORKFLOWS

### Development workflow:

# Build entire workspace
tusk build

# Build specific package
tusk build --package myapp

# Run binary
tusk run myapp

# Run tests
tusk test

# Watch mode (rebuild on file change)
tusk build --watch

# Format code
tusk fmt

# Lint and fix
tusk fix

# Start REPL with project loaded
tusk repl

### Editor integration:

# Start LSP-like build server
tusk server start

# Editor connects via JSON-RPC to get:
# - Incremental compilation
# - Diagnostics
# - Build graph queries
# - Fast rebuilds

### CI/CD:

# Release build
tusk build --release

# All tests
tusk test --all

# Cache populated for incremental builds

---

## PACKAGE DEPENDENCY GRAPH (KEY PATHS)

kernel (C FFI, event loop)
  ↓
miniriot (actor runtime)
  ↓
std (stdlib) ← [35 packages depend on this]
  ↓
  ├→ tty → colors → gooey → minttea (TUI stack)
  ├→ http → {blink, suri, grpc} (networking stack)
  ├→ {sqlite, postgres} → sqlx_driver → sqlx (database stack)
  ├→ syn → {raml, tusk-fix} (language tools)
  ├→ tusk-model → tusk-planner → tusk-executor → tusk-server → tusk-cli (build system)
  └→ {email, markdown, protobuf, pubgrub, mime, ...} (libraries)

---

## CODE ORGANIZATION PATTERNS

### Common Design Patterns

#### Config Module Aliasing
When a package defines its own Config module that conflicts with Std.Config:
```ocaml
open Std

(* Alias Std.Config to avoid conflicts *)
module Conf = Config

(* Now define package-specific Config *)
module Config = struct
  type t = { ... }
end

(* Access Std.Config via Conf alias *)
let load_config () = Conf.Loader.load ~env:"dev" "config.toml"
```

#### Error Type Design
Prefer structured variant types over strings for better error handling:
```ocaml
(* Bad: string errors *)
type result = (value, string) Result.t

(* Good: structured errors *)
type error =
  | NotFound of { resource : string }
  | PermissionDenied of { path : string; user : string }
  | Timeout of { duration : Duration.t }
  | DatabaseError of { code : Sqlstate.t; message : string }

type result = (value, error) Result.t
```

#### Component Naming (Suri)
HTML elements use consistent naming for tags vs attributes:
```ocaml
(* Tags: no underscore (actual HTML elements) *)
let page = div [
  title [ text "Page Title" ];      (* <title> element *)
  style "body { color: red; }";     (* <style> element with string *)
  script "console.log('hi')";       (* <script> element with string *)
]

(* Attributes: with underscore (HTML attributes) *)
let elem = div ~title_:"tooltip" ~class_:"container" [
  text "content"
]
```

### Module structure:

package/
  src/
    package.ml      # Main module
    package.mli     # Public interface
    module1.ml      # Sub-module implementation
    module1.mli     # Sub-module interface
    subdir/
      module2.ml
      module2.mli
  examples/
    example1.ml     # Compiled as binary
  tests/
    test1.ml        # Compiled and run as test
  tusk.toml         # Package manifest
  README.md         # Documentation

### tusk.toml structure:

[package]
name = "mypackage"
version = "0.1.0"

[lib]
path = "src/mypackage.ml"

[[bin]]
name = "mybinary"
path = "src/main.ml"

[tests.mytest]
path = "tests/mytest.ml"

[dependencies]
std = { path = "../std" }
http = { path = "../http" }

[target.macos]
cc_flags = ["-framework", "CoreFoundation"]

[profile.release]
optimization = 3
debug_info = false

[profile.debug]
ocamlc_flags = ["-O0"]
debug_info = true

[workspace]
rust_target_dir = "native/target"

---

## SUMMARY STATISTICS

• Total packages: 57+ active workspace members
• Core runtime: 3 packages (kernel, miniriot, std)
• Build system: 12 packages (tusk-*)
• Language tools: 5 packages (syn, raml, tusk-fix, tusk-fmt, macro)
• Networking: 6 packages (http, blink, jsonrpc, grpc, suri, mcp)
• Database: 6 packages (sqlite, postgres, sqlx, sqlx-driver, sqltool, codedb)
• UI: 4 packages (tty, colors, gooey, minttea)
• Parsers: 5 packages (markdown, protobuf, email, datalog, poneglyph)
• Testing: 1 package (propane)
• Utilities: 5 packages (pubgrub, mime, lol, ceibo, swisstable)
• Examples: 1 package (hello-foreign)
• Native (Rust): 9 crates (raml-*)

Recent additions: gooey (UI primitives), propane (property testing), poneglyph (EAV database), swisstable (hash tables), codedb (code database), hello-foreign (FFI examples)

Package naming: Use kebab-case in workspace (e.g., `sqlx-driver`), underscores in OCaml module names (e.g., `Sqlx_driver`)

Total lines of code: Estimated ~100k+ lines (2513+ source files)

---

This taxonomy provides complete context for understanding the RIOT codebase architecture, dependencies, and development patterns. All packages follow actor-model concurrency via miniriot, use std as universal
stdlib, and integrate into the tusk build system.
