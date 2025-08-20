# Tusk Format System Design

## Overview

The Tusk format system provides fast, incremental code formatting for OCaml projects with support for multiple interfaces (CLI, RPC, MCP) and concurrent execution. It integrates with ocamlformat while providing intelligent caching and error reporting.

## Architecture

### Core Components

```
┌─────────────────┐
│   CLI Client    │──┐
├─────────────────┤  │    ┌──────────────────┐     ┌─────────────────┐
│   RPC Client    │──┼───▶│  Format Manager  │────▶│  Format Worker  │
├─────────────────┤  │    │                  │     │     Pool        │
│   MCP Client    │──┘    │  - Request Queue │     │                 │
└─────────────────┘       │  - Result Cache  │     │  - ocamlformat  │
                          │  - Error Collect │     │  - ocamlformat- │
                          └──────────────────┘     │      rpc        │
                                                   └─────────────────┘
```

### Key Design Principles

1. **Incremental Processing**: Only format files that have changed since last format
2. **Concurrent Execution**: Use worker pool to format multiple files in parallel
3. **Unified Interface**: Same core logic for CLI, RPC, and MCP
4. **Smart Caching**: Cache formatted content hashes to skip unchanged files
5. **Rich Error Reporting**: Collect and present errors in format appropriate for client

## Implementation Components

### 1. Format Manager (`format_manager.ml`)

The central coordinator that:
- Maintains a format cache (file path → content hash → formatted result)
- Manages the worker pool for concurrent formatting
- Collects and aggregates errors
- Provides unified API for all clients

```ocaml
module Format_manager : sig
  type t
  
  type format_request = 
    | File of string          (* Single file path *)
    | Files of string list    (* Multiple specific files *)
    | Directory of string     (* All OCaml files in directory *)
    | Workspace              (* All OCaml files in workspace *)
  
  type format_result = {
    path : string;
    status : [ `Success | `Skipped | `Failed of string ];
    time_ms : float;
  }
  
  type format_response = {
    results : format_result list;
    total : int;
    successful : int;
    failed : int;
    skipped : int;
    duration_ms : float;
  }
  
  val create : workspace:Workspace.t -> t
  val format : t -> format_request -> format_response
  val clear_cache : t -> unit
end
```

### 2. Format Worker (`format_worker.ml`)

Individual worker that handles formatting with pluggable backends:

```ocaml
module Format_worker : sig
  type t
  
  type backend = 
    | Tusk_formatter     (* Future: Our own zero-config formatter *)
    | Ocamlformat_rpc of Unix.file_descr  (* Legacy: RPC connection *)
    | Ocamlformat_binary of string        (* Legacy: Path to binary *)
  
  val create : backend -> t
  val format_file : t -> path:string -> (string, string) result
  val close : t -> unit
end
```

The worker abstraction allows us to swap formatters without changing the rest of the system. The future Tusk formatter will:
- Have zero configuration options (opinionated like `gofmt`)
- Be significantly faster (written in OCaml with formatting in mind)
- Not require `.ocamlformat` files
- Produce deterministic, canonical output

### 3. Format Cache (`format_cache.ml`)

A simple disk-based cache that tracks whether files are already formatted:

```ocaml
module Format_cache : sig
  type t
  
  (* Cache strategy:
     1. Read file content and compute hash
     2. Check if ./target/<profile>/fmt/<hash> exists
     3. If exists, file is already formatted (skip)
     4. If not, format the file
     5. After successful format, compute new hash
     6. If hash changed, file was reformatted
     7. If hash unchanged, create marker at ./target/<profile>/fmt/<hash>
  *)
  
  val create : profile:string -> t
  val is_formatted : t -> path:string -> content:string -> bool
  val mark_formatted : t -> path:string -> content_hash:string -> unit
  val clear : t -> unit
  
  (* Implementation detail: 
     Cache dir structure:
     ./target/debug/fmt/
       ├── manifest           (* List of hash -> file mappings for debugging *)
       ├── a3f4b5c6...       (* Empty marker files named by content hash *)
       ├── d7e8f9a0...
       └── ...
  *)
end
```

#### How the Cache Works

The cache is incredibly simple and cheap:

1. **Check Phase**: 
   - Read file content
   - Compute SHA256 hash
   - Check if `./target/<profile>/fmt/<hash>` exists
   - If it exists, the file with this exact content has been verified as formatted

2. **Format Phase** (if not cached):
   - Run ocamlformat on the file
   - Read the formatted content
   - Compute SHA256 of formatted content
   - If hash is same as input, create marker file at `./target/<profile>/fmt/<hash>`
   - If hash differs, the file was actually reformatted (don't cache)

3. **Invalidation**:
   - When formatter backend changes, clear entire cache
   - When formatter version changes, clear entire cache  
   - Manual clear with `tusk fmt --clear-cache`
   - (Legacy: When `.ocamlformat` changes, clear cache if using ocamlformat)

#### Why This Design?

- **No warm-up needed**: Cache persists across server restarts
- **Zero overhead**: Just checking file existence is extremely fast
- **Self-validating**: If file content changes, hash changes, cache miss occurs
- **Simple cleanup**: Just delete `./target/*/fmt/` directories
- **Debuggable**: The manifest file shows which files are cached

#### Comparison with Other Tools

Unlike rustfmt (which has no cache), our approach is similar to:
- **ESLint's cache**: Uses file hashes to skip unchanged files
- **Prettier's cache**: Similar hash-based approach
- **Black's cache**: Python formatter uses similar hash-based caching

The key insight is that if a file's content hasn't changed AND we've previously verified it's formatted, we can skip it entirely.

### 4. CLI Integration

The CLI provides a simple interface for formatting:

```bash
# Format all files in workspace
tusk fmt

# Format specific package
tusk fmt -p mypackage

# Format specific files
tusk fmt src/main.ml src/lib.ml

# Format with options
tusk fmt --check        # Check if files are formatted (exit 1 if not)
tusk fmt --diff         # Show diff instead of modifying files
tusk fmt --jobs 8       # Use 8 parallel workers
```

### 5. RPC Integration

The RPC interface provides structured requests and responses:

```json
// Request
{
  "method": "format",
  "params": {
    "paths": ["src/main.ml", "src/lib.ml"],  // optional, defaults to workspace
    "options": {
      "check": false,      // optional, default false
      "diff": false,       // optional, default false
      "jobs": 4           // optional, default to CPU count
    }
  }
}

// Response
{
  "result": {
    "results": [
      {
        "path": "src/main.ml",
        "status": "success",
        "time_ms": 45.2
      },
      {
        "path": "src/lib.ml", 
        "status": "failed",
        "error": "Syntax error on line 42",
        "time_ms": 12.1
      }
    ],
    "summary": {
      "total": 2,
      "successful": 1,
      "failed": 1,
      "skipped": 0,
      "duration_ms": 57.3
    }
  }
}
```

### 6. MCP Integration

The MCP interface provides tools for AI agents:

```typescript
// Tool definition
{
  "name": "tusk.formatFile",
  "description": "Format an OCaml source file",
  "inputSchema": {
    "type": "object",
    "properties": {
      "path": {
        "type": "string",
        "description": "Path to the OCaml file to format"
      }
    },
    "required": ["path"]
  }
}

// Additional tools
{
  "name": "tusk.formatPackage",
  "description": "Format all OCaml files in a package",
  "inputSchema": {
    "type": "object", 
    "properties": {
      "package": {
        "type": "string",
        "description": "Name of the package to format"
      }
    },
    "required": ["package"]
  }
}

{
  "name": "tusk.checkFormatting",
  "description": "Check if files are properly formatted without modifying them",
  "inputSchema": {
    "type": "object",
    "properties": {
      "paths": {
        "type": "array",
        "items": { "type": "string" },
        "description": "Paths to check (optional, defaults to workspace)"
      }
    }
  }
}
```

## Concurrent Execution Strategy

### Worker Pool Management

```ocaml
module Worker_pool = struct
  type t = {
    workers : Format_worker.t array;
    available : int Queue.t;  (* Queue of available worker indices *)
    mutable active : int;
  }
  
  let create ~size ~backend_factory =
    let workers = Array.init size (fun _ -> backend_factory ()) in
    let available = Queue.create () in
    for i = 0 to size - 1 do
      Queue.add i available
    done;
    { workers; available; active = 0 }
  
  let acquire pool =
    (* Block until worker available *)
    while Queue.is_empty pool.available do
      Scheduler.yield ()
    done;
    let idx = Queue.take pool.available in
    pool.active <- pool.active + 1;
    pool.workers.(idx)
  
  let release pool worker =
    (* Find worker index and return to available queue *)
    ...
end
```

### Parallel Formatting Pipeline

1. **File Discovery**: Find all OCaml files to format
2. **Cache Check**: Use content hash to check if already formatted
3. **Work Distribution**: Assign uncached files to available workers
4. **Format & Verify**: Format files and verify if they changed
5. **Cache Update**: Mark unchanged files as formatted in cache
6. **Result Collection**: Gather results as workers complete

```ocaml
let format_parallel manager request =
  let files = discover_files request in
  
  (* Check cache for each file *)
  let check_cached file =
    let content = File.read_string file in
    let hash = Sha256.string content in
    if Format_cache.is_formatted manager.cache ~path:file ~content then
      Some { path = file; status = `Skipped; time_ms = 0.0 }
    else
      None
  in
  
  (* Separate cached and uncached files *)
  let cached_results, needs_format =
    List.fold_left (fun (cached, uncached) file ->
      match check_cached file with
      | Some result -> (result :: cached, uncached)
      | None -> (cached, file :: uncached)
    ) ([], []) files
  in
  
  (* Create work queue for uncached files *)
  let work_queue = Queue.create () in
  List.iter (fun f -> Queue.add f work_queue) needs_format;
  
  (* Worker function *)
  let worker_loop () =
    let rec loop results =
      match Queue.take_opt work_queue with
      | None -> results
      | Some file ->
          let start_time = Unix.gettimeofday () in
          let original_content = File.read_string file in
          let original_hash = Sha256.string original_content in
          
          (* Format the file *)
          let result = Format_worker.format_file worker ~path:file in
          
          let status = match result with
          | Ok () ->
              (* Check if file actually changed *)
              let new_content = File.read_string file in
              let new_hash = Sha256.string new_content in
              if original_hash = new_hash then (
                (* File was already formatted, mark in cache *)
                Format_cache.mark_formatted manager.cache 
                  ~path:file ~content_hash:original_hash;
                `Skipped
              ) else
                `Success
          | Error msg -> `Failed msg
          in
          
          let time_ms = (Unix.gettimeofday () -. start_time) *. 1000.0 in
          let result = { path = file; status; time_ms } in
          loop (result :: results)
    in
    loop []
  in
  
  (* Spawn worker processes *)
  let workers = ref [] in
  for i = 1 to manager.worker_count do
    let worker_pid = Scheduler.spawn scheduler worker_loop in
    workers := worker_pid :: !workers
  done;
  
  (* Wait for all workers to complete *)
  let worker_results = List.map Process.wait !workers in
  let all_results = cached_results @ List.flatten worker_results in
  
  aggregate_results all_results
```

## Error Handling

### Error Types

```ocaml
type format_error =
  | Syntax_error of { file: string; line: int; col: int; message: string }
  | Config_error of { file: string; message: string }  
  | Io_error of { file: string; message: string }
  | Timeout of { file: string; duration_ms: float }
  | Worker_crash of { file: string; exception: string }
```

### Error Collection and Reporting

Errors are collected during formatting and presented appropriately:

- **CLI**: Human-readable error messages with file locations
- **RPC**: Structured JSON with error details
- **MCP**: Tool error responses with actionable information

## Configuration

### Formatter Configuration

#### Future: Tusk Formatter (Zero Config)
The future Tusk formatter will have **no configuration**:
- One canonical style for all OCaml code
- No `.ocamlformat` or config files needed
- Deterministic output (same input always produces same output)
- Fast and predictable

#### Legacy: OCamlformat Support
When using ocamlformat backend, the system follows its configuration discovery:
1. Look for `.ocamlformat` in the file's directory
2. Walk up directory tree looking for `.ocamlformat`
3. Use project root `.ocamlformat` as fallback
4. Error if no `.ocamlformat` found

The goal is to eventually deprecate ocamlformat support once the Tusk formatter is stable.

### Tusk Format Configuration

Configuration in `workspace.toml`:

```toml
[format]
# Formatter backend (default: "auto")
# Options: "tusk" | "ocamlformat" | "auto" (auto-detect best available)
backend = "auto"

# Number of parallel workers (default: CPU count)
workers = 8

# Timeout per file in milliseconds (default: 5000)
timeout_ms = 5000

# Excluded paths (gitignore syntax)
exclude = [
  "_build/**",
  "*.pp.ml",
  "vendor/**"
]

# Legacy ocamlformat options (ignored when backend = "tusk")
[format.ocamlformat]
use_rpc = true  # Use ocamlformat-rpc if available
```

## Performance Optimizations

### 1. Disk-Based Cache Performance
```ocaml
(* Cache implementation example *)
module Format_cache = struct
  type t = {
    cache_dir : string;
    mutable ocamlformat_version : string option;
  }
  
  let create ~profile =
    let cache_dir = Printf.sprintf "./target/%s/fmt" profile in
    System.mkdirp cache_dir;
    { cache_dir; ocamlformat_version = None }
  
  let is_formatted t ~path ~content =
    (* Quick hash computation *)
    let hash = Sha256.string content in
    let marker_path = Filename.concat t.cache_dir hash in
    (* Ultra-fast: just stat() syscall *)
    Sys.file_exists marker_path
  
  let mark_formatted t ~path ~content_hash =
    let marker_path = Filename.concat t.cache_dir content_hash in
    (* Create empty marker file *)
    let oc = open_out marker_path in
    close_out oc;
    (* Optionally update manifest for debugging *)
    let manifest_path = Filename.concat t.cache_dir "manifest" in
    let oc = open_out_gen [Open_append; Open_creat] 0o644 manifest_path in
    Printf.fprintf oc "%s %s\n" content_hash path;
    close_out oc
  
  let clear t =
    (* Simply remove all marker files *)
    let cmd = Printf.sprintf "rm -rf %s/*" t.cache_dir in
    ignore (Sys.command cmd)
end
```

**Cache Performance Characteristics:**
- **Lookup**: O(1) - Single stat() syscall
- **Storage**: ~100 bytes per file (just hash filename)
- **Memory**: Zero runtime memory (all disk-based)
- **Overhead**: <0.1ms per file check

### 2. RPC Connection Pooling
- Maintain persistent ocamlformat-rpc connections
- Reuse connections across multiple format operations
- Amortize connection startup cost across many files

### 3. Smart File Discovery
```ocaml
let discover_files_smart request =
  match request with
  | Workspace when git_available () ->
      (* Use git to find modified files for incremental formatting *)
      let modified = git_modified_files () in
      let untracked = git_untracked_files () in
      List.filter is_ocaml_file (modified @ untracked)
  | _ ->
      (* Fall back to filesystem traversal *)
      discover_files_recursive request
```

### 4. Parallel I/O Strategy
- Read files in parallel while workers are formatting
- Pipeline: Read → Hash → Check Cache → Queue for format
- Overlap I/O with CPU-intensive formatting

## Integration with Build System

The format manager integrates with the build system to:
- Auto-format before builds (optional)
- Invalidate build cache when files are formatted
- Report formatting errors as build warnings

```ocaml
(* In build_manager.ml *)
let pre_build_hooks = [
  (* Auto-format if configured *)
  (fun workspace ->
    if workspace.config.auto_format then
      Format_manager.format manager Workspace
  );
]
```

## Testing Strategy

### Unit Tests
- Test each component in isolation
- Mock ocamlformat binary/RPC for predictable testing
- Test error handling paths

### Integration Tests
- Test full formatting pipeline
- Test concurrent formatting with multiple workers
- Test cache invalidation and persistence

### Performance Tests
- Benchmark formatting large codebases
- Measure cache hit rates
- Profile worker pool efficiency

## Migration Path

### Phase 1: Core Implementation
1. Implement Format_manager with basic functionality
2. Add CLI support for `tusk fmt`
3. Use ocamlformat binary backend only

### Phase 2: RPC Integration
1. Add ocamlformat-rpc backend
2. Implement connection pooling
3. Add RPC interface for `tusk rpc format`

### Phase 3: Advanced Features
1. Add MCP tools for formatting
2. Implement caching system
3. Add worker pool for concurrency

### Phase 4: Optimizations
1. Add incremental formatting
2. Optimize file discovery
3. Add memory-mapped I/O

## Example Usage

### CLI Usage
```bash
# Format entire workspace
$ tusk fmt
🎨 Formatting 234 OCaml files...
✓ src/main.ml (45ms)
✓ src/lib.ml (32ms)
✗ src/broken.ml - Syntax error on line 42
...
✨ Formatted 232/234 files successfully in 2.3s

# Check formatting without modifying
$ tusk fmt --check
❌ 5 files need formatting:
  - src/main.ml
  - src/lib.ml
  - test/test_utils.ml
  - bin/cli.ml
  - bin/server.ml
```

### RPC Usage
```bash
# Format specific files via RPC
$ tusk rpc format src/main.ml src/lib.ml
{
  "results": [
    {"path": "src/main.ml", "status": "success", "time_ms": 45.2},
    {"path": "src/lib.ml", "status": "success", "time_ms": 32.1}
  ],
  "summary": {
    "total": 2,
    "successful": 2,
    "failed": 0,
    "skipped": 0,
    "duration_ms": 77.3
  }
}
```

### MCP Usage (from AI agent)
```typescript
// Format a single file
await mcp.call("tusk.formatFile", { path: "src/main.ml" });

// Check if package is formatted
const result = await mcp.call("tusk.checkFormatting", { 
  paths: ["packages/mylib/src"] 
});
if (!result.formatted) {
  await mcp.call("tusk.formatPackage", { package: "mylib" });
}
```

## Future: The Tusk Formatter

### Philosophy
The Tusk formatter will follow the Go philosophy: **no knobs, no options, one true style**.

### Design Principles
1. **Zero Configuration**: No config files, no options, no debates
2. **Speed First**: Built for performance from the ground up
3. **Deterministic**: Same input always produces identical output
4. **Opinionated**: Makes consistent choices, not necessarily popular ones
5. **AST-Preserving**: Never changes program semantics

### Key Differences from ocamlformat
- **No .ocamlformat files**: Style is hardcoded
- **Faster**: Target 10x faster than ocamlformat
- **Simpler**: No configuration parsing, no option handling
- **Consistent**: One style across all OCaml code using Tusk

### Implementation Strategy
```ocaml
module Tusk_formatter = struct
  (* The entire formatter configuration in one place *)
  let style = {
    indent_width = 2;
    margin = 90;
    break_cases = `Nested;
    break_infix = `Wrap;
    (* ... other style choices hardcoded ... *)
  }
  
  (* No configuration parsing needed *)
  let format_ast ast =
    Pretty_print.layout ~style ast
    |> Pretty_print.render
  
  (* Direct AST -> formatted string, no intermediate steps *)
  let format_file ~path =
    let ast = Parse.implementation path in
    format_ast ast
end
```

### Migration Path
1. **Phase 1**: Implement basic Tusk formatter with hardcoded style
2. **Phase 2**: Add to format backend options (opt-in)
3. **Phase 3**: Make Tusk formatter the default for new projects
4. **Phase 4**: Deprecate ocamlformat support

## Future Enhancements

1. **Diff Generation**: Show unified diffs of formatting changes
2. **Partial Formatting**: Format only specific regions of files  
3. **Format on Save**: IDE integration for automatic formatting
4. **Pre-commit Hooks**: Git hooks for format checking
5. **Distributed Formatting**: Format across multiple machines for large codebases
6. **AST-Aware Caching**: Cache at the AST level for finer-grained incremental formatting
7. **Format Migrations**: Tool to migrate from ocamlformat to Tusk formatter style