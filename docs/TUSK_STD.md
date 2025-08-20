# Tusk Standard Library Design

## Overview

A thin, efficient standard library for Tusk that eliminates dependencies on external system commands by providing native OCaml implementations or bindings to robust C/Rust libraries.

## Problem Statement

Currently, the Tusk codebase relies heavily on external system commands:
- **File operations**: `cp`, `mv`, `rm`, `mkdir`, `chmod`, `find`
- **Archive operations**: `tar`, `zip`
- **Network operations**: `curl`, `wget`
- **Cryptography**: `shasum`, `md5sum`
- **Text processing**: `grep`, `sed`, `awk`, `cut`
- **System info**: `uname`

This creates several problems:
1. **Portability**: Commands vary between GNU/BSD/macOS
2. **Performance**: Spawning processes is expensive
3. **Error handling**: Parsing command output is fragile
4. **Security**: Command injection risks
5. **Windows support**: Most commands don't exist on Windows

## Architecture

```
packages/std/
├── src/
│   ├── crypto.ml       # Cryptographic functions
│   ├── crypto.mli
│   ├── fs.ml          # Filesystem operations
│   ├── fs.mli
│   ├── archive.ml     # Tar/zip operations
│   ├── archive.mli
│   ├── net.ml         # HTTP client
│   ├── net.mli
│   ├── process.ml     # Process management
│   ├── process.mli
│   ├── sys_info.ml    # System information
│   ├── sys_info.mli
│   └── lib.ml         # Main entry point
├── vendor/
│   ├── sha256/        # C implementation
│   └── blake3/        # Rust implementation
└── dune
```

## Module Designs

### 1. Crypto Module

Provides fast cryptographic hash functions without external dependencies.

```ocaml
module Crypto : sig
  module SHA256 : sig
    type t
    
    val string : string -> string
    (** Hash a string, returns hex digest *)
    
    val file : string -> string
    (** Hash a file efficiently, returns hex digest *)
    
    val stream : in_channel -> string
    (** Hash from input channel *)
    
    type context
    val init : unit -> context
    val update : context -> bytes -> int -> int -> unit
    val final : context -> string
  end
  
  module SHA512 : sig
    (* Similar interface *)
  end
  
  module MD5 : sig
    (* Similar interface - for legacy compatibility only *)
  end
  
  module Blake3 : sig
    (* Modern, fast hash function *)
    val hash : string -> string
    val hash_file : string -> string
  end
end
```

**Implementation Strategy**:
- Bind to small C implementations (like from OpenBSD)
- Or use OCaml implementations from existing libraries
- Blake3 could use Rust binding for maximum performance

### 2. Filesystem Module

Replace all `cp`, `mv`, `rm`, `find` commands with native operations.

```ocaml
module Fs : sig
  (** File operations *)
  val copy : src:string -> dest:string -> unit
  val copy_recursive : src:string -> dest:string -> unit
  val move : src:string -> dest:string -> unit
  val remove : string -> unit
  val remove_recursive : string -> unit
  
  (** Directory operations *)
  val mkdir : ?parents:bool -> ?mode:int -> string -> unit
  val mkdtemp : ?prefix:string -> unit -> string
  val rmdir : string -> unit
  
  (** Permissions *)
  val chmod : mode:int -> string -> unit
  val chown : uid:int -> gid:int -> string -> unit
  
  (** Finding files *)
  type find_options = {
    name : string option;        (* -name pattern *)
    type_filter : [`File | `Dir | `Any];
    max_depth : int option;
    follow_symlinks : bool;
  }
  
  val find : ?options:find_options -> string -> string list
  val glob : pattern:string -> string -> string list
  
  (** File info *)
  val exists : string -> bool
  val is_file : string -> bool
  val is_directory : string -> bool
  val file_size : string -> int64
  val mtime : string -> float
  
  (** Reading/Writing *)
  val read_file : string -> string
  val write_file : string -> string -> unit
  val read_lines : string -> string list
  
  (** Atomic operations *)
  val with_temp_file : 
    ?dir:string -> 
    ?prefix:string -> 
    (string -> out_channel -> 'a) -> 'a
  
  val atomic_write : string -> (out_channel -> unit) -> unit
  (** Write to temp file and atomically rename *)
end
```

**Implementation Strategy**:
- Use Unix module for basic operations
- Implement recursive operations carefully
- Use Sys.readdir for directory traversal
- Atomic writes using temp file + rename

### 3. Archive Module

Handle tar and zip files without external commands.

```ocaml
module Archive : sig
  module Tar : sig
    type entry = {
      path : string;
      size : int64;
      mode : int;
      mtime : float;
      content : [`File of string | `Directory | `Symlink of string];
    }
    
    val create : output:string -> string list -> unit
    val extract : archive:string -> dest:string -> unit
    val list : archive:string -> entry list
    
    (** Streaming API *)
    val create_stream : out_channel -> (entry -> unit) -> unit
    val extract_stream : in_channel -> (entry -> unit) -> unit
  end
  
  module Gzip : sig
    val compress : string -> string
    val decompress : string -> string
    val compress_file : src:string -> dest:string -> unit
    val decompress_file : src:string -> dest:string -> unit
  end
  
  (** High-level convenience *)
  val extract_tar_gz : archive:string -> dest:string -> unit
  val create_tar_gz : output:string -> string list -> unit
end
```

**Implementation Strategy**:
- Use ocaml-tar library or implement basic tar format
- Bind to zlib for gzip support
- Consider using Rust's tar crate for robustness

### 4. Network Module

HTTP client without curl/wget dependencies.

```ocaml
module Net : sig
  module Http : sig
    type response = {
      status : int;
      headers : (string * string) list;
      body : string;
    }
    
    type request_options = {
      headers : (string * string) list;
      timeout_ms : int option;
      follow_redirects : bool;
      max_redirects : int;
    }
    
    val get : ?options:request_options -> string -> response
    val post : ?options:request_options -> string -> body:string -> response
    val download : url:string -> dest:string -> unit
    
    (** Streaming download with progress *)
    val download_with_progress : 
      url:string -> 
      dest:string -> 
      progress:(int64 -> int64 -> unit) -> 
      unit
  end
  
  module Url : sig
    type t = {
      scheme : string;
      host : string;
      port : int option;
      path : string;
      query : (string * string) list;
      fragment : string option;
    }
    
    val parse : string -> t
    val to_string : t -> string
  end
end
```

**Implementation Strategy**:
- Use cohttp or ocaml-curl bindings
- Or implement basic HTTP/1.1 client
- Support HTTPS via OpenSSL bindings

### 5. System Information Module

Get system info without uname and similar commands.

```ocaml
module Sys_info : sig
  type os = Linux | MacOS | FreeBSD | Windows | Other of string
  type arch = X86_64 | Aarch64 | Arm | X86 | Other of string
  
  val os : unit -> os
  val arch : unit -> arch
  val hostname : unit -> string
  val cpu_count : unit -> int
  val total_memory : unit -> int64
  val available_memory : unit -> int64
  
  (** Platform-specific paths *)
  val home_dir : unit -> string
  val temp_dir : unit -> string
  val config_dir : unit -> string  (* ~/.config on Unix *)
  val cache_dir : unit -> string   (* ~/.cache on Unix *)
  
  (** Host triplet for downloads *)
  val host_triplet : unit -> string
  (* Returns like "x86_64-apple-darwin" *)
end
```

**Implementation Strategy**:
- Use Sys and Unix modules
- Parse /proc on Linux
- Use sysctl on BSD/macOS
- Compile-time detection for some values

### 6. Process Module

Better process management than Unix.system.

```ocaml
module Process : sig
  type t
  
  type spawn_options = {
    env : (string * string) list option;
    cwd : string option;
    stdin : [`Pipe | `Null | `Inherit];
    stdout : [`Pipe | `Null | `Inherit];
    stderr : [`Pipe | `Null | `Inherit | `Stdout];
  }
  
  val spawn : 
    ?options:spawn_options -> 
    string -> 
    string array -> 
    t
  
  val wait : t -> Unix.process_status
  val kill : t -> signal:int -> unit
  val pid : t -> int
  
  (** High-level interface *)
  val run : 
    ?env:(string * string) list ->
    ?cwd:string ->
    string -> 
    string array -> 
    (int * string * string)
  (** Returns (exit_code, stdout, stderr) *)
  
  (** Shell-like command parsing *)
  val shell : string -> (int * string * string)
  (** Parse and run shell-like command *)
  
  (** Pipes *)
  val pipe : string list -> (int * string * string)
  (** Run pipeline of commands *)
end
```

## Implementation Priorities

### Phase 1: Core Operations (Week 1)
1. **Fs.copy** - Replace all `cp` commands
2. **Fs.remove_recursive** - Replace `rm -rf`
3. **Fs.mkdir** - Replace `mkdir -p`
4. **Crypto.SHA256** - Replace `shasum`

### Phase 2: Archive & Network (Week 2)
1. **Archive.Tar** - Replace `tar` commands
2. **Net.Http.download** - Replace `curl`/`wget`
3. **Archive.Gzip** - Handle .tar.gz files

### Phase 3: System & Process (Week 3)
1. **Sys_info** - Replace `uname` commands
2. **Process.run** - Better than Unix.system
3. **Fs.find** - Replace `find` commands

### Phase 4: Advanced Features (Week 4)
1. **Fs.atomic_write** - Safe file updates
2. **Net.Http.download_with_progress** - Better UX
3. **Crypto.Blake3** - Modern, fast hashing

## Usage Examples

### Before (Current Code)
```ocaml
(* Copying files *)
let cmd = Printf.sprintf "cp %s %s" src dest in
ignore (System.system cmd);

(* Finding files *)
let find_cmd = 
  Printf.sprintf "find %s -name '*.ml' -o -name '*.mli'" dir in
let ic = System.open_process_in find_cmd in
(* ... parse output ... *)

(* Hashing *)
let cmd = 
  Printf.sprintf "shasum -a 256 '%s' | cut -d' ' -f1" file in
(* ... run and parse ... *)

(* Downloading *)
let cmd = Printf.sprintf "curl -L -o %s %s" dest url in
System.run_command cmd
```

### After (With Std Library)
```ocaml
open Std

(* Copying files *)
Fs.copy ~src ~dest;

(* Finding files *)
let ml_files = 
  Fs.find dir ~options:{
    name = Some "*.ml";
    type_filter = `File;
    max_depth = None;
    follow_symlinks = false;
  }

(* Hashing *)
let hash = Crypto.SHA256.file filepath;

(* Downloading *)
Net.Http.download ~url ~dest;
```

## Testing Strategy

### Unit Tests
- Test each function in isolation
- Mock filesystem for Fs tests
- Mock network for Net tests

### Integration Tests
- Test against real filesystem
- Test with real tar files
- Test with real HTTP servers

### Cross-Platform Tests
- CI on Linux, macOS, Windows
- Test with different architectures
- Verify command compatibility

### Performance Tests
- Benchmark vs shell commands
- Memory usage profiling
- Large file handling

## Migration Plan

1. **Implement Std library** as separate package
2. **Add as dependency** to Tusk
3. **Gradually replace** System.system calls
4. **Remove shell command dependencies**
5. **Eventually move to riot/std** for wider use

## Benefits

1. **Cross-platform**: Works on Windows without WSL
2. **Faster**: No process spawning overhead
3. **Safer**: No command injection risks
4. **Robust**: Proper error handling
5. **Type-safe**: OCaml types instead of string parsing
6. **Debuggable**: Can step through code
7. **Predictable**: Same behavior everywhere

## Future Extensions

- **Compression**: Support for bzip2, xz, zstd
- **Crypto**: AES encryption, digital signatures
- **Parallel**: Parallel file operations
- **Watch**: File system watching
- **Locks**: File locking primitives
- **Memory mapping**: Efficient large file handling