# Std - A Modern OCaml Standard Library

## Overview

A new standard library for OCaml that prioritizes ergonomics, cross-platform support, and modern development practices. Built to replace shell command dependencies and provide a delightful API that "just works" without the historical baggage of OCaml's current stdlib.

## Current Shell Dependencies Analysis

Based on codebase scan, we currently rely on these external commands:

### File Operations (Most Common)
- `cp`, `mv`, `rm`, `mkdir`, `chmod` - Basic file manipulation
- `find` - File discovery
- `tar`, `gzip` - Archive handling

### Network Operations
- `curl` - HTTP downloads (toolchain installation)
- URL downloads for package management

### Cryptography
- `shasum -a 256` - File integrity checking
- Hash verification for caching

### System Information
- `uname -s`, `uname -m` - Platform detection
- System architecture for toolchain selection

## Philosophy

### Core Principles
1. **Actor-Native**: Built for Miniriot/Gluon's effect-based concurrency
2. **Non-Blocking by Default**: All I/O operations yield to the scheduler
3. **Ergonomics First**: APIs should be intuitive and delightful
4. **Batteries Included**: Common tasks should be simple
5. **Cross-Platform**: Windows, macOS, Linux as first-class citizens
6. **Type-Safe**: Leverage OCaml's type system, avoid stringly-typed APIs
7. **Fast by Default**: Performance without sacrificing usability

### What Makes This Different?
- **Effect-Based**: Uses Miniriot's effect handlers for all I/O
- **Actor-Friendly**: Operations that naturally fit the actor model
- **No Blocking**: Never blocks the scheduler, always yields
- **Modern Defaults**: UTF-8 everywhere, Result types
- **Unified API**: One way to do things, not three
- **Rich Types**: Use records and variants, not tuples and ints

## Library Structure

```
packages/std/
├── src/
│   ├── std.ml           # Main entry point with common exports
│   ├── std.mli
│   │
│   │── Network
│   ├── net.ml           # Core networking (sockets, tcp, udp)
│   ├── net.mli
│   ├── http.ml          # HTTP client/server
│   ├── http.mli
│   ├── ws.ml            # WebSocket support
│   ├── ws.mli
│   ├── uri.ml           # URI parsing and manipulation
│   ├── uri.mli
│   ├── mail.ml          # SMTP/IMAP/POP email
│   ├── mail.mli
│   ├── ftp.ml           # FTP client
│   ├── ftp.mli
│   │
│   │── Data Formats
│   ├── json.ml          # JSON parsing/generation
│   ├── json.mli
│   ├── toml.ml          # TOML parsing
│   ├── toml.mli
│   ├── xml.ml           # XML parsing
│   ├── xml.mli
│   ├── csv.ml           # CSV handling
│   ├── csv.mli
│   ├── sexpr.ml         # S-expressions
│   ├── sexpr.mli
│   ├── base64.ml        # Base64/32/16 encoding
│   ├── base64.mli
│   ├── semver.ml        # Semantic versioning
│   ├── semver.mli
│   ├── pem.ml           # PEM encoding
│   ├── pem.mli
│   │
│   │── Date/Time
│   ├── time.ml          # Time and duration
│   ├── time.mli
│   ├── date.ml          # Date handling
│   ├── date.mli
│   ├── calendar.ml      # Calendar operations
│   ├── calendar.mli
│   │
│   │── Filesystem
│   ├── fs.ml            # File operations
│   ├── fs.mli
│   ├── path.ml          # Path manipulation
│   ├── path.mli
│   ├── tree.ml          # Directory tree walker
│   ├── tree.mli
│   ├── temp.ml          # Temporary files/dirs
│   ├── temp.mli
│   │
│   │── Cryptography
│   ├── crypto.ml        # Main crypto module
│   ├── crypto.mli
│   ├── uuid.ml          # UUID generation
│   ├── uuid.mli
│   ├── random.ml        # Cryptographic random
│   ├── random.mli
│   ├── hash.ml          # Hash functions (SHA*, MD5, etc)
│   ├── hash.mli
│   ├── cipher.ml        # Symmetric ciphers (AES, DES, RC4)
│   ├── cipher.mli
│   ├── signature.ml     # Digital signatures (RSA, ECDSA, Ed25519)
│   ├── signature.mli
│   ├── kdf.ml           # Key derivation (HKDF, PBKDF2)
│   ├── kdf.mli
│   ├── tls.ml           # TLS support
│   ├── tls.mli
│   ├── x509.ml          # X.509 certificates
│   ├── x509.mli
│   │
│   │── Compression
│   ├── compress.ml      # Main compression module
│   ├── compress.mli
│   ├── gzip.ml          # Gzip compression
│   ├── gzip.mli
│   ├── zlib.ml          # Zlib compression
│   ├── zlib.mli
│   ├── bzip2.ml         # Bzip2 compression
│   ├── bzip2.mli
│   │
│   │── I/O & Utilities
│   ├── buffer.ml        # Efficient buffers
│   ├── buffer.mli
│   ├── stream.ml        # Async streams
│   ├── stream.mli
│   ├── timer.ml         # Timers and delays
│   ├── timer.mli
│   ├── args.ml          # Command line argument parsing
│   ├── args.mli
│   ├── log.ml           # Structured logging
│   ├── log.mli
│   │
│   │── Actor System Extensions
│   ├── supervisor.ml    # Dynamic supervisors
│   ├── supervisor.mli
│   ├── agent.ml         # Stateful agents
│   ├── agent.mli
│   ├── registry.ml      # Process registry
│   ├── registry.mli
│   ├── task.ml          # Task abstraction
│   ├── task.mli
│   │
│   │── Application Support
│   ├── config.ml        # Configuration management
│   ├── config.mli
│   ├── locale.ml        # Internationalization
│   ├── locale.mli
│   ├── sql.ml           # SQL database interface
│   ├── sql.mli
│   │
│   └── platform.ml      # Platform detection
│       platform.mli
│
├── c/
│   ├── crypto/          # OpenSSL/libsodium bindings
│   ├── compression/     # zlib, bzip2 bindings
│   └── platform/        # Platform-specific code
│
└── rust/ (optional)
    ├── crypto/          # Ring/rustls for crypto
    ├── compression/     # Fast compression
    └── parsers/         # Fast parsing (JSON, XML, etc)
```

## Complete Module APIs

### Network Modules

#### Std.Net - Core Networking
```ocaml
module Net : sig
  module Socket : sig
    type t
    type family = [`Unix | `Inet | `Inet6]
    type socket_type = [`Stream | `Dgram | `Raw]
    
    val create : family -> socket_type -> (t, error) result
    val bind : t -> addr:string -> port:int -> (unit, error) result
    val listen : t -> backlog:int -> (unit, error) result
    val accept : t -> (t * string, error) result
    val connect : t -> addr:string -> port:int -> (unit, error) result
    val send : t -> bytes -> (int, error) result
    val recv : t -> bytes -> (int, error) result
    val close : t -> unit
  end
  
  module Tcp : sig
    type listener
    type stream
    
    val listen : addr:string -> port:int -> (listener, error) result
    val accept : listener -> (stream * string, error) result
    val connect : addr:string -> port:int -> (stream, error) result
    val send : stream -> string -> (unit, error) result
    val recv : stream -> int -> (string, error) result
  end
  
  module Udp : sig
    type socket
    
    val bind : addr:string -> port:int -> (socket, error) result
    val send_to : socket -> data:string -> addr:string -> port:int -> (unit, error) result
    val recv_from : socket -> int -> (string * string * int, error) result
  end
  
  module Pipe : sig
    type t
    val create : unit -> (t * t, error) result  (* reader, writer *)
    val read : t -> int -> (string, error) result
    val write : t -> string -> (unit, error) result
  end
end
```

#### Std.Http - Complete HTTP Implementation
```ocaml
module Http : sig
  module Client : sig
    type response = {
      status : int;
      headers : (string * string) list;
      body : string;
    }
    
    val get : string -> (response, error) result
    val post : string -> body:string -> (response, error) result
    val put : string -> body:string -> (response, error) result
    val delete : string -> (response, error) result
    val get_json : string -> (Json.t, error) result
    val post_json : string -> body:Json.t -> (response, error) result
  end
  
  module Server : sig
    type handler = request -> (response, error) result
    type server
    
    val create : addr:string -> port:int -> handler -> (server, error) result
    val start : server -> (unit, error) result
    val stop : server -> unit
  end
  
  module Middleware : sig
    type t = handler -> handler
    val cors : ?origins:string list -> unit -> t
    val logging : t
    val gzip : t
    val rate_limit : requests_per_second:int -> t
  end
end
```

#### Std.Mail - Email Support
```ocaml
module Mail : sig
  module Smtp : sig
    type connection
    type message = {
      from : string;
      to_ : string list;
      subject : string;
      body : string;
      attachments : (string * bytes) list;
    }
    
    val connect : host:string -> port:int -> ?tls:bool -> (connection, error) result
    val login : connection -> username:string -> password:string -> (unit, error) result
    val send : connection -> message -> (unit, error) result
    val close : connection -> unit
  end
  
  module Imap : sig
    type connection
    type mailbox = { name : string; messages : int }
    type message = { uid : int; subject : string; body : string }
    
    val connect : host:string -> port:int -> ?tls:bool -> (connection, error) result
    val login : connection -> username:string -> password:string -> (unit, error) result
    val list_mailboxes : connection -> (mailbox list, error) result
    val select : connection -> mailbox:string -> (unit, error) result
    val fetch : connection -> uid:int -> (message, error) result
  end
end
```

### Data Format Modules

#### Std.Json - Enhanced JSON
```ocaml
module Json : sig
  type t = 
    | Null
    | Bool of bool
    | Number of float
    | String of string
    | Array of t list
    | Object of (string * t) list
  
  val parse : string -> (t, error) result
  val stringify : ?pretty:bool -> t -> string
  
  (* Builder API *)
  val null : t
  val bool : bool -> t
  val int : int -> t
  val float : float -> t
  val string : string -> t
  val array : t list -> t
  val obj : (string * t) list -> t
  
  (* Accessor API *)
  val get : string -> t -> t option
  val get_string : string -> t -> string option
  val get_int : string -> t -> int option
  val get_bool : string -> t -> bool option
  val get_array : string -> t -> t list option
  
  (* Path-based access *)
  val at : string list -> t -> t option  (* ["users"; "0"; "name"] *)
  
  (* Combinators *)
  val map : (t -> t) -> t -> t
  val filter : (string -> t -> bool) -> t -> t
end
```

#### Std.Toml, Xml, Csv
```ocaml
module Toml : sig
  type t
  val parse : string -> (t, error) result
  val stringify : t -> string
  val get : string -> t -> t option
  val get_string : string -> t -> string option
  val get_int : string -> t -> int option
  val get_table : string -> t -> t option
end

module Xml : sig
  type element = {
    tag : string;
    attrs : (string * string) list;
    children : node list;
  }
  and node = Element of element | Text of string
  
  val parse : string -> (element, error) result
  val stringify : ?pretty:bool -> element -> string
  val find : selector:string -> element -> element list
end

module Csv : sig
  type t = string list list
  val parse : ?delimiter:char -> string -> (t, error) result
  val stringify : ?delimiter:char -> t -> string
  val parse_file : ?delimiter:char -> string -> (t, error) result
  val write_file : ?delimiter:char -> string -> t -> (unit, error) result
end
```

### Cryptography Modules

#### Std.Crypto - Comprehensive Crypto
```ocaml
module Crypto : sig
  module Hash : sig
    val md5 : string -> string
    val sha1 : string -> string
    val sha256 : string -> string
    val sha512 : string -> string
    val sha3_256 : string -> string
    val blake3 : string -> string
    
    val hmac_sha256 : key:string -> string -> string
    val hmac_sha512 : key:string -> string -> string
  end
  
  module Cipher : sig
    module Aes : sig
      type key
      val key_from_bytes : bytes -> key
      val encrypt : key -> iv:bytes -> plaintext:bytes -> bytes
      val decrypt : key -> iv:bytes -> ciphertext:bytes -> (bytes, error) result
    end
    
    module ChaCha20 : sig
      val encrypt : key:bytes -> nonce:bytes -> plaintext:bytes -> bytes
      val decrypt : key:bytes -> nonce:bytes -> ciphertext:bytes -> (bytes, error) result
    end
  end
  
  module Signature : sig
    module Ed25519 : sig
      type keypair = { public : bytes; private : bytes }
      val generate : unit -> keypair
      val sign : keypair -> bytes -> bytes
      val verify : public_key:bytes -> signature:bytes -> message:bytes -> bool
    end
    
    module Rsa : sig
      type keypair
      val generate : bits:int -> keypair
      val sign : keypair -> bytes -> bytes
      val verify : keypair -> signature:bytes -> message:bytes -> bool
    end
  end
  
  module Random : sig
    val bytes : int -> bytes
    val int : max:int -> int
    val string : length:int -> string
    val uuid4 : unit -> string
  end
end
```

### Actor System Extensions

#### Std.Supervisor - Dynamic Supervision
```ocaml
module Supervisor : sig
  type t
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
  val remove_child : t -> id:string -> (unit, error) result
  val restart_child : t -> id:string -> (Process.t, error) result
  val which_children : t -> (string * Process.t) list
  val count_children : t -> int
end
```

#### Std.Agent - Stateful Actors
```ocaml
module Agent : sig
  type 'a t
  
  val start : 'a -> ('a t, error) result
  val get : 'a t -> ('a -> 'b) -> 'b
  val update : 'a t -> ('a -> 'a) -> unit
  val cast : 'a t -> ('a -> 'a) -> unit  (* Async update *)
  val call : 'a t -> ('a -> 'a * 'b) -> 'b  (* Sync update with return *)
  val stop : 'a t -> unit
end
```

#### Std.Registry - Process Discovery
```ocaml
module Registry : sig
  type t
  
  val start : unit -> (t, error) result
  val register : t -> name:string -> Process.t -> (unit, error) result
  val unregister : t -> name:string -> unit
  val lookup : t -> name:string -> Process.t option
  val list : t -> (string * Process.t) list
  
  (* Global registry *)
  val global : unit -> t
  val register_global : name:string -> Process.t -> (unit, error) result
  val lookup_global : name:string -> Process.t option
end
```

### Application Support

#### Std.Config - Configuration Management
```ocaml
module Config : sig
  type t
  
  val load : string -> (t, error) result  (* Load from file *)
  val from_env : unit -> t  (* Load from environment *)
  val merge : t -> t -> t
  
  val get_string : t -> key:string -> default:string -> string
  val get_int : t -> key:string -> default:int -> int
  val get_bool : t -> key:string -> default:bool -> bool
  val get_list : t -> key:string -> default:string list -> string list
  
  (* Nested access *)
  val get_section : t -> section:string -> t option
  val get_nested : t -> path:string list -> string option
end
```

#### Std.Log - Structured Logging
```ocaml
module Log : sig
  type level = Debug | Info | Warn | Error | Fatal
  type field = (string * [`String of string | `Int of int | `Float of float])
  
  val debug : ?fields:field list -> string -> unit
  val info : ?fields:field list -> string -> unit
  val warn : ?fields:field list -> string -> unit
  val error : ?fields:field list -> string -> unit
  val fatal : ?fields:field list -> string -> unit
  
  (* Structured logging *)
  val with_fields : field list -> (unit -> 'a) -> 'a
  val with_context : string -> (unit -> 'a) -> 'a
  
  (* Configuration *)
  val set_level : level -> unit
  val set_output : out_channel -> unit
  val set_format : [`Json | `Pretty | `Compact] -> unit
end
```

#### Std.Sql - Database Interface
```ocaml
module Sql : sig
  type connection
  type row = (string * [`Null | `String of string | `Int of int | `Float of float]) list
  
  module Connection : sig
    val connect : string -> (connection, error) result  (* Connection string *)
    val close : connection -> unit
    val ping : connection -> bool
  end
  
  module Query : sig
    val execute : connection -> string -> (unit, error) result
    val fetch_all : connection -> string -> (row list, error) result
    val fetch_one : connection -> string -> (row option, error) result
    val prepare : connection -> string -> (statement, error) result
  end
  
  module Transaction : sig
    val begin_ : connection -> (unit, error) result
    val commit : connection -> (unit, error) result
    val rollback : connection -> (unit, error) result
    val with_transaction : connection -> (connection -> 'a) -> ('a, error) result
  end
end
```

## Complete Usage Example

Here's what building a modern OCaml application would look like with this standard library:

```ocaml
open Std
open Miniriot

(* Example: A complete web API with database, logging, and email *)

let main () =
  (* Load configuration *)
  let* config = Config.load "app.toml" in
  
  (* Setup structured logging *)
  Log.set_format `Json;
  Log.set_level (Config.get_string config ~key:"log_level" ~default:"info" |> function
    | "debug" -> Debug | "info" -> Info | "warn" -> Warn | "error" -> Error);
  
  (* Connect to database *)
  let* db = Sql.Connection.connect 
    (Config.get_string config ~key:"database_url" ~default:"sqlite:app.db") in
  
  (* Setup email *)
  let* smtp = Mail.Smtp.connect 
    ~host:(Config.get_string config ~key:"smtp_host" ~default:"localhost")
    ~port:(Config.get_int config ~key:"smtp_port" ~default:587)
    ~tls:true in
  
  (* Define HTTP handlers *)
  let handle_request request =
    Log.info "Handling request" ~fields:["path", `String request.path];
    
    match request.path with
    | "/users" when request.method = `GET ->
        let* users = Sql.Query.fetch_all db "SELECT * FROM users" in
        let json = Json.array (List.map user_to_json users) in
        Ok { status = 200; body = Json.stringify json; headers = [
          "Content-Type", "application/json"
        ]}
    
    | "/users" when request.method = `POST ->
        let* json = Json.parse request.body in
        let* user = create_user db json in
        
        (* Send welcome email in background *)
        spawn (fun () ->
          let* () = Mail.Smtp.send smtp {
            from = "noreply@app.com";
            to_ = [user.email];
            subject = "Welcome!";
            body = "Welcome to our app!";
            attachments = [];
          } in
          Log.info "Welcome email sent" ~fields:["user", `String user.email];
          Process.Normal
        );
        
        Ok { status = 201; body = Json.stringify (user_to_json user); headers = [] }
    
    | _ -> Ok { status = 404; body = "Not found"; headers = [] }
  in
  
  (* Create HTTP server with middleware *)
  let handler = 
    Http.Middleware.logging 
    @@ Http.Middleware.cors ()
    @@ Http.Middleware.rate_limit ~requests_per_second:100
    @@ handle_request
  in
  
  let* server = Http.Server.create 
    ~addr:"0.0.0.0" 
    ~port:(Config.get_int config ~key:"port" ~default:8080)
    handler in
  
  (* Start server *)
  Log.info "Starting server" ~fields:["port", `Int 8080];
  Http.Server.start server

(* Actor-based background job processor *)
let job_processor db =
  let rec loop () =
    match receive () with
    | `ProcessJob job_id ->
        Log.info "Processing job" ~fields:["job_id", `Int job_id];
        let* job = Sql.Query.fetch_one db 
          "SELECT * FROM jobs WHERE id = ? AND status = 'pending'" in
        
        (match job with
        | Some job_data ->
            let* () = process_job job_data in
            let* () = Sql.Query.execute db 
              "UPDATE jobs SET status = 'completed' WHERE id = ?" in
            Log.info "Job completed" ~fields:["job_id", `Int job_id]
        | None ->
            Log.warn "Job not found" ~fields:["job_id", `Int job_id]);
        
        loop ()
    | `Stop -> Process.Normal
  in
  loop ()

(* File watcher for hot reloading *)
let config_watcher () =
  let watcher = Fs.watch "app.toml" (fun event ->
    match event with
    | `Modified path ->
        Log.info "Config file changed, reloading" ~fields:["path", `String path];
        send (Registry.lookup_global "main") `ReloadConfig
  ) in
  
  let rec loop () =
    match receive () with
    | `Stop -> 
        Fs.Watcher.stop watcher;
        Process.Normal
  in
  loop ()

(* Application supervisor *)
let app_supervisor () =
  let children = [
    { id = "web_server"; start = main; restart = `Permanent; 
      shutdown = `Timeout 5.0 };
    { id = "job_processor"; start = (fun () -> job_processor db); 
      restart = `Permanent; shutdown = `Timeout 10.0 };
    { id = "config_watcher"; start = config_watcher; 
      restart = `Permanent; shutdown = `Timeout 1.0 };
  ] in
  
  let* supervisor = Supervisor.start `One_for_one children in
  
  (* Register supervisor globally *)
  Registry.register_global "supervisor" (self ());
  
  let rec loop () =
    match receive () with
    | `ReloadConfig ->
        Log.info "Reloading configuration";
        let* () = Supervisor.restart_child supervisor "web_server" in
        loop ()
    | `Shutdown ->
        Log.info "Shutting down application";
        Process.Normal
  in
  loop ()
```

## Implementation Roadmap

### Phase 1: Foundation (Month 1)
**Core I/O and Data Structures**
- `Std.Fs` - Non-blocking file operations
- `Std.Path` - Path manipulation
- `Std.Json` - JSON parsing/generation
- `Std.Crypto.Hash` - Basic hashing (SHA256, SHA512)
- `Std.Buffer` - Efficient buffers
- `Std.Log` - Basic logging

**Deliverable**: Replace all file operations and hashing in Tusk

### Phase 2: Networking (Month 2) 
**Basic Network Stack**
- `Std.Net` - Core socket operations
- `Std.Http.Client` - HTTP client
- `Std.Uri` - URI parsing
- `Std.Compress.Gzip` - Basic compression

**Deliverable**: Replace curl commands, HTTP toolchain downloads

### Phase 3: Actor Extensions (Month 3)
**Enhanced Actor System**
- `Std.Supervisor` - Dynamic supervision
- `Std.Agent` - Stateful actors
- `Std.Registry` - Process discovery
- `Std.Timer` - Timing utilities

**Deliverable**: Robust actor-based applications

### Phase 4: Data Formats (Month 4)
**Rich Data Handling**
- `Std.Toml` - TOML parsing (for config)
- `Std.Xml` - XML parsing
- `Std.Csv` - CSV handling
- `Std.Base64` - Encoding utilities
- `Std.Semver` - Version handling

**Deliverable**: Complete data format support

### Phase 5: Advanced Crypto (Month 5)
**Production Cryptography**
- `Std.Crypto.Cipher` - AES, ChaCha20
- `Std.Crypto.Signature` - RSA, Ed25519
- `Std.Crypto.Random` - Secure random generation
- `Std.Tls` - TLS support

**Deliverable**: Production-ready security

### Phase 6: Application Support (Month 6)
**Production Infrastructure**
- `Std.Config` - Configuration management
- `Std.Sql` - Database interface
- `Std.Mail` - Email support
- `Std.Http.Server` - HTTP server

**Deliverable**: Complete web application stack

### Phase 7: Advanced Features (Month 7+)
**Specialized Modules**
- `Std.WebSocket` - Real-time communication
- `Std.Ftp` - FTP client
- `Std.Locale` - Internationalization
- Performance optimizations
- Windows support

## Success Metrics

### Performance Targets
- **File operations**: Within 10% of native Unix commands
- **JSON parsing**: Competitive with Yojson
- **HTTP requests**: Match or exceed cohttp performance
- **Memory usage**: Minimal allocations, efficient GC

### Developer Experience
- **One-liner solutions**: Common tasks in single function calls
- **Consistent APIs**: Same patterns across all modules
- **Great errors**: Helpful error messages with context
- **Documentation**: Comprehensive examples and guides

### Production Readiness
- **Cross-platform**: Windows, macOS, Linux support
- **Actor-native**: Seamless Miniriot integration
- **Memory safe**: No unsafe code, proper resource cleanup
- **Battle-tested**: Used in production Riot applications

## Long-term Vision

This standard library would become the foundation for a new generation of OCaml applications:

1. **Tusk Build System**: Fast, reliable, cross-platform builds
2. **Riot Web Framework**: Actor-based web applications
3. **Distributed Systems**: Microservices with actor supervision
4. **CLI Tools**: Cross-platform command-line applications
5. **Embedded Systems**: Real-time actor-based control systems

The goal is to make OCaml development as pleasant and productive as modern languages like Go, Rust, and Node.js, while leveraging OCaml's unique strengths in type safety and functional programming.

```ocaml
open Std

(* One-liners for common tasks *)
let hash = Crypto.sha256 "hello world"
let file_hash = Crypto.sha256_file "/path/to/file"

(* Module API *)
module Crypto : sig
  (* Quick functions *)
  val sha256 : string -> string
  val sha256_file : string -> string
  val sha512 : string -> string
  val blake3 : string -> string  (* Fast modern hash *)
  
  (* Comparison *)
  val equal : string -> string -> bool  (* Constant-time comparison *)
  
  (* Incremental hashing *)
  module Sha256 : sig
    type t
    val init : unit -> t
    val update : t -> string -> unit
    val finalize : t -> string
  end
end
```

### Std.Fs - Non-Blocking Filesystem for Actors

```ocaml
open Std
open Miniriot

(* All operations are non-blocking and yield to scheduler *)
let main () =
  (* Reading files - yields while I/O happens *)
  let* content = Fs.read "file.txt" in
  let* lines = Fs.read_lines "file.txt" in
  let* json = Fs.read_json "config.json" in
  
  (* Writing - also non-blocking *)
  let* () = Fs.write "output.txt" "Hello, World!" in
  let* () = Fs.write_lines "output.txt" ["line1"; "line2"] in
  let* () = Fs.write_json "data.json" json_value in
  
  (* Parallel file operations in different actors *)
  let copy_task = spawn (fun () -> Fs.copy "src.txt" "dest.txt") in
  let move_task = spawn (fun () -> Fs.move "old.txt" "new.txt") in
  
  (* Wait for both to complete *)
  let* () = Process.wait copy_task in
  let* () = Process.wait move_task in
  Ok ()

(* Module API - All operations use Gluon for non-blocking I/O *)
module Fs : sig
  (* All operations return results and never block the scheduler *)
  
  (* Reading - uses Gluon file descriptors internally *)
  val read : string -> (string, error) result
  val read_lines : string -> (string list, error) result  
  val read_json : string -> (Json.t, error) result
  val read_stream : string -> Stream.t  (* Streaming reads *)
  
  (* Writing - buffered and non-blocking *)
  val write : string -> string -> (unit, error) result
  val write_lines : string -> string list -> (unit, error) result
  val write_json : string -> Json.t -> (unit, error) result
  val append : string -> string -> (unit, error) result
  
  (* Operations - yields during syscalls *)
  val copy : string -> string -> (unit, error) result
  val copy_dir : string -> string -> (unit, error) result
  val move : string -> string -> (unit, error) result
  val remove : string -> (unit, error) result
  val remove_dir : string -> (unit, error) result
  val mkdir : ?parents:bool -> string -> (unit, error) result
  
  (* Queries - cached when possible *)
  val exists : string -> bool
  val is_file : string -> bool
  val is_dir : string -> bool
  val size : string -> int64 option
  val modified_time : string -> Time.t option
  
  (* Finding files - yields between directory reads *)
  val find : ?pattern:string -> ?max_depth:int -> string -> string list
  val glob : string -> string list
  
  (* Watch for changes - perfect for actors *)
  val watch : string -> (Event.t -> unit) -> Watcher.t
  
  (* Atomic operations using temp + rename *)
  val atomic_write : string -> string -> (unit, error) result
end
```

**Implementation using Gluon:**
```ocaml
(* Example implementation of non-blocking read *)
let read path =
  let fd = Gluon.File.open_file ~flags:[O_RDONLY] path in
  let size = (Gluon.File.stat fd).st_size in
  let buffer = Bytes.create size in
  
  (* This yields to scheduler while reading *)
  let rec read_loop pos =
    if pos >= size then Ok (Bytes.to_string buffer)
    else
      match Gluon.File.read fd buffer ~pos ~len:(size - pos) with
      | Ok bytes_read -> read_loop (pos + bytes_read)
      | Error `Would_block ->
          (* Register interest and yield *)
          Effects.syscall ~name:"read" ~interest:Readable ~source:fd
            (fun () -> read_loop pos)
      | Error e -> Error e
  in
  read_loop 0
```

### Std.Path - Path Manipulation Done Right

```ocaml
open Std

(* Path manipulation that makes sense *)
let path = Path.join ["home"; "user"; "documents"; "file.txt"]
(* Results in: "home/user/documents/file.txt" on Unix *)

let dir = Path.dirname "/home/user/file.txt"  (* "/home/user" *)
let name = Path.basename "/home/user/file.txt"  (* "file.txt" *)
let ext = Path.extension "file.txt"  (* ".txt" *)

(* Module API *)
module Path : sig
  type t
  
  (* Creation *)
  val of_string : string -> t
  val to_string : t -> string
  
  (* Building paths *)
  val join : string list -> string
  val (/) : string -> string -> string  (* "dir" / "file" *)
  
  (* Parts *)
  val dirname : string -> string
  val basename : string -> string
  val extension : string -> string
  val without_extension : string -> string
  
  (* Queries *)
  val is_absolute : string -> bool
  val is_relative : string -> bool
  
  (* Resolution *)
  val normalize : string -> string  (* Remove .., ., // *)
  val absolute : string -> string
  val relative : from:string -> to_:string -> string
  
  (* Common paths *)
  val home : unit -> string
  val cwd : unit -> string
  val temp : unit -> string
end
```

### Std.Http - Actor-Based HTTP Client

```ocaml
open Std
open Miniriot

(* Concurrent requests using actors *)
let fetch_all urls =
  (* Spawn an actor for each URL *)
  let tasks = List.map (fun url ->
    spawn (fun () -> Http.get url)
  ) urls in
  
  (* Collect all results *)
  List.map Process.wait tasks

(* Rate-limited requests *)
let fetch_with_rate_limit urls ~max_concurrent:3 =
  (* Use a pool of worker actors *)
  let pool = Pool.create ~size:max_concurrent in
  List.map (fun url ->
    Pool.submit pool (fun () -> Http.get url)
  ) urls

(* Module API - Built on Gluon's non-blocking TCP *)
module Http : sig
  type response = {
    status : int;
    headers : (string * string) list;
    body : string;
  }
  
  (* All requests are non-blocking and yield to scheduler *)
  val get : string -> (response, error) result
  val post : string -> body:string -> (response, error) result
  val put : string -> body:string -> (response, error) result
  val delete : string -> (response, error) result
  
  (* JSON helpers *)
  val get_json : string -> (Json.t, error) result
  val post_json : string -> body:Json.t -> (response, error) result
  
  (* Streaming responses for large downloads *)
  val get_stream : string -> (Stream.t, error) result
  
  (* File downloads with progress (sends messages to actor) *)
  val download : string -> dest:string -> (unit, error) result
  val download_to_actor : 
    string -> 
    dest:string -> 
    progress_pid:Process.t ->  (* Actor to receive progress messages *)
    (unit, error) result
    
  (* Connection pooling per actor *)
  module Pool : sig
    type t
    val create : ?max_connections:int -> unit -> t
    val request : t -> Request.t -> (response, error) result
  end
  
  (* WebSocket support for real-time communication *)
  module WebSocket : sig
    type t
    val connect : string -> (t, error) result
    val send : t -> string -> unit
    val receive : t -> string  (* Blocks until message arrives *)
    val close : t -> unit
  end
end
```

**Implementation with Gluon:**
```ocaml
(* Non-blocking HTTP GET using Gluon *)
let get url =
  let addr = parse_url url in
  
  (* Connect using Gluon's non-blocking TCP *)
  let* socket = Gluon.Net.TcpStream.connect addr in
  
  (* Send request - yields if would block *)
  let request = Format.sprintf "GET %s HTTP/1.1\r\nHost: %s\r\n\r\n" 
    path host in
  let* () = send_all socket request in
  
  (* Read response - yields while waiting for data *)
  let* response = read_response socket in
  
  (* Close connection *)
  Gluon.Net.TcpStream.close socket;
  Ok response
```

### Std.Json - JSON Without the Pain

```ocaml
open Std

(* Parse JSON *)
let json = Json.parse {|{"name": "Alice", "age": 30}|}

(* Access fields safely *)
let name = json |> Json.get "name" |> Json.to_string
let age = json |> Json.get "age" |> Json.to_int

(* Build JSON *)
let user = Json.obj [
  "name", Json.string "Bob";
  "age", Json.int 25;
  "active", Json.bool true;
]

(* Module API *)
module Json : sig
  type t
  
  (* Parsing *)
  val parse : string -> (t, error) result
  val parse_exn : string -> t
  
  (* Building *)
  val null : t
  val bool : bool -> t
  val int : int -> t
  val float : float -> t
  val string : string -> t
  val array : t list -> t
  val obj : (string * t) list -> t
  
  (* Accessing *)
  val get : string -> t -> t option
  val get_exn : string -> t -> t
  val member : string -> t -> t option  (* Alias for get *)
  
  (* Converting *)
  val to_string : t -> string option
  val to_int : t -> int option
  val to_float : t -> float option
  val to_bool : t -> bool option
  val to_list : t -> t list option
  val to_obj : t -> (string * t) list option
  
  (* Serialization *)
  val stringify : t -> string
  val pretty : t -> string
end
```

### Std.Cmd - Command Execution Made Simple

```ocaml
open Std

(* Run simple commands *)
let output = Cmd.run "ls -la"
let lines = Cmd.lines "find . -name '*.ml'"

(* Check if command exists *)
match Cmd.which "git" with
| Some path -> Printf.printf "Git found at %s" path
| None -> print_endline "Git not installed"

(* Module API *)
module Cmd : sig
  type result = {
    exit_code : int;
    stdout : string;
    stderr : string;
  }
  
  (* Simple execution *)
  val run : string -> result
  val run_exn : string -> string  (* Returns stdout, raises on error *)
  val lines : string -> string list  (* Stdout as lines *)
  
  (* Building commands safely *)
  val exec : string -> string list -> result
  (* Cmd.exec "git" ["add"; "."] *)
  
  (* Checking existence *)
  val which : string -> string option
  
  (* Advanced *)
  val with_env : (string * string) list -> string -> result
  val with_cwd : string -> string -> result
  val with_stdin : string -> string -> result
end
```

### Std.Time - Dates and Times for Humans

```ocaml
open Std

(* Current time *)
let now = Time.now ()
let today = Time.today ()

(* Parsing *)
let date = Time.parse "2024-01-15"
let datetime = Time.parse "2024-01-15 14:30:00"

(* Formatting *)
let formatted = Time.format now "%Y-%m-%d %H:%M:%S"

(* Module API *)
module Time : sig
  type t
  
  val now : unit -> t
  val today : unit -> t
  
  (* Parsing *)
  val parse : string -> t option
  val parse_exn : string -> t
  
  (* Formatting *)
  val format : t -> string -> string
  val to_string : t -> string  (* ISO 8601 *)
  
  (* Operations *)
  val add_days : t -> int -> t
  val add_hours : t -> int -> t
  val diff : t -> t -> Duration.t
  
  (* Comparison *)
  val before : t -> t -> bool
  val after : t -> t -> bool
  val equal : t -> t -> bool
end
```

## Actor Model Integration

### How Std Works with Miniriot

The entire standard library is built to work seamlessly with Miniriot's actor model:

```ocaml
open Std
open Miniriot

(* Example: A file processing actor *)
let file_processor () =
  (* Receive file paths to process *)
  let rec loop () =
    match receive () with
    | `Process path ->
        (* All I/O is non-blocking *)
        let* content = Fs.read path in
        let processed = transform content in
        let* () = Fs.write (path ^ ".out") processed in
        loop ()
    | `Stop -> 
        Process.Normal
  in
  loop ()

(* Example: HTTP service with rate limiting *)
let api_gateway () =
  let pool = Http.Pool.create ~max_connections:10 in
  
  let rec loop () =
    match receive () with
    | `Request (url, reply_to) ->
        (* Spawn a child actor for each request *)
        spawn (fun () ->
          let* response = Http.Pool.request pool url in
          send reply_to (`Response response);
          Process.Normal
        );
        loop ()
    | `Shutdown ->
        Process.Normal
  in
  loop ()

(* Example: File watcher actor *)
let file_monitor dir =
  (* Watch for file changes *)
  let watcher = Fs.watch dir (fun event ->
    (* Send events to parent actor *)
    send (self ()) (`FileChanged event)
  ) in
  
  let rec loop () =
    match receive () with
    | `FileChanged event ->
        Printf.printf "File changed: %s\n" event.path;
        loop ()
    | `Stop ->
        Fs.Watcher.stop watcher;
        Process.Normal
  in
  loop ()
```

### Key Design Patterns

#### 1. **All I/O Yields**
Every I/O operation in Std automatically yields to the Miniriot scheduler:
- File reads/writes use Gluon's non-blocking file operations
- Network operations use Gluon's kqueue/epoll integration
- Never blocks the actor or scheduler

#### 2. **Stream Processing**
Large data is handled via streams that yield between chunks:
```ocaml
let process_large_file path =
  let stream = Fs.read_stream path in
  Stream.iter (fun chunk ->
    (* Process chunk - yields between chunks *)
    process_chunk chunk
  ) stream
```

#### 3. **Parallel Operations**
Use actors for natural parallelism:
```ocaml
let parallel_download urls =
  urls
  |> List.map (fun url -> spawn (fun () -> Http.get url))
  |> List.map Process.wait
```

#### 4. **Progress Reporting**
Long operations report progress via messages:
```ocaml
let download_with_progress url =
  let progress_actor = spawn (fun () ->
    let rec loop () =
      match receive () with
      | `Progress (downloaded, total) ->
          Printf.printf "Downloaded %Ld/%Ld bytes\n" downloaded total;
          loop ()
      | `Complete -> Process.Normal
    in
    loop ()
  ) in
  
  Http.download_to_actor url ~dest:"file.zip" ~progress_pid:progress_actor
```

### Implementation Details

All Std modules are implemented using:

1. **Gluon for I/O**: All file and network operations use Gluon's non-blocking primitives
2. **Effects for Yielding**: Use Miniriot's effect handlers to yield control
3. **No Threading**: Everything runs in the same OS thread, scheduled by Miniriot
4. **Message Passing**: Progress, errors, and results communicated via actor messages

## Implementation Strategy

### Phase 1: Core I/O (Week 1)
Replace the most commonly used commands:

1. **Sys.Fs.copy** - Used everywhere for file copying
2. **Sys.Fs.mkdir_p** - Directory creation
3. **Sys.Crypto.SHA256** - Hashing for cache
4. **Sys.Platform** - Platform detection

### Phase 2: Build System (Week 2)
Replace build-specific operations:

1. **Sys.Archive** - Tar handling for toolchains
2. **Sys.Net.download** - Toolchain downloads
3. **Sys.Fs.find** - File discovery

### Phase 3: Polish (Week 3)
Complete the library:

1. **Sys.Process** - Better command execution
2. **Error handling** - Consistent exceptions
3. **Documentation** - Usage examples
4. **Tests** - Cross-platform testing

## Usage Migration Examples

### Current Code (Shell Commands)
```ocaml
(* File operations *)
let cmd = Printf.sprintf "cp %s %s" src dest in
ignore (Unix.system cmd);

(* Archive extraction *)
let cmd = Printf.sprintf "tar -xzf %s -C %s" archive dest in
let success, output = System.run_command cmd;

(* Hashing *)
let cmd = Printf.sprintf "shasum -a 256 '%s' | cut -d' ' -f1" file in
let ic = Unix.open_process_in cmd in
let hash = input_line ic in
```

### New Code (Sys Library)
```ocaml
open Sys

(* File operations *)
Fs.copy ~src ~dest;

(* Archive extraction *)
Archive.extract_tar_gz ~file:archive ~dest;

(* Hashing *)
let hash = Crypto.SHA256.file file;
```

## Benefits

### Immediate Benefits
1. **Windows Support**: No dependency on Unix commands
2. **Performance**: No process spawning overhead
3. **Error Handling**: Proper OCaml exceptions
4. **Type Safety**: No string parsing

### Long-term Benefits
1. **Maintainability**: OCaml code vs shell scripts
2. **Debugging**: Can step through operations
3. **Testing**: Unit tests vs integration tests
4. **Security**: No command injection risks

## Design Principles

1. **Minimal Dependencies**: Avoid heavy external libraries
2. **Cross-Platform First**: Windows/macOS/Linux from day one
3. **Performance**: C/Rust for hot paths, OCaml for logic
4. **Gradual Adoption**: Can migrate incrementally
5. **Clear Errors**: Descriptive exceptions, not exit codes

## Performance Targets

- File copy: Match `cp` performance
- SHA256: Within 2x of `shasum`
- Tar extraction: Match `tar` for common cases
- HTTP download: Match `curl` for simple downloads

## Testing Requirements

### Unit Tests
- Each function tested in isolation
- Mock filesystem operations
- Property-based testing where applicable

### Integration Tests
- Real filesystem operations
- Real network requests
- Large file handling

### Platform Tests
- CI on Linux, macOS, Windows
- ARM64 and x86_64
- Different OS versions

## Future Extensions

Once core is stable, consider:

- **Sys.Crypto.sign** - Digital signatures
- **Sys.Fs.watch** - File system watching
- **Sys.Archive.Zip** - ZIP file support
- **Sys.Net.parallel_download** - Concurrent downloads
- **Sys.Terminal** - Terminal colors/control

## Migration Plan

1. **Create `packages/sys`** with basic structure
2. **Implement Phase 1** modules
3. **Add to Tusk** as dependency
4. **Replace one command** at a time
5. **Remove System.run_command** gradually
6. **Document patterns** for other projects

## Success Metrics

- Zero shell commands in core build path
- Windows CI passing without WSL
- 50% reduction in process spawns
- Consistent behavior across platforms

## Open Questions

1. Should we vendor C code or use opam packages?
2. How much Rust is acceptable for performance?
3. Should this be part of Riot or standalone?
4. What's the versioning strategy?
5. How do we handle platform-specific features?