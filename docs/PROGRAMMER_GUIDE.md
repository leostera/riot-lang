# The Riot ML Programmer's Guide
## A Comprehensive Developer's Guide to the Riot Stack

**Last Updated:** November 2025  
**For:** New OCaml Programmers joining the Riot ecosystem  
**What:** Complete walkthrough of Std library, packages, idioms, and best practices

---

## Table of Contents

1. [Quick Start](#quick-start)
2. [Critical Rules](#critical-rules)
3. [The Std Library](#the-std-library)
4. [Collections Deep Dive](#collections-deep-dive)
5. [Error Handling](#error-handling)
6. [Actor Model & Concurrency](#actor-model--concurrency)
7. [File System Operations](#file-system-operations)
8. [Writing Tests](#writing-tests)
9. [Creating New Packages](#creating-new-packages)
10. [Interface Design Patterns](#interface-design-patterns)
11. [Common Patterns & Idioms](#common-patterns--idioms)
12. [Build System (Tusk)](#build-system-tusk)

---

## Quick Start

### Your First Riot Program

```ocaml
(* src/main.ml *)
open Std

let () =
  Miniriot.run ~main:(fun ~args:_ ->
    println "Hello, Riot!";
    Ok ()
  ) ~args:Env.args ()
```

### Project Structure

```
my-app/
├── tusk.toml          # Package manifest
├── src/
│   ├── main.ml        # Entry point (for binaries)
│   └── my_app.ml      # Library code (for libraries)
└── tests/
    └── test_my_app.ml # Tests
```

### Basic `tusk.toml`

```toml
[package]
name = "my-app"
version = "0.1.0"

[[bin]]
name = "my-app"
path = "src/main.ml"

[dependencies]
std = { path = "../std" }
```

---

## Critical Rules

### ABSOLUTE REQUIREMENTS

These rules are **non-negotiable** and violating them will break your code:

1. **ALWAYS `open Std` at the top of every file**
   ```ocaml
   (* CORRECT *)
   open Std
   
   let example () = ...
   
   (* WRONG - will not compile *)
   let example () = ...
   ```

2. **NEVER use `Stdlib`, `Unix`, `Sys`, or `Obj` modules**
   - Use `Std` instead
   - These are not available in nostdlib mode

3. **NEVER use built-in `ref` for mutable values**
   ```ocaml
   (* WRONG *)
   let counter = ref 0
   
   (* CORRECT *)
   let counter = cell 0
   (* or *)
   let counter = Sync.Cell.make 0
   ```

4. **For mutable record fields, use `mutable` keyword**
   ```ocaml
   (* CORRECT - use mutable keyword *)
   type state = {
     name : string;
     mutable count : int;
   }
   
   let s = { name = "test"; count = 0 } in
   s.count <- s.count + 1
   
   (* WRONG - don't use Cell in records *)
   type state = {
     name : string;
     count : int Cell.t;  (* Don't do this! *)
   }
   ```

5. **NEVER use `sed`, `awk`, `perl`, or bash to edit files**
   - Use the Edit tool provided by the environment

6. **NEVER use `ocamlc`, `opam`, or `dune` directly**
   - Use `tusk` build system only

7. **Always run `tusk` from workspace root**
   - It operates from root regardless of your `cd` location

8. **Use `timeout` for long-running commands**
   ```bash
   timeout 30 tusk test my-package
   ```

9. **In tests: use `expect`/`unwrap` instead of graceful error handling**
   ```ocaml
   (* CORRECT - test the happy path *)
   let result = compute () |> Result.expect ~msg:"computation failed"
   
   (* WRONG in tests - don't handle errors gracefully *)
   match compute () with
   | Ok x -> x
   | Error _ -> default_value
   ```

10. **Discover targets with `tusk completions`**
    ```bash
    tusk completions --packages
    tusk completions --binaries
    tusk completions --tests
    ```

---

## The Std Library

### Overview

`Std` is Riot's comprehensive standard library providing everything you need for building robust, concurrent, fault-tolerant applications:

- **Error Handling** - Modern Result/Option types with rich combinators
- **Collections** - Vector, HashMap, HashSet, Queue, Deque, Heap, List
- **Actor Concurrency** - Process-based with supervision trees
- **Filesystem** - Type-safe operations with Path abstraction
- **Networking** - TCP/TLS with HTTP client/server primitives
- **Time & Date** - Duration, Instant, SystemTime, Datetime with full calendar support
- **Data Formats** - JSON, TOML, CSV, XML, Sexp, Base16/32/64/85
- **Cryptography** - SHA-256, SHA-512, MD5 hashing
- **Testing** - Built-in test framework with TAP/JUnit/JSON reporters
- **Unicode** - Full UTF-8 support with grapheme clusters and text segmentation
- **And much more...**

### Quick Navigation

The Std library is organized into logical categories. Here's how to find what you need:

#### "I Want To..." Quick Index

- **Read/Write Files** → `Fs.read`, `Fs.write`, `Fs.File`, `Path`
- **Parse Config Files** → `Data.Toml` for TOML, `Data.Json` for JSON
- **Handle Errors** → `Result` for recoverable errors, `Option` for missing values
- **Work with Collections** → `vec`, `map`, `set`, `queue` helpers or `Collections.*`
- **Build a TCP Server** → `Net.TcpServer`, `Net.TcpListener`, `Net.TcpStream`
- **Make HTTP Requests** → Use Blink package (see Web Programming Guide)
- **Parse JSON/XML/CSV** → `Data.Json`, `Data.Xml`, `Data.Csv`
- **Hash Data** → `Crypto.Sha256`, `Crypto.hash_string`
- **Measure Time** → `Time.Instant` for elapsed, `Time.Duration` for spans
- **Build Fault-Tolerant Systems** → `Supervisor`, `Application`
- **Manage Shared State** → `Agent` for simple state, `Sync.Cell` for mutable cells
- **Write Tests** → `Test`, `Test.Assertions`
- **Log Messages** → `Log`
- **Generate UUIDs** → `UUID`

### Module Organization by Category

```ocaml
open Std

(* Core Types & Error Handling *)
Result.*      (* Typed error handling with Ok/Error *)
Option.*      (* Optional values with None/Some *)
Path.*        (* Type-safe filesystem paths *)
String.*      (* UTF-8 strings with iteration *)
Int.* / Float.* / Bool.* / Char.*  (* Primitives with utilities *)
UUID.*        (* Globally unique identifiers *)
Version.*     (* Semantic versioning *)

(* Collections - Use type aliases for convenience *)
let v = vec [1; 2; 3]              (* Vector - growable array *)
let m = map [("a", 1); ("b", 2)]   (* HashMap - O(1) lookups *)
let s = set [1; 2; 3]              (* HashSet - unique values *)
let q = queue ["a"; "b"]           (* Queue - FIFO *)

(* Or use modules directly *)
Collections.Vector.*
Collections.HashMap.*
Collections.HashSet.*
Collections.Queue.*
Collections.Deque.*
Collections.Heap.*
Collections.List.*

(* Time & Date *)
Time.Duration.*      (* Time spans - of_sec, of_millis *)
Time.Instant.*       (* Monotonic time - for benchmarking *)
Time.SystemTime.*    (* Wall-clock time - timestamps *)
Datetime.*           (* Calendar operations - parse, format *)
Timer.*              (* Timed events for actors *)

(* Filesystem & I/O *)
Fs.*                 (* File operations - read, write, create_dir_all *)
Fs.File.*            (* Streaming file I/O *)
Fs.FileWatcher.*     (* Watch files for changes *)
IO.*                 (* Generic Reader/Writer abstractions *)

(* Networking *)
Net.TcpServer.*      (* TCP server with handlers *)
Net.TcpListener.*    (* Low-level connection accepting *)
Net.TcpStream.*      (* TCP client/server streams *)
Net.TlsStream.*      (* TLS/SSL encrypted connections *)
Net.Addr.*           (* Network addresses *)
Net.Uri.*            (* URL/URI parsing *)
Net.Http.*           (* HTTP primitives *)
  Net.Http.Request.*
  Net.Http.Response.*
  Net.Http.Header.*
  Net.Http.Method.*
  Net.Http.Status.*

(* Data Formats *)
Data.Json.*          (* JSON parsing/generation *)
Data.Toml.*          (* TOML config files *)
Data.Csv.*           (* CSV data *)
Data.Xml.*           (* XML parsing *)
Data.Sexp.*          (* S-expressions *)
Data.Base64.*        (* Base64 encoding *)
Data.Base16.*        (* Hex encoding *)
Data.Base32.*        (* Base32 encoding *)
Data.Base85.*        (* Ascii85 encoding *)

(* Cryptography *)
Crypto.*             (* Hash utilities *)
Crypto.Sha256.*      (* SHA-256 hashing *)
Crypto.Sha512.*      (* SHA-512 hashing *)
Crypto.Digest.*      (* Digest formatting *)

(* Actor/OTP Patterns *)
Process.*            (* Spawn, send, receive *)
Pid.*                (* Process identifiers *)
Message.*            (* Extensible message types *)
Agent.*              (* Simple state servers *)
Supervisor.*         (* Fault tolerance trees *)
Supervisor.Dynamic.* (* Dynamic children *)
Task.*               (* Async operations *)
WorkerPool.*         (* Parallel work distribution *)
Application.*        (* Multi-app systems *)

(* System & I/O *)
Command.*            (* Run external programs *)
Env.*                (* Environment variables *)
System.*             (* System information *)
Log.*                (* Structured logging *)
Exception.*          (* Exception utilities *)
Random.*             (* Random generation *)

(* Iteration *)
Iter.Iterator.*      (* Immutable, backtrackable *)
Iter.MutIterator.*   (* Single-pass, efficient *)
Iter.Cursor.*        (* String parsing with backtracking *)

(* Testing *)
Test.*               (* Test framework *)
Test.Assertions.*    (* assert_equal, assert_ok, etc. *)
Test.Reporter.*      (* TAP, JUnit, JSON, Pretty *)

(* Unicode & Text *)
Unicode.*            (* Unicode utilities *)
Unicode.Rune.*       (* Code points *)
Unicode.Grapheme.*   (* User-perceived characters *)
Unicode.Utf8.*       (* UTF-8 encoding/decoding *)
Unicode.Segmentation.* (* Word/line breaking *)

(* Utilities *)
ArgParser.*          (* CLI argument parsing *)
Diff.*               (* Difference computation *)
Telemetry.*          (* Instrumentation/metrics *)
Sync.*               (* Synchronization primitives *)
Sync.Cell.*          (* Mutable cells *)
Graph.*              (* Graph data structures *)
Graph.Dot.*          (* Graphviz visualization *)
Graph.Mermaid.*      (* Mermaid.js diagrams *)
```

### Understanding the Module Hierarchy

Std uses a **flat import** model - when you `open Std`, all modules are available at the top level. Here are the key organizational patterns:

**Parent Modules** contain related functionality:
- `Collections.*` - All data structures
- `Time.*` - Duration, Instant, SystemTime
- `Net.*` - All networking (TCP, HTTP, TLS, Uri, Addr)
- `Data.*` - All data formats (Json, Toml, Csv, Xml, etc.)
- `Crypto.*` - All cryptographic functions
- `Fs.*` - All filesystem operations
- `Iter.*` - All iteration utilities
- `Test.*` - Testing framework components
- `Unicode.*` - Unicode text processing

**Type Aliases** provide shortcuts for common collections:
```ocaml
type 'a vec = 'a Collections.Vector.t
type 'a queue = 'a Collections.Queue.t
type 'a set = 'a Collections.HashSet.t
type ('k, 'v) map = ('k, 'v) Collections.HashMap.t
```

**Helper Functions** create collections from lists:
```ocaml
let v = vec [1; 2; 3]              (* Vector.of_list *)
let m = map [("a", 1); ("b", 2)]   (* HashMap.of_list *)
let s = set [1; 2; 3]              (* HashSet.of_list *)
let q = queue ["a"; "b"; "c"]      (* Queue.of_list *)
```

### Detailed Module Reference

For complete API documentation with "When to use" guidance and examples, see:
- **`packages/std/src/std.mli`** - Complete module documentation with use cases
- The full Std.mli includes a comprehensive table of contents with:
  - Quick Start examples
  - Browse by Category
  - Find by Use Case ("I want to...")
  - Alphabetical Index
  - Module Hierarchy tree
  - Common Patterns section

### Global Functions

These are available immediately after `open Std`:

```ocaml
(* Printing *)
print "hello"           (* no newline *)
println "hello"         (* with newline *)
eprint "error"          (* stderr, no newline *)
eprintln "error"        (* stderr, with newline *)

(* Panics *)
panic "fatal error"     (* terminate program *)
todo "not implemented"  (* panic with TODO message *)
unimplemented ()        (* panic with generic message *)

(* Mutable cells *)
let counter = cell 0    (* create mutable cell *)
Sync.Cell.get counter   (* read *)
Sync.Cell.set counter 1 (* write *)

(* Process management *)
let pid = spawn fn      (* spawn process *)
send pid msg            (* send message *)
receive ~selector ()    (* receive message *)
sleep duration          (* sleep *)
yield ()                (* yield to scheduler *)
```

---

## Collections Deep Dive

### Type Aliases

Std provides convenient type aliases and constructors:

```ocaml
(* Type aliases *)
type 'a vec = 'a Collections.Vector.t
type 'a queue = 'a Collections.Queue.t
type 'a set = 'a Collections.HashSet.t
type ('k, 'v) map = ('k, 'v) Collections.HashMap.t

(* Constructors from lists *)
let v = vec [1; 2; 3]              (* create vector *)
let q = queue ["a"; "b"; "c"]      (* create queue *)
let s = set [1; 2; 3; 2; 1]        (* create set - duplicates removed *)
let m = map [("a", 1); ("b", 2)]   (* create map *)
```

### Vector (Growable Array)

```ocaml
open Std.Collections

(* Creation *)
let v = Vector.create () in
let v = Vector.of_list [1; 2; 3] in
let v = vec [1; 2; 3] in  (* shorthand *)

(* Adding elements *)
Vector.push v 4;           (* O(1) amortized *)

(* Accessing *)
Vector.get v 0             (* Some 1 - O(1) *)
Vector.get v 10            (* None *)
Vector.len v               (* 3 *)

(* Iteration *)
Vector.iter (fun x -> println (Int.to_string x)) v;
Vector.iteri (fun i x -> println "%d: %d" i x) v;

(* Mapping *)
let doubled = Vector.map (fun x -> x * 2) v in

(* Convert to list *)
Vector.to_list v
```

### HashMap (Hash Table)

```ocaml
open Std.Collections

(* Creation *)
let map = HashMap.create () in
let map = HashMap.of_list [("a", 1); ("b", 2)] in
let map = map [("a", 1); ("b", 2)] in  (* shorthand *)

(* Insertion *)
HashMap.insert map "key" "value"  (* returns previous value option *)
|> ignore;

(* Lookup *)
HashMap.find map "key"        (* Some "value" *)
HashMap.find map "missing"    (* None *)

(* Check existence *)
HashMap.contains_key map "key"  (* true *)

(* Removal *)
HashMap.remove map "key"      (* Some "value" *)

(* Iteration *)
HashMap.iter (fun k v ->
  println "%s = %s" k v
) map;

(* Size *)
HashMap.len map
```

### HashSet (Unique Values)

```ocaml
open Std.Collections

(* Creation *)
let set = HashSet.create () in
let set = HashSet.of_list [1; 2; 3; 2; 1] in  (* duplicates removed *)
let set = set [1; 2; 3] in  (* shorthand *)

(* Insertion *)
HashSet.insert set 4  (* returns true if inserted *)
|> ignore;

(* Check membership *)
HashSet.contains set 2  (* true *)

(* Removal *)
HashSet.remove set 2    (* true if removed *)

(* Set operations *)
let s1 = set [1; 2; 3] in
let s2 = set [2; 3; 4] in
HashSet.union s1 s2          (* {1, 2, 3, 4} *)
HashSet.intersection s1 s2   (* {2, 3} *)
HashSet.difference s1 s2     (* {1} *)

(* Size *)
HashSet.len set
```

### Queue (FIFO)

```ocaml
open Std.Collections

(* Creation *)
let q = Queue.create () in
let q = queue [1; 2; 3] in

(* Enqueue *)
Queue.enqueue q 4;

(* Dequeue *)
Queue.dequeue q  (* Some 1 - FIFO order *)
Queue.dequeue q  (* Some 2 *)

(* Peek *)
Queue.peek q     (* Some 3 - doesn't remove *)

(* Size *)
Queue.len q
Queue.is_empty q
```

### Deque (Double-Ended Queue)

```ocaml
open Std.Collections

let d = Deque.create () in

(* Push to both ends *)
Deque.push_back d 1;
Deque.push_front d 0;

(* Pop from both ends *)
Deque.pop_front d   (* Some 0 *)
Deque.pop_back d    (* Some 1 *)

(* Peek both ends *)
Deque.front d
Deque.back d
```

---

## Error Handling

### Result Type

The `Result` type is used for operations that can fail:

```ocaml
type ('a, 'e) Result.t =
  | Ok of 'a
  | Error of 'e
```

#### Basic Usage

```ocaml
(* Function that can fail *)
let divide x y =
  if y = 0 then
    Error "division by zero"
  else
    Ok (x / y)

(* Pattern matching *)
match divide 10 2 with
| Ok result -> println "Result: %d" result
| Error msg -> eprintln "Error: %s" msg

(* Using unwrap (panics on error) *)
let result = divide 10 2 |> Result.unwrap in

(* Using expect (panics with custom message) *)
let result = divide 10 2
  |> Result.expect ~msg:"division should not fail" in

(* Providing default value *)
let result = divide 10 0
  |> Result.unwrap_or ~default:0 in
```

#### Chaining Operations

```ocaml
(* map - transform success value *)
let doubled = divide 10 2
  |> Result.map (fun x -> x * 2) in

(* map_err - transform error *)
let with_context = divide 10 0
  |> Result.map_err (fun e -> "Math error: " ^ e) in

(* and_then - chain fallible operations *)
let chained = divide 100 10
  |> Result.and_then (fun x -> divide x 2)
  |> Result.and_then (fun x -> divide x 5) in
  (* Result: Ok 1 *)

(* Realistic example *)
let process_config () =
  Fs.read (Path.v "config.toml")
  |> Result.and_then Data.Toml.parse
  |> Result.map extract_settings
  |> Result.expect ~msg:"Failed to load configuration"
```

#### Query Methods

```ocaml
Result.is_ok (Ok 5)      (* true *)
Result.is_err (Ok 5)     (* false *)

Result.is_ok_and (fun x -> x > 0) (Ok 5)    (* true *)
Result.is_err_and (fun e -> String.contains e "not found") (Error "file not found")  (* true *)
```

### Option Type

The `Option` type represents an optional value:

```ocaml
type 'a Option.t =
  | Some of 'a
  | None
```

#### Basic Usage

```ocaml
(* Function that may not return a value *)
let find_user id =
  if id > 0 then Some { name = "Alice"; id }
  else None

(* Pattern matching *)
match find_user 1 with
| Some user -> println "Found: %s" user.name
| None -> println "Not found"

(* Using unwrap (panics on None) *)
let user = find_user 1 |> Option.unwrap in

(* Using expect *)
let user = find_user 1
  |> Option.expect ~msg:"user should exist" in

(* Providing default *)
let user = find_user 0
  |> Option.unwrap_or ~default:guest_user in
```

#### Chaining Operations

```ocaml
(* map - transform Some value *)
let name = find_user 1
  |> Option.map (fun u -> u.name) in  (* Some "Alice" *)

(* and_then - chain optional operations *)
let result = Some 5
  |> Option.and_then (fun x ->
      if x > 0 then Some (x * 2)
      else None) in  (* Some 10 *)

(* Converting between Option and Result *)
let opt = Some 42 in
Option.ok_or ~error:"missing value" opt  (* Ok 42 *)

let res = Ok 42 in
Result.to_option res  (* Some 42 *)
```

---

## Actor Model & Concurrency

Riot uses an Erlang-inspired actor model for concurrency.

### Processes

```ocaml
open Std

(* Spawn a process *)
let worker_fn () =
  println "Worker starting";
  sleep 1.0;
  println "Worker done";
  Ok ()
in

let pid = spawn worker_fn in

(* Spawn and link (failures propagate) *)
let pid = spawn_link worker_fn in
```

### Message Passing

```ocaml
(* Extend the Message type *)
type Message.t +=
  | Ping
  | Pong
  | GetValue of { reply : int -> unit }

(* Sender *)
let () =
  let pid = spawn server in
  send pid Ping;
  send pid (GetValue { reply = fun v -> println "Value: %d" v })

(* Receiver with selector *)
let server () =
  let selector = function
    | Ping -> Some `Ping
    | Pong -> Some `Pong
    | _ -> None
  in
  
  match receive ~selector () with
  | `Ping ->
      println "Received Ping";
      let sender = self () in
      send sender Pong
  | `Pong ->
      println "Received Pong"
```

### Agent (Simple State Server)

Agent provides a simple way to manage state in a concurrent context:

```ocaml
open Std

(* Create an agent with initial state *)
let counter = Agent.start (fun () -> 0) in

(* Read state *)
let value = Agent.get counter (fun n -> n) in
println "Counter: %d" value;

(* Read with transformation *)
let doubled = Agent.get counter (fun n -> n * 2) in

(* Update state synchronously *)
Agent.update counter (fun n -> n + 1);

(* Update and return old value *)
let old = Agent.get_and_update counter (fun n -> (n, n + 10)) in

(* Update asynchronously (cast) *)
Agent.cast counter (fun n -> n + 5);

(* Stop the agent *)
Agent.stop counter;

(* Type polymorphism example *)
type person = { name : string; age : int }

let person_agent = Agent.start (fun () ->
  { name = "Alice"; age = 30 }
) in

Agent.update person_agent (fun p -> { p with age = p.age + 1 });
let person = Agent.get person_agent (fun p -> p) in
```

### WorkerPool

For CPU-intensive parallel tasks:

```ocaml
open Std

(* Simple parallel map *)
let results = WorkerPool.run
  ~concurrency:4
  ~tasks:[1; 2; 3; 4; 5; 6; 7; 8]
  ~fn:(fun x -> x * x)
in
(* results = [1; 4; 9; 16; 25; 36; 49; 64] *)

(* Dynamic task assignment *)
type Message.t +=
  | WorkerReady of { worker_id : int }
  | TaskComplete of { result : int }

let pool = WorkerPool.Dynamic.start
  ~concurrency:4
  ~owner:(self ())
  ~worker_fn:(fun task ->
      (* Process task *)
      let result = expensive_computation task in
      TaskComplete { result }
  )
in

(* Dispatch tasks dynamically *)
let rec dispatch_loop remaining_tasks =
  match receive ~selector:(function
    | WorkerReady { worker_id } -> Some (`Ready worker_id)
    | TaskComplete { result } -> Some (`Done result)
    | _ -> None
  ) () with
  | `Ready worker_id ->
      (match remaining_tasks with
       | task :: rest ->
           WorkerPool.Dynamic.assign_task pool worker_id task;
           dispatch_loop rest
       | [] -> dispatch_loop [])
  | `Done result ->
      println "Task complete: %d" result;
      (* Handle result *)
      dispatch_loop remaining_tasks
in
dispatch_loop my_tasks
```

---

## File System Operations

### Path Type

Always use `Path.t` for filesystem paths:

```ocaml
open Std

(* Create paths *)
let home = Path.v "/home/user" in
let config = home / Path.v ".config" / Path.v "app.toml" in

(* Path components *)
Path.basename config         (* "app.toml" *)
Path.parent config           (* Some "/home/user/.config" *)
Path.extension config        (* Some "toml" *)

(* Modify paths *)
Path.with_extension config "yaml"  (* "/home/user/.config/app.yaml" *)

(* Convert *)
Path.to_string config        (* "/home/user/.config/app.toml" *)
```

### File Operations

```ocaml
open Std

(* Read entire file *)
let content = Fs.read (Path.v "input.txt")
  |> Result.expect ~msg:"Failed to read file"
in

(* Write file *)
Fs.write "Hello, world!" (Path.v "output.txt")
  |> Result.expect ~msg:"Failed to write file";

(* Append to file *)
Fs.append "\nNew line" (Path.v "output.txt")
  |> Result.expect ~msg:"Failed to append";

(* Check existence *)
if Fs.exists (Path.v "data.json") |> Result.unwrap_or ~default:false then
  println "File exists"
```

### Directory Operations

```ocaml
(* Create directory *)
Fs.create_dir (Path.v "output")
  |> Result.expect ~msg:"Failed to create directory";

(* Create directory tree *)
Fs.create_dir_all (Path.v "output/results/2024")
  |> Result.expect ~msg:"Failed to create directories";

(* Read directory *)
match Fs.read_dir (Path.v "src") with
| Ok iter ->
    Iter.MutIterator.iter (fun path ->
      println "Found: %s" (Path.to_string path)
    ) iter
| Error e ->
    eprintln "Error reading directory"

(* Remove directory *)
Fs.remove_dir (Path.v "old_output")
  |> Result.expect ~msg:"Failed to remove directory";
```

---

## Writing Tests

### Test Structure

```ocaml
(* tests/test_my_module.ml *)
open Std

let tests = [
  Test.case "addition works" (fun () ->
    let result = MyModule.add 2 3 in
    Test.assert_equal ~expected:5 ~actual:result;
    Ok ()
  );
  
  Test.case "handles errors" (fun () ->
    let result = MyModule.divide 10 0 in
    Test.assert_error result;
    Ok ()
  );
  
  Test.skip "not ready yet" (fun () ->
    (* This test will be skipped *)
    Ok ()
  );
  
  Test.todo "implement validation test";
]

let () =
  Miniriot.run @@ fun () ->
  Test.Cli.main ~name:"my-module" ~tests ~args:Env.args
```

### Test Assertions

```ocaml
(* Equality *)
Test.assert_equal ~expected:5 ~actual:(2 + 3)

(* Boolean *)
Test.assert_true (x > 0)
Test.assert_false (x < 0)

(* Result types *)
Test.assert_ok result
Test.assert_error result

(* In tests, use expect/unwrap aggressively *)
let value = compute ()
  |> Result.expect ~msg:"computation failed" in

let item = find_item id
  |> Option.expect ~msg:"item should exist" in
```

### Running Tests

```bash
# Run all tests
tusk test

# Run specific package tests
tusk test my-package

# Run with timeout
timeout 60 tusk test
```

---

## Creating New Packages

### Package Structure

```
packages/my-package/
├── tusk.toml
├── src/
│   ├── my_package.ml       # Main module
│   └── my_package.mli      # Interface (optional but recommended)
├── tests/
│   └── test_my_package.ml
└── examples/
    └── example_usage.ml
```

### Basic tusk.toml

```toml
[package]
name = "my-package"
version = "0.1.0"

[lib]
path = "src/my_package.ml"

[[bin]]
name = "example"
path = "examples/example_usage.ml"

[dependencies]
std = { path = "../std" }
another-package = { path = "../another-package" }
```

### Add to Workspace

Edit root `tusk.toml`:

```toml
[workspace]
members = [
  "packages/existing-package",
  "packages/my-package",  # Add this
  # ...
]
```

### Building

```bash
# Build everything
tusk build

# Build specific package
tusk build my-package

# Build binary
tusk build example
```

---

## Interface Design Patterns

### Abstract Types

Prefer abstract types in `.mli` files for encapsulation:

```ocaml
(* my_module.mli *)
type t
(* Abstract - implementation hidden *)

val create : int -> t
val get : t -> int

(* my_module.ml *)
type t = { value : int }

let create value = { value }
let get t = t.value
```

### Module Signatures

```ocaml
(* my_module.mli *)
module Config : sig
  type t
  
  val default : t
  val load : Path.t -> (t, string) Result.t
  val save : t -> Path.t -> (unit, string) Result.t
end

(* my_module.ml *)
module Config = struct
  type t = {
    port : int;
    host : string;
  }
  
  let default = { port = 8080; host = "localhost" }
  
  let load path =
    Fs.read path
    |> Result.and_then Data.Json.parse
    |> Result.map parse_config
  
  let save config path =
    let json = config_to_json config in
    Fs.write (Data.Json.to_string json) path
end
```

### Functors (Rare in Riot)

Functors are used sparingly. Most generic code uses polymorphism instead:

```ocaml
(* Prefer this *)
let process : 'a list -> 'b list = ...

(* Over this *)
module type PROCESSOR = sig
  type t
  val process : t list -> t list
end

module Make(P : PROCESSOR) = struct
  (* ... *)
end
```

---

## Common Patterns & Idioms

### The Canonical Actor Pattern

Based on `coordinator.ml` reference:

```ocaml
open Std

(* Types at top *)
type state = {
  immutable_field : string;
  mutable field : int;
}

type Message.t +=
  | Command1 of { data : int }
  | Command2 of { flag : bool }

(* Mutually recursive helpers *)
let rec loop state =
  let selector = function
    | Command1 _ -> Some `Cmd1
    | Command2 _ -> Some `Cmd2
    | _ -> None
  in
  
  if should_terminate state then
    ()
  else
    match receive ~selector () with
    | `Cmd1 data -> handle_command1 state data
    | `Cmd2 flag -> handle_command2 state flag

and handle_command1 state data =
  (* Process Command1 *)
  state.mutable_field <- state.murable_field + data.data;
  loop state  (* Tail-recursive *)

and handle_command2 state flag =
  (* Process Command2 *)
  if flag then
    println "Flag is true"
  else
    println "Flag is false";
  loop state  (* Tail-recursive *)

(* Initialization *)
let init ~name =
  let state = {
    immutable_field = name;
    mutable_field = cell 0;
  } in
  loop state
```

### Scoped Opens

Use scoped opens for clarity in small sections:

```ocaml
(* Local scope *)
let process_data data =
  let open Collections in
  let vec = Vector.of_list data in
  Vector.map transform vec

(* Or inline *)
let result =
  let open Result in
  compute ()
  |> map transform
  |> and_then validate
```

### Pattern Matching Inline

```ocaml
(* In log messages *)
Log.info "Processing: %s" (match status with
  | Ready -> "ready"
  | Waiting -> "waiting"
  | Done -> "done"
);
```

### Prefer Vector.push Over List Append

```ocaml
(* GOOD - O(1) *)
let v = Vector.create () in
Vector.push v item;

(* BAD - O(n) *)
let lst = ref [] in
lst := !lst @ [item]
```

### Keep Functions Tail-Recursive

```ocaml
(* Tail-recursive *)
let rec process_all items acc =
  match items with
  | [] -> acc
  | x :: xs -> process_all xs (transform x :: acc)

(* Not tail-recursive - avoid *)
let rec process_all items =
  match items with
  | [] -> []
  | x :: xs -> transform x :: process_all xs
```

---

## Build System (Tusk)

### Common Commands

```bash
# Build
tusk build                  # Build all
tusk build my-package       # Build specific package
tusk build my-binary        # Build specific binary

# Test
tusk test                   # Run all tests
tusk test my-package        # Run package tests

# Clean
tusk clean                  # Remove build artifacts

# Format
tusk fmt                    # Format code

# Completions
tusk completions --packages
tusk completions --binaries
tusk completions --tests
```

### Dependency Management

Dependencies are specified in `tusk.toml`:

```toml
[dependencies]
# Local workspace dependency
std = { path = "../std" }

# External dependency (if supported)
# some-lib = { version = "1.0.0" }
```

### Build Profiles

```toml
[profile.debug]
kind = "native"
opt_level = 0
debug = true

[profile.release]
kind = "native"
opt_level = 3
debug = false
```

### Platform-Specific Configuration

```toml
[target.macos]
cc_flags = ["-framework", "CoreFoundation"]
ld_flags = ["-L/opt/homebrew/opt/openssl/lib"]

[target.linux]
cc_flags = []
ld_flags = ["-lssl", "-lcrypto"]
```

---

## Appendix: Quick Reference

### Most Common Std Modules

| Module | Purpose | Key Functions |
|--------|---------|---------------|
| Result | Error handling | `map`, `and_then`, `expect`, `unwrap_or` |
| Option | Optional values | `map`, `and_then`, `unwrap_or`, `is_some` |
| Vector | Growable array | `push`, `pop`, `get`, `map`, `iter` |
| HashMap | Hash table | `insert`, `find`, `remove`, `iter` |
| Path | Filesystem paths | `v`, `join` (`/`), `basename`, `extension` |
| Fs | File operations | `read`, `write`, `exists`, `create_dir_all` |
| Log | Logging | `error`, `warn`, `info`, `debug` |
| Agent | State server | `start`, `get`, `update`, `cast` |
| Process | Actors | `spawn`, `send`, `receive`, `self` |

### File Checklist

When creating a new `.ml` file:

- [ ] `open Std` at the top
- [ ] Use `Path.t` for paths
- [ ] Use `cell` for mutable values (not `ref`)
- [ ] Use `mutable` keyword for record fields
- [ ] Return `Result.t` for fallible operations
- [ ] Return `Option.t` for maybe-absent values
- [ ] Use `expect`/`unwrap` in tests
- [ ] Make recursive functions tail-recursive
- [ ] Add `.mli` file for public modules

---

## Getting Help

- Check `packages/std/src/*.mli` for API documentation
- Look at examples in `packages/*/examples/`
- Read tests in `packages/*/tests/` for usage patterns
- Review `ARCHITECTURE.md` for system overview

---

**Welcome to Riot ML! Happy coding!** 🚀
