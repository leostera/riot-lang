# Prompt for working on this repository

## CRITICAL: GIT SAFETY RULES - NEVER VIOLATE THESE

### NEVER USE `git reset --hard` WITHOUT CHECKING FIRST
**THIS IS THE MOST IMPORTANT RULE.** Using `git reset --hard` will PERMANENTLY DESTROY:
- ALL uncommitted changes in the working directory
- ALL staged changes
- ALL untracked files that other agents or users may have been working on
- WEEKS OR MONTHS of work can be lost in an instant

### ALWAYS follow these git safety practices:
1. **BEFORE any destructive git operation:**
   - Run `git status` to check for uncommitted changes
   - Run `git stash` to save any uncommitted work
   - Run `git diff` to see what changes exist
   - Check if other agents might have work in progress

2. **NEVER use these commands without explicit user permission:**
   - `git reset --hard`
   - `git clean -fd`
   - `git push --force`
   - Any command that rewrites history

3. **SAFE alternatives to use instead:**
   - Use `git revert` to undo commits (creates a new commit, preserves history)
   - Use `git reset --soft` to undo commits but keep changes staged
   - Use `git stash` to temporarily save work
   - Use `git checkout -- <file>` to discard changes to specific files only
   - Use `git reset HEAD~1` (without --hard) to undo last commit but keep changes

4. **If you need to undo changes:**
   - First ALWAYS check `git status`
   - Save any important work with `git stash`
   - Use the least destructive method possible
   - Prefer creating new commits over destroying old ones

**Remember:** Once `git reset --hard` is run, the changes are GONE FOREVER. There is no undo. You will destroy work that cannot be recovered. This is not a joke or exaggeration - you WILL lose data permanently.

## System Prompt

- always explain your reasoning in steps
- if you think you're overcomplicating things, ask me
- scrutinize my inputs and corrections
- always go back to the project root after `cd`-ing somewhere
- you will only call `tusk` 


## Writing OCaml with Tusk

* NEVER reference modules by their namespaced name. For ex. Std.Crypto.Algo.Sha256 is also available as Std__Crypto__Algo__Sha256 but it should NEVER be referenced as such

## Writing OCaml with Std

* Always `open Std` at the top
* Use Path.t for paths instead of strings, and create them with Path.of_string only when you want to handle the error case of not valid paths -- otherwise use Path.v to make new paths from strings
* If you see Path.to_string, consider keeping the Path.t around until it needs printing instead of turning it into a string for manipulation
* Prefer Result.expect ~msg and Option.expect ~msg over Result.unwrap or Option.unwrap

## Standard Library Usage Guide

### CRITICAL: Never use OCaml's stdlib directly
**NEVER** use `Stdlib`, `Unix`, `Sys` or any OCaml standard library modules directly. This project provides three layers of custom standard libraries that must be used instead:

1. **Kernel** (`packages/kernel`) - Low-level system interface
2. **Std** (`packages/std`) - High-level standard library
3. **Miniriot** (`packages/miniriot`) - Actor-based concurrency runtime

### Library Hierarchy and Usage

#### 1. Kernel Library (Low-level) - RARELY USED
**Only use Kernel for very low-level system programming**. Most applications should never touch Kernel directly - use Std instead.

Use `Kernel` ONLY when you need:
- Direct system calls and I/O operations (when Std.Fs isn't sufficient)
- Basic OCaml types without stdlib dependency (for bootstrapping)
- Async I/O primitives (when Std.Task isn't sufficient)
- Low-level networking (TCP streams, sockets) - use Std.Net instead

```ocaml
open Kernel

(* File I/O *)
let content = Fs.File.read_to_string "file.txt"

(* Async operations *)
let result = Async.syscall (fun () ->
  (* async operation *)
)

(* Networking *)
let stream = Net.Tcp_stream.connect addr
```

Key modules in Kernel:
- `Kernel.Fs` - Filesystem operations
- `Kernel.Net` - Networking (TCP, sockets)
- `Kernel.Async` - Async I/O primitives
- `Kernel.Crypto` - Cryptographic hashing
- `Kernel.System` - System operations
- `Kernel.Time` - Time operations
- `Kernel.IO` - I/O operations
- Core types: `String`, `List`, `Option`, `Buffer`, `Bytes`, `Array`, `Hashtbl`

#### 2. Std Library (High-level) - PREFERRED
**Always use Std for new code** unless you specifically need low-level Kernel features. Std is the primary library for 99% of application development.

```ocaml
open Std

(* Path manipulation - type-safe paths *)
let path = Path.v "/home/user/file.txt"
let parent = Path.parent path |> Option.unwrap
let new_path = Path.v "src" / Path.v "main.ml"

(* Filesystem operations *)
let content = Fs.read (Path.v "config.toml")
  |> Result.expect ~msg:"Failed to read config"

(* Command execution *)
let output = Command.make "ls" ~args:["-la"]
  |> Command.output
  |> Result.expect ~msg:"Command failed"

(* Error handling with Result *)
let process_file path =
  Fs.read path
  |> Result.map String.trim
  |> Result.and_then parse_content
  |> Result.map_err (fun e -> format "Error in %s: %s" (Path.to_string path) e)

(* Collections *)
let map = Collections.HashMap.create ()
let vec = Collections.Vector.create ()
```

Key modules in Std:
- `Std.Path` - Type-safe filesystem paths (ALWAYS use instead of strings)
- `Std.Fs` - High-level filesystem operations
- `Std.Command` - Process spawning and management
- `Std.Result` - Rust-like Result type for error handling
- `Std.Option` - Rust-like Option type
- `Std.Collections` - HashMap, HashSet, Vector, Deque
- `Std.Task` - Async task execution
- `Std.Log` - Structured logging
- `Std.Crypto` - Cryptographic operations (SHA256, SHA512, MD5)
- `Std.Net` - High-level networking with HTTP support
- `Std.Data.Json` - JSON parsing/serialization
- `Std.Data.Toml` - TOML parsing
- `Std.WorkerPool` - Parallel execution with worker pools
- `Std.Time` - Duration, Instant, SystemTime
- `Std.System` - System information and operations

#### 3. Miniriot Library (Actor concurrency) - PROCESS ARCHITECTURE
**Use Miniriot for all concurrent programming**. Applications should be built as collaborating processes that communicate via messages, following Erlang-style process architecture.

```ocaml
open Miniriot

(* Define typed internal messages *)
type worker_msg = Work of string | Result of int

type Message.t += WorkerMsg of worker_msg

(* Process implementation *)
let rec loop state =
  let selector msg =
    match msg with
    | WorkerMsg msg -> `select msg  (* Only worker_msg values *)
    | _ -> `skip
  in
  match receive ~selector () with  (* Type-safe message handling *)
  | Work task ->
      let result = process_task task in
      send (self ()) (WorkerMsg (Result result));
      loop state

let init () =
  let state = () in  (* Worker state could include task queue, etc. *)
  loop state

let start () =
  spawn (fun () -> init ())

(* Spawn and send messages *)
let worker = start () in
send worker (WorkerMsg (Work "task1"))

(* Receive with selector *)
let result = receive ~selector:(function
  | WorkerMsg (Result n) -> `select n
  | _ -> `skip
) ()
```

Key features in Miniriot:
- `spawn` - Create new actor processes
- `send` - Send messages between actors
- `receive` / `receive_any` - Receive messages
- `self` - Get current process ID
- `yield` - Yield control to scheduler
- `syscall` - Perform I/O operations without blocking

### Erlang-Style Process Architecture Patterns

**Module Message Hiding**: A module should hide its internal message types and provide functions to generate them:

```ocaml
(* mymodule.ml *)
open Miniriot

(* Internal message types - not exported *)
type Message.t +=
  | DoWork of string
  | WorkDone of result

(* Public API - functions that send messages *)
let do_work pid task =
  send pid (DoWork task)

let get_result pid =
  receive ~selector:(function
    | WorkDone result -> `select result
    | _ -> `skip
  ) ()
```

**Process-Based Application Structure**: Build applications as collaborating processes:

```ocaml
(* supervisor.ml *)
type supervisor_msg = WorkerFailed of Pid.t | Shutdown

type Message.t += SupervisorMsg of supervisor_msg

let rec supervise workers =
  let selector msg =
    match msg with
    | SupervisorMsg msg -> `select msg  (* Only supervisor_msg values *)
    | _ -> `skip
  in
  match receive ~selector () with  (* Type-safe message handling *)
  | WorkerFailed pid ->
      Log.error "Worker %a failed, restarting" Pid.pp pid;
      let new_worker = spawn worker_process in
      supervise (new_worker :: workers)
  | Shutdown -> ()

let init () =
  let worker1 = spawn worker_process in
  let worker2 = spawn worker_process in
  let monitor = spawn monitor_process in
  let workers = [worker1; worker2] in
  supervise workers

let start () =
  spawn (fun () -> init ())
```

**Message Passing Over Shared State**: Use message passing instead of shared mutable state:

```ocaml
(* GOOD - Message passing with typed selectors *)
type internal_msg = GetValue | SetValue of int | ValueResponse of int

type Message.t += StateMsg of internal_msg

let rec loop value =
  let selector msg =
    match msg with
    | StateMsg msg -> `select msg
    | _ -> `skip
  in
  match receive ~selector () with  (* Only receives internal_msg values *)
  | GetValue ->
      send (self ()) (StateMsg (ValueResponse value));
      loop value
  | SetValue new_val ->
      loop new_val
  | ValueResponse _ -> loop value  (* Handle responses if needed *)

let init initial_value =
  loop initial_value

let start ?(initial_value=0) () =
  spawn (fun () -> init initial_value)

(* BAD - Shared mutable state *)
let shared_counter = ref 0  (* Don't do this *)

(* BAD - Using receive_any with manual filtering *)
let bad_process () =
  let rec loop () =
    match receive_any () with  (* Receives any Message.t *)
    | StateMsg GetValue -> ...  (* Manual unwrapping *)
    | StateMsg (SetValue n) -> ...
    | _ -> loop ()  (* Skip unrelated messages *)
  in loop ()
```

### Library-Specific LLM Guidance

#### Collections (CRITICAL)
**NEVER use OCaml stdlib collections!** Always use Std.Collections instead:

| OCaml Stdlib (NEVER USE) | Std.Collections (ALWAYS USE) |
|---------------------------|------------------------------|
| `Hashtbl` | `Std.Collections.HashMap` |
| `Hashtbl.create` | `HashMap.create ()` |
| `Hashtbl.add` | `HashMap.insert` |
| `Hashtbl.find` | `HashMap.get` (returns Option) |
| `Hashtbl.mem` | `HashMap.contains_key` |
| `Set` | `Std.Collections.HashSet` |
| `Queue` | `Std.Collections.Queue` or `Deque` |
| `Stack` | `Std.Collections.Vector` (use as stack) |
| `Array` | `Std.Collections.Vector` for growable arrays |

#### Filesystem Operations (CRITICAL)
**NEVER use Unix, Sys, or Stdlib filesystem functions!** Always use Std.Fs:

| OCaml Stdlib/Unix (NEVER USE) | Std.Fs (ALWAYS USE) |
|--------------------------------|---------------------|
| `Unix.open_file` | `Std.File.open_` |
| `Sys.file_exists` | `Std.Fs.exists` |
| `Sys.is_directory` | `Std.Fs.is_dir` |
| `Sys.remove` | `Std.Fs.remove_file` |
| `Unix.mkdir` | `Std.Fs.create_dir` |
| `Unix.rmdir` | `Std.Fs.remove_dir` |
| `open_in` / `open_out` | `Std.File` functions |
| `input_line` | `Std.File.read_lines` |
| `Sys.readdir` | `Std.Fs.read_dir` |
| `Unix.rename` | `Std.Fs.rename` |
| `Unix.stat` | `Std.Fs.metadata` |
| `Unix.chmod` | `Std.Fs.set_permissions` |

### Common Patterns and Best Practices

#### Path Handling
```ocaml
(* GOOD - Type-safe paths *)
let config_path = Path.v "config" / Path.v "settings.toml"
let exists = Fs.exists config_path |> Result.unwrap_or ~default:false

(* BAD - String paths *)
let config_path = "config/settings.toml"  (* Don't use strings for paths *)
```

#### Error Handling
```ocaml
(* GOOD - Explicit error handling *)
let read_config () =
  Fs.read (Path.v "config.toml")
  |> Result.map parse_toml
  |> Result.expect ~msg:"Config file required"

(* BAD - Ignoring errors *)
let config = Fs.read (Path.v "config.toml") |> Result.unwrap  (* No context *)
```

#### Collections
```ocaml
(* Use Std collections, not OCaml stdlib *)
let map = Collections.HashMap.create ()  (* GOOD *)
let map = Hashtbl.create 10             (* BAD - stdlib *)

let vec = Collections.Vector.create ()   (* GOOD *)
let list = []                           (* OK for simple cases *)
```

#### Async Operations
```ocaml
(* For I/O-bound parallel work *)
let results = Task.async (fun () -> expensive_io ())
  |> Task.await
  |> Result.expect ~msg:"Task failed"

(* For CPU-bound parallel work *)
let results = WorkerPool.SimpleWorkerPool.run
  ~concurrency:8
  ~tasks:items
  ~fn:process_item
  ()
```

#### Logging
```ocaml
(* Structured logging *)
Log.set_level Log.Debug;
Log.info "Starting server on port %d" port;
Log.error "Failed to connect: %s" error_msg;
```

### Process-Based Architecture Principles

**Build applications as collaborating processes, not threads with shared state:**

1. **Message Passing Over Shared State**: Use `send`/`receive` instead of mutexes and shared variables
2. **Process Isolation**: Each process manages its own state - no shared mutable data
3. **Failure Isolation**: Process crashes don't bring down the entire application
4. **Horizontal Scalability**: Easy to spawn multiple instances of processes
5. **Supervisor Trees**: Parent processes monitor and restart child processes on failure

**Message Handling Patterns**: Always use typed selectors over receive_any:

```ocaml
(* GOOD - Typed internal messages with selectors *)
type internal_msg = | DoWork of task | WorkDone of result

type Message.t += MyMsg of internal_msg

let process () =
  let rec loop () =
    let selector msg =
      match msg with
      | MyMsg msg -> `select msg  (* Type-safe: only internal_msg values *)
      | _ -> `skip
    in
    match receive ~selector () with  (* Only receives internal_msg *)
    | DoWork task -> handle_work task; loop ()
    | WorkDone result -> handle_result result; loop ()
  in loop ()

(* BAD - Using receive_any with manual pattern matching *)
let bad_process () =
  let rec loop () =
    match receive_any () with  (* Receives any Message.t *)
    | MyMsg (DoWork task) -> ...  (* Manual unwrapping *)
    | MyMsg (WorkDone result) -> ...
    | OtherMsgType _ -> loop ()   (* Skip unrelated messages *)
    | _ -> loop ()
  in loop ()
```

**Process Structure Pattern**: Use proper initialization and state management:

```ocaml
(* counter.ml *)
type msg = Incr | Decr
type Message.t += Counter of msg
type state = { i : int }

let rec loop state =
  let selector msg =
    match msg with
    | Counter msg -> `select msg
    | _ -> `skip
  in
  match receive ~selector () with
  | Incr -> handle_incr state
  | Decr -> handle_decr state

and handle_incr state =
  let new_state = { i = state.i + 1 } in
  loop new_state

and handle_decr state =
  let new_state = { i = state.i - 1 } in
  loop new_state

let init initial_value =
  let state = { i = initial_value } in
  loop state

let start ?(initial_value=0) () =
  spawn (fun () -> init initial_value)
```

**Complex Message Handling**: When the main loop becomes big, make it a dispatcher:

```ocaml
(* complex_process.ml *)
type msg = | DoComplexWork of data | HandleResult of result | Shutdown
type Message.t += ComplexMsg of msg
type state = { pending_work : work list; results : result list }

let rec loop state =
  let selector msg =
    match msg with
    | ComplexMsg msg -> `select msg
    | _ -> `skip
  in
  match receive ~selector () with
  | DoComplexWork data -> handle_complex_work state data
  | HandleResult result -> handle_result state result
  | Shutdown -> handle_shutdown state

and handle_complex_work state data =
  (* Complex work processing logic *)
  let updated_state = process_work state data in
  loop updated_state

and handle_result state result =
  (* Result handling logic *)
  let updated_state = store_result state result in
  loop updated_state

and handle_shutdown state =
  (* Cleanup logic *)
  cleanup state;
  ()  (* Exit process *)

let init () =
  let state = { pending_work = []; results = [] } in
  loop state

let start () =
  spawn (fun () -> init ())
```

**API Design Pattern**: Hide internal message types, expose functions:

```ocaml
(* GOOD - Encapsulated API *)
MyModule.do_work pid task
MyModule.get_result pid

(* BAD - Exposed message types *)
send pid (MyModule.DoWork task)
```

### Module Resolution Rules

1. **Never use namespaced names**: Use `Std.Crypto` not `Std__Crypto`
2. **Always open Std at the top**: This is the primary library
3. **Import specific modules when needed**: `open Std.Collections`
4. **Use module aliases for clarity**: `module Json = Std.Data.Json`

### Typical File Structure

```ocaml
(* mymodule.ml *)
open Std  (* Always first *)
open Miniriot  (* If using actors *)

(* Module aliases *)
module Json = Data.Json
module HashMap = Collections.HashMap

(* Internal message types - not exported in .mli *)
type internal_msg = ProcessConfig of Path.t | ConfigLoaded of config | ConfigError of string

type Message.t += ConfigMsg of internal_msg

(* Public API - functions that send messages *)
let load_config pid config_path =
  send pid (ConfigMsg (ProcessConfig config_path))

let get_config pid =
  receive ~selector:(function
    | ConfigMsg (ConfigLoaded config) -> `select (Ok config)
    | ConfigMsg (ConfigError msg) -> `select (Error msg)
    | _ -> `skip
  ) ()

(* Type definitions *)
type config = {
  port : int;
  host : string;
}

(* Process implementation *)
let rec loop state =
  let selector msg =
    match msg with
    | ConfigMsg msg -> `select msg  (* Only select our internal messages *)
    | _ -> `skip
  in
  match receive ~selector () with  (* Only receives internal_msg values *)
  | ProcessConfig path ->
      (match load_and_parse_config path with
       | Ok config ->
           send (self ()) (ConfigMsg (ConfigLoaded config))
       | Error msg ->
           send (self ()) (ConfigMsg (ConfigError msg)));
      loop state

let init () =
  let state = () in  (* Could be more complex state *)
  loop state

let start () =
  spawn (fun () -> init ())

(* Main code - spawn processes, not direct computation *)
let main () =
  let config_loader_pid = start () in
  let config_path = Path.v "config.json" in

  load_config config_loader_pid config_path;

  match get_config config_loader_pid with
  | Ok config ->
      Log.info "Loaded config: port=%d, host=%s" config.port config.host
  | Error msg ->
      Log.error "Failed to load config: %s" msg;
      shutdown 1
```
