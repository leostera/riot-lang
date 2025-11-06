(** # Std - Riot's standard library

    A comprehensive standard library providing:
    - Modern error handling with [Result] and [Option]
    - Type-safe filesystem operations with [Path] and [Fs]
    - Efficient collections ([HashMap], [HashSet], [Vector], [Queue], [Deque])
    - Time measurement ([Duration], [Instant], [SystemTime])
    - Data formats ([Json], [Toml], [Sexp])
    - HTTP utilities ([Net.Http])
    - Structured logging ([Log])
    - Process management ([Command])
    - Concurrent task execution ([Task], [WorkerPool])

    ## Quick Start

    ```ocaml open Std

    (* File operations *) let content = Fs.read (Path.v "config.toml") |>
    Result.expect ~msg:"Config not found" in

    (* Collections *) let map = Collections.HashMap.create () in
    Collections.HashMap.insert map "key" "value" |> ignore;

    (* Logging *) Log.set_level Log.Info; Log.info "Application started"

    (* Time measurement *) let start = Time.Instant.now () in
    expensive_operation (); let elapsed = Time.Instant.elapsed start ```

    ## Module Organization

    ### Process Management (OTP Patterns)
    - [Agent] - Simple state wrapper
    - [GenServer] - Generic server behavior
    - [Supervisor] - Process supervision

    ### Core Types
    - [Result] - Error handling
    - [Option] - Optional values
    - [Path] - Type-safe filesystem paths
    - [UUID] - Universally unique identifiers
    - [String] - UTF-8 strings
    - [Buffer] - String building
    - [Char] - Character operations

    ### Collections
    - [Collections] - Data structures
    - [HashMap] - Hash tables
    - [HashSet] - Unique values
    - [Vector] - Growable arrays
    - [Queue] - FIFO queues
    - [Deque] - Double-ended queues

    ### Time & Date
    - [Time] - Time utilities
    - [Duration] - Time spans
    - [Instant] - Monotonic time
    - [SystemTime] - Wall-clock time
    - [Datetime] - Calendar operations

    ### I/O & System
    - [Fs] - Filesystem operations
    - [Command] - Process execution
    - [Task] - Async tasks
    - [Log] - Structured logging
    - [Env] - Environment variables
    - [System] - System information

    ### Data Formats
    - [Data] - Data parsing/serialization
    - [Json] - JSON
    - [Toml] - TOML
    - [Sexp] - S-expressions
    - [Csv] - CSV (Comma-Separated Values)
    - [Base16] - Hexadecimal encoding
    - [Base32] - Base32 encoding
    - [Base64] - Base64 encoding
    - [Base85] - Ascii85 encoding

    ### Networking
    - [Net] - Network I/O
    - [Http] - HTTP client/server
    - [Uri] - URL parsing

    ### Utilities
    - [ArgParser] - Command-line arguments
    - [Version] - Semantic versioning
    - [Crypto] - Cryptographic hashing
    - [Graph] - Graph visualization
    - [WorkerPool] - Parallel execution

    ### Low-Level
    - [Iter] - Iteration and cursor utilities
    - [Cell] - Mutable cells
    - [Exception] - Exception handling *)

module Agent = Agent
(** Simple state wrapper for concurrent access *)

module ArgParser = Arg_parser
(** Command-line argument parsing *)

module Char = Char
(** Character operations *)

module Collections = Collections
(** Collection data structures *)

module Command = Command
(** Process spawning and management *)

module Crypto = Crypto
(** Cryptographic hashing *)

module Data = Data
(** Data format parsing and serialization *)

module Datetime = Datetime
(** Calendar date and time operations *)

module Diff = Diff
(** Difference computation for data structures *)

module Env = Env
(** Environment variables and system info *)

module Exception = Exception
(** Exception handling utilities *)

module Fs = Fs
(** Filesystem operations *)

(* module GenServer = Gen_server *)
(** Generic server behavior with type-safe functor API - V1 has existential escape issues *)

module Graph = Graph
(** Graph data structures and visualization *)

module IO = IO
(** Generic I/O abstractions - Reader, Writer, and Iovec *)

module Iter = Iter
(** Iteration and cursor utilities for sequences and parsing *)

module Log = Log
(** Structured logging *)

module Net = Net
(** Network I/O and protocols *)

module Option = Option
(** Optional value handling *)

module Path = Path
(** Type-safe filesystem paths *)

module Ref = Ref
(** Unique, opaque, type-witnessing references *)

module Result = Result
(** Error handling with Result type *)

module String = String
(** UTF-8 string operations *)

module Supervisor = Supervisor
(** OTP-style process supervision *)

module Sync = Sync
(** Synchronization primitives *)

module System = System
(** System information and operations *)

module Task = Task
(** Asynchronous task execution *)

module Telemetry = Telemetry
(** Telemetry framework *)

module Test = Test
(** Test framework with TAP output *)

module Time = Time
(** Time measurement and duration *)

module Unicode = Unicode
(** Unicode text processing (runes, graphemes, width, segmentation) *)

module UUID = Uuid
(** Universally unique identifiers *)

module Version = Version
(** Semantic versioning *)

module WorkerPool = Worker_pool
(** Parallel execution with worker pools *)

val panic : string -> 'a
(** Panic with a message - raises an uncatchable exception *)

val cell : 'a -> 'a Sync.Cell.t
(** Create a mutable cell with the given value *)

val format : ('a, unit, string, string) format4 -> 'a
(** Format string helper - alias for format *)

val print : ('a, unit, string, unit) format4 -> 'a
(** Print to stdout with immediate flush *)

val println : ('a, unit, string, unit) format4 -> 'a
(** Print to stdout with newline and immediate flush *)

val eprint : ('a, unit, string, unit) format4 -> 'a
(** Print to stderr with immediate flush *)

val eprintln : ('a, unit, string, unit) format4 -> 'a
(** Print to stderr with newline and immediate flush *)

val todo : string -> 'a
(** Mark code as TODO with a message - panics when called *)

val unimplemented : unit -> 'a
(** Mark code as unimplemented - panics when called *)

(** {1 Collection Type Aliases} *)

type 'a vec = 'a Collections.Vector.t
(** Vector type alias - dynamically-sized array. Use [vec] to create from a list. *)

type 'a queue = 'a Collections.Queue.t
(** Queue type alias - FIFO queue. Use [queue] to create from a list. *)

type 'a set = 'a Collections.HashSet.t
(** Set type alias - hash-based set. Use [set] to create from a list. *)

type ('k, 'v) map = ('k, 'v) Collections.HashMap.t
(** Map type alias - hash-based map. Use [map] to create from a list of pairs. *)

val vec : 'a list -> 'a vec
(** Create a vector from a list. Example: [let v = vec [1; 2; 3]] *)

val queue : 'a list -> 'a queue
(** Create a queue from a list. Example: [let q = queue [1; 2; 3]] *)

val set : 'a list -> 'a set
(** Create a set from a list. Example: [let s = set [1; 2; 3]] *)

val map : ('k * 'v) list -> ('k, 'v) map
(** Create a map from a list of key-value pairs. Example: [let m = map [("a", 1); ("b", 2)]] *)

(** {1 Process Management} *)

module Pid = Pid
(** Process identifiers *)

module Message = Message
(** Extensible message type for actor communication *)

module Process = Process
(** Process state and operations *)

exception Receive_timeout
(** Raised when a receive operation times out *)

exception Syscall_timeout
(** Raised when a syscall operation times out *)

type 'msg selector = 'msg Miniriot.selector
(** Message selector type *)

val self : unit -> Pid.t
(** Get the PID of the currently running process *)

val spawn : (unit -> (unit, Process.exit_reason) result) -> Pid.t
(** Spawn a new process *)

val spawn_link : (unit -> (unit, Process.exit_reason) result) -> Pid.t
(** Spawn a new process linked to the current process *)

val send : Pid.t -> Message.t -> unit
(** Send a message to a process *)

val receive : selector:'value selector -> ?timeout:float -> unit -> 'value
(** Receive a message using a selector *)

val receive_any : ?timeout:float -> unit -> Message.t
(** Receive any message *)

val yield : unit -> unit
(** Yield control to the scheduler *)

val shutdown : status:int -> unit
(** Shutdown the runtime with the given exit status *)

module Timer = Timer
(** Timer operations *)

(** {1 Application Management} *)

module Application = Application
(** Application supervision with dependency resolution *)

val start : apps:Application.t list -> unit
(** Start the runtime with applications.
    Applications are started in dependency order.
    
    Uses [Miniriot.run] under the hood. *)
