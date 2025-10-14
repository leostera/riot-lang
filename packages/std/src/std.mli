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

    ### Core Types
    - [Result] - Error handling
    - [Option] - Optional values
    - [Path] - Type-safe filesystem paths
    - [String] - UTF-8 strings
    - [List] - Extended list utilities
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

module ArgParser = Arg_parser
(** Command-line argument parsing *)

module Buffer = Buffer
(** Growable string buffers *)

module Cell = Cell
(** Mutable cells for interior mutability *)

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

module Env = Env
(** Environment variables and system info *)

module Exception = Exception
(** Exception handling utilities *)

module Fs = Fs
(** Filesystem operations *)

module Graph = Graph
(** Graph data structures and visualization *)

module IO = IO
(** Generic I/O abstractions - Reader, Writer, and Iovec *)

module Iter = Iter
(** Iteration and cursor utilities for sequences and parsing *)

module List = List
(** Extended list utilities *)

module Log = Log
(** Structured logging *)

module Net = Net
(** Network I/O and protocols *)

module Option = Option
(** Optional value handling *)

module Path = Path
(** Type-safe filesystem paths *)

module Result = Result
(** Error handling with Result type *)

module String = String
(** UTF-8 string operations *)

module System = System
(** System information and operations *)

module Task = Task
(** Asynchronous task execution *)

module Test = Test
(** Test framework with TAP output *)

module Time = Time
(** Time measurement and duration *)

module Version = Version
(** Semantic versioning *)

module WorkerPool = Worker_pool
(** Parallel execution with worker pools *)

val panic : string -> 'a
(** Panic with a message - raises an uncatchable exception *)

val cell : 'a -> 'a Cell.t
(** Create a mutable cell with the given value *)

val format : ('a, unit, string, string) format4 -> 'a
(** Format string helper - alias for format *)

val print : ('a, unit, string, unit) format4 -> 'a
(** Print to stdout with immediate flush *)

val println : ('a, unit, string, unit) format4 -> 'a
(** Print to stdout with newline and immediate flush *)

val todo : string -> 'a
(** Mark code as TODO with a message - panics when called *)

val unimplemented : unit -> 'a
(** Mark code as unimplemented - panics when called *)
