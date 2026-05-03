(**
   Riot's Standard Library

   A complete standard library providing modern primitives for building robust,
   concurrent, fault-tolerant applications in OCaml.
*)

(**
   Simple concurrent state access

   Use Agent when you need a lightweight state server accessed from multiple processes.
   Perfect for counters, caches, shared configuration, or session storage.

   **Don't use when:** You need complex request/reply patterns → use GenServer

   **Examples:**
   - Shared application configuration
   - In-memory cache
   - Request counter / rate limiter
   - User session storage
*)
module Agent = Agent

(**
   Reading and writing archive containers

   Use Archive for tar archives and related reader/writer archive helpers.
*)
module Archive = Archive

(**
   Parsing command-line arguments

   Use ArgParser for building CLI tools with flags, options, and subcommands.

   **Examples:**
   - Build tools (like riot, cargo, npm)
   - Developer utilities
   - System administration scripts
*)
module ArgParser = Arg_parser

(**
   Fixed-size arrays

   Use Array for index-addressed collections where the size is known up front.
*)
module Array = Collections.Array

(**
   Standard benchmarking harness

   Use Bench for measuring and comparing performance of code.
   Simple, low-level benchmarking framework with basic statistics.

   **Examples:**
   - Comparing data structure performance
   - Measuring algorithm speed
   - Performance regression testing

   **See also:** {!Test} for unit testing
*)
module Bench = Bench

(**
   Boolean operations

   Extended bool operations with parsing, formatting, and utilities.
*)
module Bool = Bool

(**
   Character classification and conversion

   Extended character operations beyond stdlib.

   **See also:** {!Unicode.Rune} for Unicode code points
*)
module Char = Char

(**
   Common data structures (Vector, HashMap, Queue, etc)

   Parent module containing Vector, HashMap, HashSet, Queue, Deque, Heap, List, and Proplist.

   Use collections when you need:
   - Fast lookups → HashMap
   - Unique values → HashSet
   - Dynamic arrays → Vector
   - FIFO queues → Queue
   - Double-ended operations → Deque
   - Priority ordering → Heap
   - Functional lists → List
   - Duplicate-friendly key/value lists → Proplist
*)
module Collections = Collections

(**
   Streaming compression/decompression

   Use Compress for codec-style I/O transformations such as gzip streams.

   **See also:** {!Archive} for tar archives layered on top of readers.
*)
module Compress = Compress

(**
   Running external programs

   Use Command when you need to execute shell commands or external tools.

   **Examples:**
   - Running git commands
   - Invoking build tools
   - System administration tasks
   - Integration with external utilities

   **See also:** {!System} for system information
*)
module Command = Command

(**
   Application configuration management

   Use Config for type-safe, environment-aware configuration loading.

   **Examples:**
   - Loading server configuration
   - Database connection settings
   - Environment-specific configs (dev/test/prod)
   - Multi-app configuration files

   **Features:**
   - Namespaced TOML sections
   - Type-safe parsing
   - Environment detection
   - Deep merging
*)
module Config = Config

(**
   Cryptographic hashing

   Use Crypto for content-addressable storage, integrity verification, checksums.

   **Examples:**
   - Hash-based deduplication
   - Content verification
   - Cache keys
   - Merkle trees

   **WARNING:** NOT for password hashing - use proper KDFs like Argon2

   **Algorithms:** SHA-256 (recommended), SHA-512, MD5 (legacy only)
*)
module Crypto = Crypto

(**
   Parsers and writes for common data formats (JSON, XML, TOML, etc)

   Parent module for structured data formats.

   **Use cases:**
   - API communication → Json
   - Configuration files → Toml
   - Data export/import → Csv
   - Legacy systems → Xml
   - Lisp-like data → Sexp
*)
module Data = Data

(**
   Binary/text and numeric encodings

   Parent module for Base16/Hex, Base32, Base64, Base85, and Octal.

   **Use cases:**
   - Hex digests and binary inspection → Hex / Base16
   - Human-friendly binary transport → Base32
   - Binary-in-text payloads → Base64
   - Compact text encodings → Base85
   - Octal numeric fields → Octal
*)
module Encoding = Encoding

(**
   Low-level Gregorian calendar math

   Use Calendar when implementing date/time primitives that need calendar arithmetic.
*)
module Calendar = Calendar

(**
   Working with civil calendar dates

   Use Date for date-only values, whole-day arithmetic, and ISO 8601 dates.

   **Examples:**
   - Birthdays and anniversaries
   - Date-only config values
   - Day-based retention windows
*)
module Date = Date

(**
   Calendar dates and times

   Use DateTime for human-readable dates, date arithmetic, formatting.

   **Examples:**
   - Parsing user-input dates
   - Date calculations (add 3 months)
   - Timezone-aware operations
   - Calendar displays

   **See also:** {!Time.SystemTime} for timestamps, {!Time.Instant} for elapsed time
*)
module DateTime = DateTime

(**
   Computing differences between data structures

   Use Diff for change detection, audit logs, version control.

   **Examples:**
   - Configuration change tracking
   - Database record diffing
   - API response comparison
   - Undo/redo systems
*)
module Diff = Diff

(**
   Environment variables and system info

   Use Env for reading configuration from environment, detecting OS/arch.

   **Examples:**
   - Reading DATABASE_URL, API_KEY
   - Detecting production vs development
   - Platform-specific behavior
   - Container orchestration config
*)
module Env = Env

(**
   Exception rendering and backtrace capture

   Use Exception from packages above `std` when you need exception text or
   raw backtrace formatting without reaching into `Kernel`.
*)
module Exception = Exception

(**
   Floating-point operations

   Extended float operations with parsing and formatting.
*)
module Float = Float

(**
   Filesystem operations

   Use Fs for all file and directory operations with Result-based error handling.

   **Common operations:**
   - Reading/writing files
   - Creating directories
   - Copying/moving files
   - Querying metadata
   - Watching for changes

   **See also:** {!Path} for path manipulation, {!Fs.File} for streaming
*)
module Fs = Fs

(**
   Matching paths or names against glob patterns

   Use Glob when users provide gitignore-style or shell-style path patterns.
*)
module Glob = Glob

(**
   Graph data structures and visualization

   Use Graph for dependency graphs, build systems, workflow diagrams.

   **Modules:**
   - SimpleGraph → dependency tracking with topo sort
   - Dot → Graphviz visualization
   - Mermaid → Markdown-friendly diagrams
*)
module Graph = Graph

(**
   Generic I/O abstractions

   Use IO for Reader/Writer traits and vectored I/O operations.

   **Most users should use:** {!Fs} or {!Net} instead
*)
module IO = IO

(**
   Building owned heap strings

   Use StringBuilder when you intentionally want to accumulate text into an
   owned `string`. `Std.IO.Buffer` is the off-heap I/O buffer default; this
   module is the explicit heap-building boundary.
*)
module StringBuilder = StringBuilder

(**
   Integer operations

   Extended int operations with parsing, formatting, and utilities.
*)
module Int = Int

(** 32-bit integer operations *)
module Int32 = Int32

(**
   64-bit integer operations

   Use Int64 for timestamps, large numbers, file sizes.
*)
module Int64 = Int64

(**
   Iteration and parsing utilities

   Parent module for Iterator, MutIterator, Cursor, and MutCursor.

   **Use Iterator when:** You need functional, backtrackable iteration
   **Use MutIterator when:** You need efficient, single-pass iteration
   **Use Cursor when:** You're parsing borrowed slices with backtracking
   **Use MutCursor when:** You need fast, single-pass slice parsing
*)
module Iter = Iter

(**
   Immutable linked lists

   Use List for functional programming, pattern matching, recursive algorithms.

   **Prefer Vector when:** You need random access or frequent appends
*)
module List = Collections.List

(**
   Structured logging

   Use Log for application logging with levels (Debug, Info, Warn, Error).

   **Examples:**
   - Application events
   - Error tracking
   - Debugging
   - Audit trails
*)
module Log = Log

(**
   Defining extensible message types

   Use Message for building type-safe actor communication protocols.
*)
module Message = Message

(**
   Network I/O

   Parent module for TCP, HTTP, TLS, URI, and network addresses.

   **Common tasks:**
   - TCP servers → TcpServer
   - TCP clients → TcpStream
   - HTTP → Http.Request, Http.Response
   - TLS/SSL → TlsStream
   - URLs → Uri
*)
module Net = Net

(**
   Optional values

   Use Option when absence is normal and not an error condition.

   **Examples:**
   - Optional configuration fields
   - HashMap lookups (key might not exist)
   - User input that can be empty
   - Function arguments that are optional

   **Prefer Result when:** Absence indicates an error you want to handle
*)
module Option = Option

(**
   Value ordering

   Use Order when APIs need Less/Equal/Greater style comparison outcomes.
*)
module Order = Order

(**
   Type-safe file system Paths

   Use Path for type-safe, UTF-8 validated, cross-platform path operations.

   **Always use Path instead of strings for filesystem paths!**

   **See also:** {!Fs} for filesystem operations
*)
module Path = Path

(**
   Actor identifiers

   Use Pid for actor/process identification and messaging.
*)
module Pid = Pid

(**
   Actor lifecycle and monitoring

   Use Actor for spawning, linking, monitoring, and managing actors running on
   the std runtime.
*)
module Actor = Actor

(**
   Actor/process compatibility surface and operating system process id access.

   Use Actor for new actor runtime code. Process currently stays as a
   compatibility surface and also exposes the current operating system process
   id through `Process.id ()`.
*)
module Process = Process

(**
   Physical equality and pointer operations

   Rarely needed in normal application code.
*)
module Ptr = Ptr

(**
   Random value generation

   Use Random for simple pseudo-random values like `int`, `bool`, and `char`.
   Initialize the default generator with `Random.init ?seed ()` when you need
   deterministic runs.
*)
module Random = Random

(**
   Representing open, closed, and unbounded intervals

   Use Range for interval membership checks, interval intersection, and
   carrying ordering semantics alongside interval endpoints.
*)
module Range = Range

(**
   Unique, opaque, type-witnessing references

   Use Ref for ensuring type safety across module boundaries.
*)
module Ref = Ref

(**
   Regex syntax trees and compiled regular expressions

   Use Regex for pure pattern construction and matching through Kernel.Regex.
*)
module Regex = Regex

(**
   Explicit error handling

   Use Result for operations that can fail in expected, recoverable ways.

   **Examples:**
   - File I/O (file might not exist)
   - Parsing (input might be invalid)
   - Network operations (connection might fail)
   - Validation (data might not meet requirements)

   **Always use Result instead of exceptions for expected errors!**

   **See also:** {!Option} for missing values without error context
*)
module Result = Result

(**
   Accessing the actor runtime surface that backs `std`

   Use Runtime when you need direct scheduler, mailbox, timer, or low-level
   actor runtime primitives that sit below the higher-level `Std.Process`,
   `Std.Global`, and `Std.Timer` helpers.
*)
module Runtime = Runtime

(**
   UTF-8 string processing

   Use String for text manipulation with proper UTF-8 iteration support.

   **See also:** {!Unicode} for advanced text processing
*)
module String = String

(**
   Building fault-tolerant process trees

   Use Supervisor for applications that must automatically recover from crashes.

   **Strategies:**
   - OneForOne → restart only failed child
   - OneForAll → restart all children when one fails
   - RestForOne → restart failed child and those started after
   - SimpleOneForOne → dynamic children with same spec

   **Use Dynamic when:** Managing thousands of dynamic children

   **Examples:**
   - Long-running services
   - Connection pools
   - Worker pools
   - Per-user sessions
*)
module Supervisor = Supervisor

(**
   Synchronization primitives

   Use Sync for mutable cells, mutexes, condition variables.

   **Most common:** Cell for mutable values

   **See also:** {!Agent} for concurrent state with actor patterns
*)
module Sync = Sync

(**
   System information queries

   Use System for OS detection, resource limits, system paths.
*)
module System = System

(**
   Asynchronous task execution

   Use Task for fire-and-forget or awaitable async operations.

   **Examples:**
   - Background jobs
   - Parallel computations
   - Async I/O operations

   **See also:** {!WorkerPool} for distributing work across workers
*)
module Task = Task

(**
   Instrumentation and metrics

   Use Telemetry for adding observability to your application.

   **Examples:**
   - Request timing
   - Counter metrics
   - Custom events
   - Performance monitoring
*)
module Telemetry = Telemetry

(**
   Writing unit tests

   Use Test for building test suites with assertions and reporters.

   **Reporters:** TAP, JUnit XML, JSON, Pretty print
*)
module Test = Test

(**
   Concurrency-capacity queries

   Use Thread for runtime thread-budget hints like `available_parallelism`.
*)
module Thread = Thread

(**
   Time measurement

   Parent module for Duration, Instant, and SystemTime.

   **Use Duration for:** Time spans, timeouts, delays
   **Use Instant for:** Elapsed time, benchmarking
   **Use SystemTime for:** Wall-clock timestamps
*)
module Time = Time

(**
   Timers and timed events
*)
module Timer = Timer

(** Type-level programming utilities *)
module Type = Type

(**
   Unicode text processing
*)
module Unicode = Unicode

(** Universally Unique Identifiers *)
module UUID = Uuid

(** Semantic Versions parsing and operations *)
module Version = Version

(**
   Parallel work distribution

   Use WorkerPool for CPU-bound parallel processing or batch jobs.

   **Examples:**
   - Image processing pipeline
   - Data transformation
   - Batch analytics
   - Parallel map operations
*)
module WorkerPool = Worker_pool

(** Re-exported from Global *)
include module type of Global

(**
   Multi-application systems with dependencies

   Use Application for composing multiple OTP-style applications with
   automatic dependency resolution and ordered startup/shutdown.

   **Examples:**
   - Web app with database app dependency
   - Microservices with shared infrastructure
   - Plugin systems
*)
module Application = Application

(**
   Start the runtime with applications.

   Applications are started in dependency order.
   Uses `Runtime.run` under the hood.

   **Example:**
   ```ocaml
   let () = Std.start ~apps:[database_app; web_app]
   ```
*)
val start: apps:Application.t list -> unit

(**
   Panic with a message - raises an uncatchable exception.

   Unrecoverable errors, invariant violations.
   **Don't use for:** Expected errors → use Result instead
*)
val panic: string -> 'a

(**
   Create a mutable cell with the given value.

   **Example:** `let counter = cell 0 in Cell.update counter (fun n -> n + 1)`
*)
val cell: 'a -> 'a Sync.Cell.t

(** Print to stdout with immediate flush (no newline) *)
val print: string -> unit

(** Print to stdout with newline and immediate flush *)
val println: string -> unit

(** Print to stderr with immediate flush (no newline) *)
val eprint: string -> unit

(** Print to stderr with newline and immediate flush *)
val eprintln: string -> unit

(**
   Mark code as TODO with a message - panics when called.

   **Use for:** Placeholder implementations during development
*)
val todo: string -> 'a

(** Mark code as unimplemented - panics when called *)
val unimplemented: unit -> 'a

(** Vector type alias - dynamically-sized array *)
type 'a vec = 'a Collections.Vector.t
(** Queue type alias - FIFO queue *)
type 'a queue = 'a Collections.Queue.t
(** Set type alias - hash-based set *)
type 'a set = 'a Collections.HashSet.t
(** Map type alias - hash-based map *)
type ('k, 'v) map = ('k, 'v) Collections.HashMap.t

(**
   Create a vector from a list.

   **Example:** `let v = vec [1; 2; 3] in Vector.push v 4`
*)
val vec: 'a list -> 'a vec

(**
   Create a queue from a list.

   **Example:** `let q = queue [1; 2; 3] in Queue.dequeue q`
*)
val queue: 'a list -> 'a queue

(**
   Create a set from a list.

   **Example:** `let s = set [1; 2; 3; 2; 1] in HashSet.len s (* 3 *)`
*)
val set: 'a list -> 'a set

(**
   Create a map from a list of key-value pairs.

   **Example:** `let m = map [("a", 1); ("b", 2)] in HashMap.get m "a"`
*)
val map: ('k * 'v) list -> ('k, 'v) map

(** Raised when a receive operation times out *)
exception Receive_timeout

(** Raised when a syscall operation times out *)
exception Syscall_timeout

(** Message selector result type *)
type 'msg selection = 'msg Runtime.selection =
  | Select of 'msg
  | Skip
(** Message selector type *)
type 'msg selector = 'msg Runtime.selector

(** Get the PID of the currently running actor *)
val self: unit -> Pid.t

(** Spawn a new actor *)
val spawn: (unit -> (unit, Actor.exit_reason) Kernel.result) -> Pid.t

(** Spawn a new actor linked to the current actor *)
val spawn_link: (unit -> (unit, Actor.exit_reason) Kernel.result) -> Pid.t

(** Send a message to an actor *)
val send: Pid.t -> Message.t -> unit

(** Receive a message using a selector *)
val receive: selector:'value selector -> ?timeout:Time.Duration.t -> unit -> 'value

(** Receive any message *)
val receive_any: ?timeout:Time.Duration.t -> unit -> Message.t

(** Sleeps the current actor for at least the specified duration *)
val sleep: Time.Duration.t -> unit

(** Yield control to the scheduler *)
val yield: unit -> unit

(** Shutdown the runtime with the given exit status *)
val shutdown: status:int -> unit
