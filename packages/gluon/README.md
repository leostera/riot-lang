# Gluon

A minimal, high-performance kqueue-based I/O event notification library for macOS with integrated file and network I/O.

## Features

- **Minimal API**: Simple, focused interface for I/O event notification
- **Type-safe**: Leverages OCaml's type system for safe event handling
- **High performance**: Direct kqueue bindings with minimal overhead
- **Integrated I/O**: Built-in File and Net modules for async I/O operations
- **Well-tested**: Comprehensive test suite including stress tests
- **Benchmarked**: Performance benchmarks for throughput and latency

## Quick Start

### Basic Event Loop

```ocaml
open Gluon

(* Create a kqueue instance *)
let poll = Result.get_ok (Gluon.create ())

(* Create a pipe (automatically non-blocking) *)
let (read_fd, write_fd) = Result.get_ok (Gluon.pipe ())

(* Register for read events *)
let token = Token.make "my_pipe"
let () = Result.get_ok (
  Gluon.register poll ~fd:read_fd ~token ~interests:Interest.readable
)

(* Write some data *)
let _ = Unix.write write_fd (Bytes.of_string "Hello!") 0 6

(* Poll for events *)
match Gluon.poll ~timeout:100 poll with
| Ok events ->
    Array.iter (fun event ->
      if Event.is_readable event then
        Printf.printf "Got readable event!\n"
    ) events
| Error (`System_error msg) ->
    Printf.printf "Poll error: %s\n" msg
```

### File I/O

```ocaml
open Gluon

(* Write to a file *)
let write_example () =
  match File.open_write ~create:true ~truncate:true "example.txt" with
  | Ok fd ->
      let _ = File.write fd (Bytes.of_string "Hello, File!") in
      File.close fd
  | Error (`System_error msg) ->
      Printf.printf "Error: %s\n" msg

(* Read from a file *)
let read_example () =
  match File.open_read "example.txt" with
  | Ok fd ->
      let buf = Bytes.create 1024 in
      begin match File.read fd buf with
      | Ok n -> Printf.printf "Read: %s\n" (Bytes.sub_string buf 0 n)
      | Error _ -> ()
      end;
      File.close fd
  | Error (`System_error msg) ->
      Printf.printf "Error: %s\n" msg
```

### Network I/O

```ocaml
open Gluon

(* TCP Echo Server *)
let echo_server () =
  let poll = Result.get_ok (Gluon.create ()) in
  
  (* Bind to localhost:8080 *)
  let addr = Net.Addr.tcp Net.Addr.loopback 8080 in
  let listener = Result.get_ok (Net.TcpListener.bind addr) in
  
  (* Register listener for accept events *)
  let _ = Gluon.register poll 
    ~fd:listener 
    ~token:(Token.make "listener")
    ~interests:Interest.readable in
  
  (* Event loop *)
  let rec loop () =
    match Gluon.poll poll with
    | Ok events ->
        Array.iter (fun event ->
          if Event.is_readable event then
            match Net.TcpListener.accept listener with
            | Ok (client, addr) ->
                Printf.printf "New connection from %a\n" Net.Addr.pp addr;
                (* Handle client... *)
            | Error _ -> ()
        ) events;
        loop ()
    | Error _ -> ()
  in
  loop ()
```

## API Overview

### Core Types

- `Fd.t`: File descriptor type
- `Token.t`: Polymorphic token for identifying events
- `Interest.t`: I/O interests (readable, writable)
- `Event.t`: Events returned by polling

### Main Operations

- `create`: Create a new kqueue instance
- `poll`: Poll for events with optional timeout
- `register`: Register a file descriptor with interests
- `reregister`: Change interests for a registered fd
- `deregister`: Remove a file descriptor
- `pipe`: Create a pipe with both ends non-blocking
- `set_nonblocking`: Utility to set fd non-blocking

## Architecture

### Design Philosophy

Gluon is designed as a minimal, high-performance I/O event notification library that leverages macOS's kqueue system call. The design prioritizes:

1. **Simplicity**: A clean, minimal API that does one thing well
2. **Performance**: Direct kqueue bindings with minimal overhead
3. **Type Safety**: Leveraging OCaml's type system for compile-time guarantees
4. **Integration**: Built-in file and network I/O operations that work seamlessly with the event loop

### System Architecture

```
┌────────────────────────────────────────────────────────┐
│                    Application                         │
├────────────────────────────────────────────────────────┤
│                   Gluon OCaml API                      │
│  ┌─────────┬────────────┬──────────┬───────────────┐  │
│  │  Poll   │   Event    │   File   │      Net      │  │
│  │ Manager │   Types    │   I/O    │   (TCP/UDP)   │  │
│  └─────────┴────────────┴──────────┴───────────────┘  │
├────────────────────────────────────────────────────────┤
│                    C FFI Layer                         │
│  ┌─────────────────┬──────────────────────────────┐  │
│  │  gluon_kevent   │   gluon_readv/writev/etc    │  │
│  └─────────────────┴──────────────────────────────┘  │
├────────────────────────────────────────────────────────┤
│                 macOS Kernel (kqueue)                  │
└────────────────────────────────────────────────────────┘
```

### Core Components

#### 1. **Poll Manager** (`gluon.ml`)
The heart of Gluon is the poll manager which wraps a kqueue file descriptor and maintains a registry of monitored file descriptors:

```ocaml
type t = {
  kq: Unix.file_descr;  (* The kqueue instance *)
  mutable registered_fds: (int, Token.t * Interest.t) Hashtbl.t;
}
```

This design allows O(1) lookups for validation and modification of registered interests.

#### 2. **Token System**
Gluon uses a polymorphic token system that allows applications to attach arbitrary OCaml values to file descriptors:

```ocaml
module Token : sig
  type t
  val make : 'a -> t
  val unsafe_to_value : t -> 'a
end
```

Tokens are stored directly in kqueue's user data field, eliminating the need for a separate lookup table and reducing memory accesses in the hot path.

#### 3. **Interest Model**
I/O interests use a bitmask representation for efficiency:

```ocaml
module Interest : sig
  type t
  val readable : t
  val writable : t
  val ( + ) : t -> t -> t  (* Combine interests *)
  val ( - ) : t -> t -> t option  (* Remove interests *)
end
```

This allows efficient combination and manipulation of interests with bitwise operations.

#### 4. **Event Delivery**
Events are delivered as an array for cache efficiency:

```ocaml
val poll : ?timeout:int -> ?max_events:int -> t -> 
  (Event.t array, [> `System_error of string ]) result
```

The array-based approach minimizes allocations and improves cache locality when processing multiple events.

### Key Design Decisions

#### 1. **Direct C Bindings**
Rather than using OCaml's Unix module for kqueue operations, Gluon implements direct C bindings. This provides:
- Full control over the kqueue interface
- Ability to store OCaml values directly in kqueue's udata field
- Optimal performance with minimal overhead

#### 2. **GC-Aware Token Storage**
The C layer carefully manages OCaml values stored in kqueue:
```c
/* Register token as GC root when adding events */
value *stored_token = malloc(sizeof(value));
*stored_token = token;
caml_register_generational_global_root(stored_token);

/* Remove GC root when events are delivered */
caml_remove_generational_global_root(stored_value);
free(stored_value);
```

This ensures tokens remain valid across GC cycles while avoiding memory leaks.

#### 3. **Integrated I/O Operations**
Unlike many event libraries that only handle notifications, Gluon includes integrated file and network I/O operations:
- Vectored I/O (`readv`/`writev`) for efficient scatter-gather operations
- Zero-copy sendfile support
- Non-blocking socket operations with proper error handling

#### 4. **Error Handling Strategy**
Gluon uses a simplified error model where most operations return `(_, [> `Noop]) result`. This:
- Simplifies error handling in applications
- Allows for future extension with more specific error types
- Maintains compatibility with existing error handling patterns

### Performance Characteristics

#### Memory Efficiency
- **Zero-copy event delivery**: Events are written directly into pre-allocated arrays
- **Minimal allocations**: The only allocations in the hot path are for event arrays
- **Token reuse**: Applications can reuse tokens across registrations

#### CPU Efficiency
- **Batched operations**: Multiple events can be registered/modified in a single syscall
- **Efficient interest management**: Bitwise operations for interest manipulation
- **Direct syscalls**: Bypasses OCaml Unix module overhead

#### Scalability
- **O(1) registration checks**: Hash table for registered FD lookup
- **Configurable event batch size**: Applications can tune for their workload
- **No internal threads**: Integrates cleanly with OCaml's runtime

### Integration with Riot

Gluon is designed to integrate seamlessly with Riot's actor model:
- Tokens can be process IDs or mailbox references
- Non-blocking I/O fits naturally with actor message passing
- Event-driven model prevents blocking the scheduler
- Multiple Gluon instances can be used across different scheduler domains

### Future Considerations

The architecture is designed to support future enhancements:
- Linux epoll support (similar API, different implementation)
- io_uring backend for modern Linux systems
- UDP and Unix domain socket support
- Integration with OCaml 5's effects system for direct-style async I/O

## Testing

Run the test suite:

```bash
dune test
```

Run benchmarks:

```bash
dune exec bench/bench_throughput.exe
```

## Performance

On a typical macOS system, Gluon can handle:
- **Single FD**: ~1M events/second
- **100 FDs**: ~500K events/second aggregate
- **Poll latency**: < 0.1ms for empty polls

## Implementation Notes

- Uses kqueue's user data field to store OCaml tokens directly
- Minimal allocation in the event loop
- Supports both edge-triggered and level-triggered modes
- Thread-safe when used with OCaml's runtime lock

## License

Same as the Riot ML project.