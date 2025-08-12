# Gluon Architecture

Gluon is a high-performance I/O event notification library for macOS that provides a clean, type-safe OCaml interface over the kqueue system call. This document explains the internal architecture and design decisions.

## Overview

```
┌─────────────────────────────────────────────────────────────┐
│                    Application Layer                        │
│  ┌─────────────┬────────────────┬─────────────────────────┐  │
│  │    Poll     │   File/Net     │      Event Loops        │  │
│  │  Interface  │   Operations   │                         │  │
│  └─────────────┴────────────────┴─────────────────────────┘  │
├─────────────────────────────────────────────────────────────┤
│                     Gluon OCaml Layer                       │
│  ┌─────────┬─────────────┬─────────┬─────────┬────────────┐  │
│  │  Event  │   Token     │ Interest│ Source  │   Adapter  │  │
│  │ System  │   System    │ Manager │ Abstraction│ Layer  │  │
│  └─────────┴─────────────┴─────────┴─────────┴────────────┘  │
├─────────────────────────────────────────────────────────────┤
│                       C FFI Layer                           │
│  ┌─────────────────┬───────────────────────────────────────┐  │
│  │ kqueue bindings │  I/O operations (readv/writev/etc)    │  │
│  └─────────────────┴───────────────────────────────────────┘  │
├─────────────────────────────────────────────────────────────┤
│                   macOS Kernel (kqueue)                     │
└─────────────────────────────────────────────────────────────┘
```

## Core Design Principles

### 1. Zero-Copy Architecture
- Direct kqueue integration without intermediate buffering
- Vectored I/O (`readv`/`writev`) for scatter-gather operations
- Sendfile support for zero-copy file transmission
- Minimal allocations in the hot path

### 2. Type Safety
- OCaml's type system prevents common async I/O errors
- Phantom types distinguish socket kinds (`listen` vs `stream`)
- Result types make error handling explicit
- Token system provides type-safe event correlation

### 3. Platform Abstraction
- Modular adapter system supports different backends
- Event abstraction layer hides platform differences
- Clean separation between platform-specific and generic code

## Key Components

### Token System

The token system is the heart of Gluon's event correlation mechanism:

```ocaml
module Token : sig
  type t
  val make : 'value -> t
  val unsafe_to_value : t -> 'value
end
```

**Implementation:**
- Tokens use OCaml's `Obj.magic` to store arbitrary values type-unsafely
- Values are passed directly through kqueue's `udata` field
- No hash table lookups required in the event delivery path
- GC-safe: OCaml values are properly registered as GC roots in C code

**C Integration:**
```c
value *stored_value = (value *)(intptr_t)kevent->udata;
Store_field(event, 3, *stored_value);
```

### Interest System

Interests use bitmask representation for efficiency:

```ocaml
module Interest : sig
  type t = Non_zero_int.t
  val readable : t  (* 0b0001 *)
  val writable : t  (* 0b0010 *)
  val add : t -> t -> t       (* bitwise OR *)
  val remove : t -> t -> t option  (* bitwise AND NOT *)
end
```

This allows O(1) interest manipulation and direct mapping to kqueue filter types.

### Event Delivery

Events use an existential type pattern for platform abstraction:

```ocaml
module Event : sig
  type t = E : (module Intf with type t = 'state) * 'state -> t
end
```

This allows the same event interface across different platforms while maintaining zero-cost abstractions.

## C FFI Layer

### kqueue Integration

The kqueue integration is implemented in `gluon_unix_kqueue.c`:

```c
CAMLprim value gluon_unix_kevent(value max_events_val, value timeout_val, value fd_val)
```

**Key features:**
- Direct kevent system call without intermediate wrappers
- Proper OCaml runtime integration with `caml_enter_blocking_section`
- GC-aware token storage using OCaml's generational GC roots
- Platform-specific compilation with `#ifdef` guards

**Memory Management:**
```c
// Register OCaml value as GC root when storing in kqueue
value *stored_token = malloc(sizeof(value));
*stored_token = token;
caml_register_generational_global_root(stored_token);

// Remove GC root when event is delivered
caml_remove_generational_global_root(stored_value);
free(stored_value);
```

### Vectored I/O

High-performance scatter-gather I/O is implemented using system calls:

```c
CAMLprim value gluon_unix_readv(value v_fd, value v_iovecs)
CAMLprim value gluon_unix_writev(value v_fd, value v_iovecs)
```

**Implementation details:**
- Direct `readv`/`writev` system calls
- Zero-copy buffer management
- Efficient iovec array construction from OCaml Iovec.t
- Cross-platform compatibility (Linux, macOS, BSDs)

### Sendfile Integration

Zero-copy file transmission:

```c
#ifdef __APPLE__
   off_t len = Int_val(v_len);
   int ret = sendfile(fd, s, offset, &len, NULL, 0);
#else
   size_t len = Int_val(v_len);
   int ret = sendfile(fd, s, &offset, len);
#endif
```

Handles platform differences in sendfile semantics between macOS and Linux.

## Error Handling Strategy

### Systematic Error Management

```ocaml
type io_error = [
  | `Would_block    (* EAGAIN/EWOULDBLOCK - retry later *)
  | `Unix_error of Unix.error  (* System errors *)
  | `Connection_closed  (* Peer disconnection *)
  | `Eof           (* End of file/stream *)
  (* ... *)
]
```

### Non-blocking I/O Pattern

All I/O operations follow this pattern:

```ocaml
let syscall fn =
  match fn () with
  | ok -> ok
  | exception Unix.(Unix_error (EINTR, _, _)) -> syscall fn  (* Retry *)
  | exception Unix.(Unix_error ((EAGAIN | EWOULDBLOCK), _, _)) -> 
      Error `Would_block  (* Expected for async I/O *)
  | exception Unix.(Unix_error (reason, _, _)) -> 
      Error (`Unix_error reason)
```

This ensures consistent error handling across all operations.

## Performance Characteristics

### Memory Efficiency

1. **Event Array Allocation**: Events are allocated in batches to amortize allocation costs
2. **Token Storage**: Direct value storage in kqueue eliminates hash table overhead
3. **Zero-Copy Operations**: Vectored I/O and sendfile avoid unnecessary data copying

### CPU Efficiency

1. **Bitwise Interest Operations**: Interest manipulation uses fast bitwise operations
2. **Direct System Calls**: Bypasses OCaml Unix module overhead where beneficial
3. **Efficient Event Dispatch**: Existential types provide zero-cost abstractions

### Scalability

1. **O(1) Event Registration**: kqueue provides O(1) registration/deregistration
2. **Configurable Batch Sizes**: Applications can tune event batch sizes for their workload
3. **No Internal Locking**: Thread-safe when used with OCaml's runtime lock

## Platform Abstraction

### Adapter Pattern

The adapter pattern allows supporting multiple event systems:

```ocaml
module Adapter : sig
  module Selector : sig
    type t
    val name : string
    val make : unit -> (t, [> `Noop ]) io_result
    val select : ?timeout:int64 -> ?max_events:int -> t -> 
                (Event.t list, [> `Noop ]) io_result
  end
end
```

Currently implements kqueue, but designed to support:
- Linux epoll 
- Windows IOCP
- io_uring

### Source Abstraction

The Source module provides uniform treatment of I/O objects:

```ocaml
type t = S : ((module Intf with type t = 'state) * 'state) -> t
```

This allows files, sockets, pipes, and custom I/O objects to be used interchangeably with the polling system.

## Integration with Riot

Gluon is designed to integrate seamlessly with Riot's actor model:

### Actor-Friendly Design
- Non-blocking operations fit naturally with message-passing
- Token system can hold process IDs or mailbox references  
- Event-driven model prevents blocking the scheduler
- Multiple Gluon instances can be used across scheduler domains

### Concurrency Model
- Single-threaded within each domain
- Thread-safe across domains when using proper synchronization
- No internal shared state between Poll instances

## Future Extensibility

The architecture supports several planned enhancements:

### Multi-Platform Support
- Linux epoll backend using same interface
- Windows IOCP integration for cross-platform applications
- io_uring support for modern Linux systems

### Advanced Features
- UDP socket support
- Unix domain sockets
- File system event monitoring
- Timer integration

### OCaml 5 Integration
- Effects-based direct-style async I/O
- Integration with OCaml 5's domains for true parallelism
- Structured concurrency patterns

## Testing and Verification

### Test Coverage
- Unit tests for all major components
- Stress tests for high-load scenarios
- Cross-platform compatibility tests
- Memory leak detection

### Benchmarking
- Throughput tests against other async I/O libraries
- Latency measurements for different workload patterns
- Memory usage profiling
- CPU utilization analysis

## Dependencies

### Runtime Dependencies
- OCaml runtime (≥ 4.14)
- macOS (kqueue support)
- C standard library

### Build Dependencies  
- OCaml compiler with C FFI support
- Platform-specific system headers (`sys/event.h` for kqueue)
- Standard build tools (make, etc.)

### Optional Dependencies
- For testing: benchmarking libraries
- For examples: additional networking libraries

This architecture provides a solid foundation for high-performance async I/O while maintaining OCaml's type safety guarantees and integrating cleanly with the Riot actor system.