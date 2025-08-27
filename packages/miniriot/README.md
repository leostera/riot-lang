# Miniriot

A minimal single-core actor runtime for building build systems and lightweight concurrent applications.

## Overview

Miniriot is a stripped-down version of the full Riot actor runtime, designed specifically for single-core environments where you need lightweight processes, message passing, and cooperative concurrency without the complexity of multi-core scheduling. It's particularly well-suited for build systems, task orchestration, and scenarios where you want actor-model concurrency within a single OS thread.

## Core Philosophy & Design Decisions

### 1. **Single-Core Simplicity**
Unlike the full Riot runtime which implements work-stealing across multiple cores, miniriot uses a simple single-threaded cooperative scheduler. This eliminates:
- Complex synchronization primitives
- Lock-free data structures  
- Cross-core work stealing
- Thread safety concerns

**Trade-off**: Cannot utilize multiple CPU cores, but gains simplicity and predictability.

### 2. **Cooperative Scheduling**
Processes must explicitly yield control via `yield()` or blocking operations (`receive`). This provides:
- Deterministic execution order (useful for testing)
- No preemption overhead
- Simple debugging and reasoning about execution

**Trade-off**: A process that never yields can starve others, but this makes control flow explicit.

### 3. **Effect-based Process Management** 
Uses OCaml 5's effect handlers for process suspension/resumption:
- `Yield` effect: Suspend process and return to scheduler
- `Receive` effect: Suspend process until message arrives

This provides lightweight green threads without OS thread overhead.

### 4. **Run-Once Restriction**
The scheduler can only be started once per OS process via `run ~main`. Subsequent calls raise a clear error.

**Rationale**: Prevents accidentally nested schedulers and makes resource cleanup simpler.

## Architecture

### Core Components

```
┌─────────────────┐    ┌──────────────┐    ┌─────────────┐
│   Scheduler     │───▶│ Run Queue    │───▶│  Process 1  │
│                 │    │ (FIFO Queue) │    │             │
│ - process table │    │              │    │ - mailbox   │
│ - current proc  │    │              │    │ - save queue│
│ - run queue     │    │              │    │ - state     │
│ - status        │    │              │    └─────────────┘
└─────────────────┘    │              │           │
                       │              │           │
                       │              │    ┌─────────────┐
                       │              │───▶│  Process 2  │
                       │              │    │             │
                       │              │    │ - mailbox   │
                       │              │    │ - save queue│
                       │              │    │ - state     │
                       └──────────────┘    └─────────────┘
```

### 1. **Scheduler (`scheduler.ml`)**
The heart of the runtime, managing:
- **Process Table**: HashMap of PID → Process
- **Run Queue**: FIFO queue of processes ready to execute
- **Current Process**: Currently executing process (if any)
- **Status**: Exit code for entire program

**Key Design Decision**: Uses a simple round-robin scheduler rather than priority-based scheduling for predictability.

### 2. **Process (`process.ml`)**
Lightweight process abstraction containing:
- **PID**: Unique process identifier
- **Mailbox**: Incoming message queue (FIFO)  
- **Save Queue**: Messages skipped by selective receive
- **State**: Running/Waiting/Dead status
- **Function**: User-provided process function

**Mailbox Design**: Processes have both a main mailbox and a "save queue" for messages that don't match selective receive patterns. This implements proper Erlang-style selective receive semantics.

### 3. **Message System (`message.ml`)**
Uses OCaml's extensible variants for type-safe message passing:
```ocaml
type Message.t = ..  (* Extensible variant *)
type Message.t += 
  | MyMessage of string
  | AnotherMessage of int
```

**Design Benefits**:
- Type safety: Messages are statically typed
- Extensibility: Each module can add its own message types
- Pattern matching: Natural OCaml pattern matching on messages

### 4. **Process State Management (`proc_state.ml`)**
Uses OCaml 5 effect handlers to manage process continuations:
- **Continue**: Process has more work to do
- **Suspend**: Process is waiting for messages (empty mailbox)
- **Delay**: Process has messages but needs to yield (fairness)

**Critical Design Decision**: Copied directly from full Riot runtime because effect-based cooperative multitasking is complex and well-tested.

### 5. **Effects (`proc_effect.ml`)**
Defines the core effects that processes can perform:
```ocaml
type _ Effect.t +=
  | Yield : unit Effect.t
  | Receive : { selector : Message.t -> ('msg, unit) Proc_state.selector_result } -> 'msg Effect.t
```

**Design Philosophy**: Effects make concurrency explicit - you can see exactly where a process might suspend by looking for effect handlers.

## API Design

### Core Functions

#### `run : main:(unit -> Process.exit_reason) -> int`
Starts the scheduler with an initial process. Returns Unix exit code.
- **Single-use**: Can only be called once per OS process
- **Blocking**: Runs until all processes complete or main process exits
- **Exit Handling**: Converts process exit reasons to Unix exit codes

#### `spawn : (unit -> Process.exit_reason) -> Pid.t`
Creates a new lightweight process.
- **Non-blocking**: Process is added to run queue but doesn't execute immediately
- **Returns PID**: Unique identifier for sending messages
- **Isolation**: Each process has its own call stack and local variables

#### `send : Pid.t -> Message.t -> unit`
Sends a message to a process.
- **Asynchronous**: Never blocks sender
- **Reliable**: Messages always arrive (no network failures in single-core)
- **Ordered**: Messages from sender A to receiver B arrive in order
- **Wake-up**: Automatically wakes waiting processes

#### `receive : unit -> Message.t`
Receives any message from the process mailbox.
- **Blocking**: Suspends process if no messages available  
- **FIFO**: Returns oldest message first
- **Type-safe**: Returns `Message.t` but you pattern match to specific types

#### `selective_receive : (Message.t -> [`select of 'msg | `skip]) -> 'msg`
Receives only messages matching a pattern.
- **Filtering**: Only accepts messages where selector returns `select`
- **Queueing**: Skipped messages are saved for later
- **Type-safe**: Return type is determined by selector function

**Design Rationale**: This implements proper Erlang-style selective receive, which is crucial for many concurrent patterns like request-response protocols.

### Example Usage Patterns

#### 1. Basic Message Passing
```ocaml
type Message.t += Hello of string

let worker () =
  match receive () with
  | Hello name -> 
      Printf.printf "Hello, %s!\n" name;
      Ok ()
  | _ -> Ok ()

let main () =
  let worker_pid = spawn worker in
  send worker_pid (Hello "World");
  yield (); (* Let worker run *)
  Ok ()

let () = run ~main |> exit
```

#### 2. Request-Response Pattern  
```ocaml
type Message.t += 
  | Request of Pid.t * string  (* reply_to, data *)
  | Response of string

let server () =
  let rec loop () =
    match receive () with
    | Request (reply_to, data) ->
        let result = "Processed: " ^ data in
        send reply_to (Response result);
        loop ()
    | Exit -> Ok ()
    | _ -> loop ()
  in loop ()

let client server_pid () =
  let my_pid = self () in
  send server_pid (Request (my_pid, "hello"));
  match receive () with
  | Response result -> 
      Printf.printf "Got: %s\n" result;
      Ok ()
  | _ -> Ok ()
```

#### 3. Build System Pattern (Coordinator/Worker)
```ocaml
type Message.t += 
  | Compile of string
  | Compiled of string * bool
  | Done

let worker name coordinator_pid () =
  let rec loop () =
    match receive () with
    | Compile file ->
        (* Simulate compilation *)
        Printf.printf "[%s] Compiling %s\n" name file;
        yield (); (* Simulate work *)
        send coordinator_pid (Compiled (file, true));
        loop ()
    | Exit -> Ok ()
    | _ -> loop ()
  in loop ()

let coordinator files () =
  let workers = List.init 3 (fun i ->
    spawn (worker ("Worker" ^ string_of_int i) (self ()))) in
  
  (* Distribute work *)
  List.iteri (fun i file ->
    let worker = List.nth workers (i mod 3) in
    send worker (Compile file)) files;
    
  (* Collect results *)
  let rec collect n acc =
    if n = 0 then acc else
    match receive () with  
    | Compiled (file, success) ->
        collect (n-1) ((file, success) :: acc)
    | _ -> collect n acc
  in
  let results = collect (List.length files) [] in
  List.iter (send) workers Exit; (* Shutdown workers *)
  Ok ()
```

## Testing Architecture

### Test Organization
Tests are organized into separate executables to avoid the "run-once" restriction:

```
test/
├── spawn_single_test.ml          # Basic process spawning
├── spawn_multiple_test.ml        # Multiple process spawning  
├── spawn_self_pid_test.ml        # Self PID retrieval
├── message_basic_test.ml         # Basic message passing
├── message_multiple_test.ml      # Multiple message handling
├── message_ping_pong_test.ml     # Bidirectional communication
├── message_dead_process_test.ml  # Error handling
├── selective_receive_skip_test.ml    # Message filtering
├── selective_receive_queue_test.ml   # Queue ordering
├── receive_any_test.ml              # Non-selective receive
├── lifecycle_normal_exit_test.ml     # Normal termination
├── lifecycle_exception_exit_test.ml  # Exception handling
├── lifecycle_main_process_exit_test.ml     # Main process lifecycle
├── lifecycle_state_transitions_test.ml    # Process states
└── lifecycle_scheduler_termination_test.ml # Scheduler cleanup
```

### Test Design Principles
1. **One Runtime Per Test**: Each test file creates its own scheduler instance
2. **Deterministic**: Tests use `yield()` to control execution order  
3. **Self-Contained**: No shared state between tests
4. **Clear Assertions**: Tests fail with descriptive error messages

## Debugging & Observability

### Trace System
Enable detailed execution tracing:
```ocaml
let () =
  enable_trace ();  (* Turn on tracing *)
  run ~main;        (* Run with tracing *)
  disable_trace ()  (* Turn off tracing *)
```

**Trace Output Example**:
```
[TRACE] Spawning process <0.2.0>
[TRACE] Adding process <0.2.0> to run queue  
[TRACE] Process <0.1.0> yielding
[TRACE] Sending message to <0.2.0>
[TRACE] Process <0.2.0> was waiting, now runnable
[TRACE] Process <0.2.0> receiving (mailbox empty? false)
[TRACE] Process <0.2.0> has 1 messages
[TRACE] Process <0.2.0> got message
[TRACE] Process <0.2.0> selected message
```

**Design**: Tracing is optional and can be enabled/disabled at runtime without recompilation.

## Limitations & Trade-offs

### What's Missing (Compared to Full Riot)
- **Multi-core scheduling**: Single-threaded only
- **I/O integration**: No async file/network operations  
- **Timers**: Sleep is just yield, no real time-based scheduling
- **Process linking**: No automatic failure propagation
- **Process monitoring**: No death notifications
- **Supervisors**: No automatic restart strategies
- **Hot code loading**: No dynamic code updates

### Performance Characteristics
- **Process creation**: Very lightweight (just memory allocation)
- **Message passing**: Single memory copy, no serialization
- **Context switching**: Effect handler overhead only
- **Memory usage**: Minimal per-process overhead
- **Throughput**: Limited by single-core execution

### When to Use Miniriot
**Good for**:
- Build systems and task orchestration
- Testing concurrent algorithms  
- Educational purposes (learning actor model)
- Prototyping larger concurrent systems
- Single-core environments (embedded, etc.)

**Not good for**:
- CPU-intensive parallel computation
- High-throughput network servers  
- Real-time systems (no preemption)
- Large-scale distributed systems

## Implementation Notes

### Key Files
- `miniriot.ml/.mli`: Public API and main entry points
- `scheduler.ml`: Core scheduling algorithm and process management  
- `process.ml`: Process data structure and mailbox management
- `proc_state.ml`: Effect-based continuation handling (copied from full Riot)
- `proc_effect.ml`: Effect type definitions
- `message.ml`: Extensible message type system
- `mailbox.ml`: FIFO message queues
- `pid.ml`: Process identifier implementation  
- `trace.ml`: Optional debug tracing system

### Dependencies
- **OCaml 5.1+**: Required for effect handlers
- **No external dependencies**: Pure OCaml implementation
- **Gluon**: Optional dependency (unused in miniriot but available)

### Build System Integration
```ocaml
# dune-project
(package
 (name miniriot)
 (synopsis "Minimal single-core actor runtime for build systems")
 (depends ocaml gluon))
```

## Future Directions

### Possible Extensions  
- **Timer support**: Real time-based scheduling
- **Process monitoring**: Death notifications and linking
- **I/O integration**: File system operations via effects
- **Backpressure**: Message queue size limits
- **Metrics**: Runtime statistics and profiling
- **Supervision**: Basic restart strategies

### Migration Path to Full Riot
Code written for miniriot should be largely compatible with full Riot:
- Same message passing semantics
- Same process model
- Same effect-based API
- Compatible PID types

The main changes needed are:
- Multi-core considerations (no shared mutable state)
- I/O operations (use Riot's async I/O instead of blocking)
- Supervision trees (replace manual process management)

## Contributing

### Code Style
- Follow existing patterns and naming conventions
- Add comprehensive tests for new features
- Update documentation for API changes
- Use meaningful trace messages for debugging

### Testing Guidelines
- Each test should be a separate executable
- Tests should be deterministic and not depend on timing
- Use descriptive failure messages
- Test both success and failure cases

### Performance Considerations
- Prefer simple algorithms over micro-optimizations
- Profile before optimizing (use tracing)
- Consider memory allocation patterns
- Balance between generality and performance

---

Miniriot provides a clean, understandable implementation of the actor model suitable for learning, prototyping, and building concurrent applications that don't require multi-core parallelism. Its simple architecture makes it easy to understand, debug, and extend while providing the core benefits of actor-based concurrency.