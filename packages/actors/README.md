# Actors

Multicore actors for Riot.

`actors` is Riot's low-level actor runtime package. It gives you lightweight
processes, typed message passing, links and monitors, timers, async syscalls,
and a runtime that can spread runnable actors across multiple scheduler workers
with work stealing.

If `std` is the application-facing stack, `actors` is the smaller surface you
reach for when you want the runtime itself: process orchestration, internal
concurrency, or infrastructure that should sit closer to Riot's scheduling and
mailbox model.

## Install

```sh
riot add actors
```

## What value it gives you

Use `actors` when you want:

- Erlang-style processes and mailboxes without pulling in the whole `std`
  surface;
- parallel execution across multiple worker schedulers instead of a single-core
  event loop;
- failure-aware process relationships through links and monitors;
- timer-based wakeups and async syscall integration inside the same actor
  runtime;
- a small runtime package you can build other Riot infrastructure on top of.

In practice that makes `actors` a good fit for:

- build systems and task executors;
- schedulers, brokers, queues, and orchestration layers;
- protocol runtimes and infrastructure packages;
- systems code that wants actor semantics without the rest of the application
  stack.

If you are building a normal application, service, or CLI, start with `std`
instead. `std` already builds on the same runtime and gives you the rest of the
practical surface area around it.

## Quick start

```ocaml
open Std
open Actors

type Message.t +=
  | Ping of Pid.t
  | Pong

let worker = fun () ->
  let sender =
    receive ~selector:(function
      | Ping sender -> `select sender
      | _ -> `skip)
      ()
  in
  send sender Pong;
  Ok ()

let main = fun ~args:_ ->
  let pid = spawn worker in
  send pid (Ping (self ()));
  receive
    ~selector:(function
      | Pong -> `select ()
      | _ -> `skip)
    ();
  Ok ()

let () = run ~main ~args:Env.args ()
```

The package also ships a runnable example:

```sh
riot run -p actors ping_pong
```

## The APIs you will actually use

The most important entry points are:

- `run` to start the runtime;
- `spawn` and `spawn_link` to create processes;
- `send`, `receive`, and `receive_any` for mailbox-driven workflows;
- `self` to get the current PID;
- `yield` to spend cooperative reductions explicitly when long-running work
  should give other actors a chance to run;
- `Process.monitor`, `Process.link`, `Process.unlink`, and `Process.demonitor`
  for process lifecycle coordination;
- `Timer.send_after` and `Timer.send_interval` for delayed or repeating
  messages;
- `syscall` for async source integration through the runtime's reactor.

Most day-to-day actor code stays very small:

1. define message constructors on `Message.t`
2. `spawn` a process loop
3. `send` messages to it
4. `receive` the messages you care about

## Runtime model

You do not need to manually assign actors to cores.

The runtime starts one normal worker scheduler per configured slot plus a
dedicated reactor domain for timers and async I/O. Runnable actors are placed
onto worker-local queues, and idle workers can steal runnable actors from busy
workers. That means a burst of independent actors can spread across multiple
cores without you having to shard work yourself.

The important semantic guarantees remain actor-centric:

- PIDs are runtime-wide.
- `send` targets a PID, not a scheduler.
- messages sent from one sender to one recipient are observed in send order.
- blocked actors resume when their mailbox, timer, or async source makes them
  runnable again.

The runtime is still cooperative at the process level. A process doing a large
amount of CPU work should either block in meaningful runtime operations or call
`yield` periodically so it shares execution fairly with other runnable actors.

## Configuration and observability

The runtime defaults to Riot's normal scheduler sizing, but you can pass an
explicit `Actors.Config.t` to `run` when you want to size the worker pool or
change timer resolution.

For debugging and tuning:

- `enable_trace` / `disable_trace` turn tracing on and off;
- `trace_counters` exposes scheduler counters such as steals, failed steals,
  remote wakeups, and duplicate enqueue races;
- `reset_trace_counters` clears those counters between runs.

This is most useful when you are working on infrastructure code and want to
understand whether the runtime is balancing work the way you expect.

## Example patterns

Patterns that fit naturally in `actors`:

- coordinator/worker execution pools;
- request/response processes;
- background resource managers;
- mailbox-driven protocol implementations;
- timer-triggered retries and timeouts;
- supervision-like lifecycle coordination built from links and monitors.

## Related packages

- `kernel` provides the lower-level async and runtime primitives that `actors`
  builds on.
- `std` is the higher-level application stack built on the same actor runtime.
- `blink` and `suri` are examples of packages that build richer abstractions on
  top of Riot's concurrency model.
