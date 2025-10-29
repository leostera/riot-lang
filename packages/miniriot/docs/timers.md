# Timer Wheel Implementation for Miniriot

This document describes the hierarchical timing wheel implementation added to miniriot.

## Architecture Overview

### Components

1. **Kernel.Time.monotonic_time_nanos** - Native C bindings for monotonic clock
2. **Timer_id** - Opaque int64-based timer identifiers with atomic generation
3. **Config** - Runtime configuration including timer resolution
4. **Timer** - Timer data structures and operations
5. **Timer_wheel** - Hierarchical 4-level timing wheel
6. **Scheduler integration** - Timer processing in the main loop
7. **Effects** - Timeout support for Receive and Syscall effects

### Hierarchical Wheel Structure

```
Level 0: 256 slots at base resolution (e.g., 1ms each = 256ms total)
Level 1: 64 slots at 256x resolution (e.g., 256ms each = ~16s total)
Level 2: 64 slots at 16Kx resolution (e.g., 16s each = ~17min total)
Level 3: 64 slots at 1Mx resolution (e.g., 17min each = ~19hr total)
```

## Timer Resolution

Configurable via `Config.timer_resolution`:
- `Second` - 1 second per tick
- `Millisecond` - 1 millisecond per tick (default)
- `Microsecond` - 1 microsecond per tick
- `Nanosecond` - 1 nanosecond per tick

## API Usage

### Basic Timers

```ocaml
(* Send a message after a delay *)
let timer_id = Miniriot.Timer.send_after 
  target_pid 
  MyMessage 
  ~after:1.5  (* seconds *)
in

(* Send a message repeatedly *)
let timer_id = Miniriot.Timer.send_interval
  target_pid
  Tick
  ~interval:0.1  (* seconds *)
in

(* Cancel a timer *)
Miniriot.Timer.cancel timer_id
```

### Receive with Timeout

```ocaml
try
  let msg = Miniriot.receive 
    ~selector:(function 
      | Response x -> `select x
      | _ -> `skip)
    ~timeout:5.0  (* timeout after 5 seconds *)
    ()
  in
  handle_response msg
with
| Miniriot.Receive_timeout ->
    handle_timeout ()
```

### Syscall with Timeout

```ocaml
try
  Miniriot.syscall
    ~timeout:10.0
    ~name:"read"
    ~interest:Kernel.Async.Interest.readable
    ~source
    (fun () -> perform_io ())
with
| Miniriot.Syscall_timeout ->
    handle_io_timeout ()
```

### Sleep Implementation (in Std)

```ocaml
(* Simple sleep using receive timeout *)
let sleep duration =
  try
    let _ = Miniriot.receive 
      ~selector:(fun _ -> `skip)  (* Skip all messages *)
      ~timeout:duration
      ()
    in
    ()
  with
  | Miniriot.Receive_timeout -> ()  (* Expected *)
```

## Multiple Timers Per Process

A process can have unlimited timers:

```ocaml
let worker () =
  let my_pid = Miniriot.self () in
  
  (* Set up 100 interval timers *)
  let timer_ids = List.init 100 (fun i ->
    Miniriot.Timer.send_interval 
      my_pid 
      (Tick i)
      ~interval:(0.1 +. (float i *. 0.01))
  ) in
  
  (* Process messages as they arrive *)
  let rec loop () =
    match Miniriot.receive_any () with
    | Tick i -> 
        Printf.printf "Tick %d\n%!" i;
        loop ()
    | Exit -> Ok ()
    | _ -> loop ()
  in
  loop ()
```

## Performance Characteristics

- **Insert timer**: O(1) amortized
- **Cancel timer**: O(1) hash table lookup
- **Tick with no expirations**: O(1) per level
- **Tick with N expirations**: O(N)
- **Cascade operation**: O(M) where M = timers in slot
- **Memory**: ~32KB for 4-level wheel structure + timer objects

## Implementation Details

### Timer Expiration Flow

1. `Timer_wheel.tick()` is called with current monotonic time
2. Calculate number of ticks since last update
3. For each tick:
   - Advance level 0 slot
   - Process timers in current slot
   - Check if timers should fire (compare expires_at with now)
   - Collect expired timers
   - Cascade from higher levels if level wraps around
4. Return list of expired timers
5. Scheduler processes expired timers:
   - `Wake_process` → add process to run queue
   - `Send_message` → send message to target PID
   - For intervals, reschedule with same duration

### Timeout Handling

**Receive timeout:**
1. Process calls `receive ~timeout:5.0 ()`
2. If mailbox empty, create timer with `Wake_process` action
3. Store timer ID in process state
4. Suspend process
5. When timer expires or message arrives, wake process
6. Check if timeout occurred, raise `Receive_timeout` if yes

**Syscall timeout:**
1. Process calls `syscall ~timeout:10.0 ...`
2. Register I/O interest
3. Create timer with `Wake_process` action
4. Store timer ID in process state
5. Suspend process
6. When timer expires or I/O ready, wake process
7. Check which woke us, raise `Syscall_timeout` if timer

### Cascading

When level 0 wraps around (slot returns to 0):
1. Advance level 1 slot
2. Take all timers from level 1 current slot
3. Re-insert each timer into level 0 (or higher if needed)
4. Repeat for level 2 and 3 if they wrap

This ensures timers eventually reach level 0 where they can fire.

## Multicore Considerations

Current design is single-core but multicore-ready:

1. **Per-scheduler wheels** - Each scheduler has its own timer wheel (no contention)
2. **Atomic timer IDs** - int64 IDs use compare-and-swap for thread-safe generation
3. **No shared state** - Timers stored in scheduler-local structures
4. **Migration** - For work-stealing, can move timer references between schedulers

## Configuration Example

```ocaml
let () =
  let config = Miniriot.Config.make 
    ~timer_resolution:Miniriot.Config.Millisecond
    ()
  in
  Miniriot.run ~config ~main ~args
```

## Testing

Tests should cover:
- [x] Basic timer creation and expiration
- [ ] Timer cancellation
- [ ] Multiple simultaneous timers
- [ ] Interval timers
- [ ] Receive timeout
- [ ] Syscall timeout
- [ ] Timer accuracy
- [ ] Long timeouts (multiple wheel levels)
- [ ] Edge cases (zero duration, past expiration, etc.)

## Future Enhancements

1. **Timer coalescing** - Group nearby timers for better cache locality
2. **High-resolution mode** - Sub-millisecond timing for real-time applications
3. **Timer statistics** - Track timer creation, expiration, cancellation rates
4. **Adaptive resolution** - Adjust tick size based on actual timer distribution
5. **Timer priorities** - Allow critical timers to fire before others
