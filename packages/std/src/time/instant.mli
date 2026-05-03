(**
   Monotonic clock measurements.

   A measurement from a monotonically non-decreasing clock, useful for timing
   operations and benchmarks. Instants are opaque and can only be compared with
   other instants or used to measure elapsed time.

   ## Examples

   Measuring elapsed time:

   ```ocaml open Std.Time

   let start = Instant.now () in

   (* ... perform some work ... *) expensive_computation ();

   let elapsed = Instant.elapsed start in Log.info "Operation took %f seconds"
   (Duration.to_secs_float elapsed) ```

   Comparing instants:

   ```ocaml let t1 = Instant.now () in sleep 0.1; let t2 = Instant.now () in

   let diff = Instant.duration_since ~earlier:t1 t2 in Duration.to_millis diff
   (* ~100 ms *) ```

   Using for timeouts:

   ```ocaml let deadline = Instant.now () |> Instant.add (Duration.from_secs 5)
   in

   let rec poll () = if Instant.now () |> Instant.compare deadline >= 0 then
   Error "Timeout" else match try_read () with | Some data -> Ok data | None ->
   poll () in poll () ```

   ## Guarantees and Limitations

   ### Monotonicity

   Instants are guaranteed to never go backwards (barring platform bugs). Each
   call to [now] returns an instant >= all previous calls.

   ### Not a Wall Clock

   - Cannot convert to calendar time (use [SystemTime] for that)
   - Cannot get "seconds since epoch"
   - Only measures relative durations between instants

   ### Platform Behavior

   - Clock ticks may vary in length (not guaranteed steady)
   - May jump forward or experience time dilation
   - System suspend behavior is platform-dependent
   - Implementation varies across platforms

   ## Common Use Cases

   - Measuring performance and benchmarking
   - Timing operations and detecting timeouts
   - Rate limiting and throttling
   - Profiling code sections
*)

(**
   A point in time from a monotonic clock. Opaque - cannot be converted to
   wall-clock time, only compared with other instants.
*)
type t

(**
   Returns the current instant from the monotonic clock.

   ## Examples

   ```ocaml let start = Instant.now () in perform_work (); let finish =
   Instant.now () in let duration = Instant.duration_since ~earlier:start
   finish ```

   ## Platform Notes

   Uses the system's monotonic clock:
   - Linux: CLOCK_MONOTONIC
   - macOS: mach_absolute_time
   - Windows: QueryPerformanceCounter
*)
val now: unit -> t

(**
   Returns the time elapsed from [earlier] to the given instant.

   Panics if [earlier] is actually later than the current instant.

   ## Examples

   ```ocaml let t1 = Instant.now () in sleep 0.1; let t2 = Instant.now () in
   let elapsed = Instant.duration_since ~earlier:t1 t2 in (* elapsed ~= 100ms
   *) ```

   ## Panics

   If [earlier] is greater than [self], use [saturating_duration_since] for
   safe handling.
*)
val duration_since: earlier:t -> t -> Duration.t

(**
   Returns the time elapsed from [earlier], or zero if [earlier] is actually
   later (defensive version of [duration_since]).

   ## Examples

   ```ocaml let t1 = Instant.now () in let t2 = Instant.now () in

   (* Safe even if t2 < t1 (shouldn't happen but defensively handles it) *)
   Instant.saturating_duration_since ~earlier:t2 t1 (* >= Duration.zero *) ```

   ## Use Case

   Use when you want to avoid panics from clock anomalies or incorrect ordering
   of instants.
*)
val saturating_duration_since: earlier:t -> t -> Duration.t

(**
   Returns the time elapsed since this instant was created. Equivalent to
   [duration_since ~earlier:self (now ())].

   ## Examples

   ```ocaml let start = Instant.now () in expensive_computation (); let
   time_taken = Instant.elapsed start in

   if Duration.to_secs time_taken > 5 then Log.warn "Operation took too long:
   %fs" (Duration.to_secs_float time_taken) ```
*)
val elapsed: t -> Duration.t

(**
   Adds a duration to an instant, returning a future instant. Panics on
   overflow.

   ## Examples

   ```ocaml let now = Instant.now () in let deadline = Instant.add now
   (Duration.from_secs 30) in

   (* Check if we've passed the deadline *) if Instant.compare (Instant.now ())
   deadline >= 0 then handle_timeout () ```
*)
val add: t -> Duration.t -> t

(**
   Subtracts a duration from an instant, returning a past instant. Panics if
   result would be before the epoch.

   ## Examples

   ```ocaml let now = Instant.now () in let past = Instant.sub now
   (Duration.from_secs 10) in ```
*)
val sub: t -> Duration.t -> t

(**
   Adds a duration if the result can be represented, returns [None] on
   overflow.

   ## Examples

   ```ocaml match Instant.checked_add instant Duration.max with | Some future
   -> (* OK *) | None -> (* Overflow *) ```
*)
val checked_add: t -> Duration.t -> t option

(**
   Subtracts a duration if the result can be represented, returns [None] if it
   would underflow.

   ## Examples

   ```ocaml match Instant.checked_sub instant (Duration.from_secs 1000000) with
   | Some past -> (* OK *) | None -> (* Would be before epoch *) ```
*)
val checked_sub: t -> Duration.t -> t option

(**
   Compares two instants. Returns negative if first < second, 0 if equal,
   positive if first > second.

   ## Examples

   ```ocaml let t1 = Instant.now () in let t2 = Instant.now () in

   Instant.compare t1 t2 (* <= 0, t1 should be earlier or equal *) ```
*)
val compare: t -> t -> Order.t

(**
   Tests equality of two instants.

   ## Examples

   ```ocaml let t1 = Instant.now () in Instant.equal t1 t1 (* true *) ```
*)
val equal: t -> t -> bool

(**
   Returns the earlier of two instants.

   ## Examples

   ```ocaml let earliest = Instant.min instant1 instant2 ```
*)
val min: t -> t -> t

(**
   Returns the later of two instants.

   ## Examples

   ```ocaml let latest = Instant.max instant1 instant2 ```
*)
val max: t -> t -> t
