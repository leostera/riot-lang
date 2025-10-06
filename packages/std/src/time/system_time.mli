(** # Time.SystemTime - Wall-clock time
    
    A measurement of the system's real-world clock, suitable for timestamps
    and calendar operations. Unlike [Instant], SystemTime can go backwards
    due to clock adjustments, leap seconds, or NTP synchronization.
    
    ## Examples
    
    Recording timestamps:
    
    ```ocaml
    open Std.Time
    
    let created_at = SystemTime.now () in
    
    (* Later... *)
    let age = SystemTime.elapsed created_at in
    Log.info "Resource created %f seconds ago"
      (Duration.to_secs_float age)
    ```
    
    Comparing times:
    
    ```ocaml
    let t1 = SystemTime.now () in
    sleep 1;
    let t2 = SystemTime.now () in
    
    let diff = SystemTime.duration_since ~earlier:t1 t2 in
    Duration.to_secs diff  (* ~1 second *)
    ```
    
    ## Differences from Instant
    
    | SystemTime | Instant |
    |-----------|---------|
    | Wall-clock time | Monotonic clock |
    | Can go backwards | Never goes backwards |
    | For timestamps | For measuring durations |
    | Affected by NTP | Not affected by NTP |
    | Can convert to calendar time | Opaque, relative only |
    
    ## When to Use SystemTime
    
    - Recording creation/modification times
    - Timestamping events for logging
    - Calendar and date operations
    - Comparing with external timestamps
    
    ## When to Use Instant
    
    - Measuring elapsed time
    - Benchmarking
    - Timeouts and deadlines
    - Protecting against clock adjustments
    
    ## Platform-Specific Precision
    
    SystemTime precision depends on the OS and underlying time format:
    
    - **Linux/Unix**: Nanosecond precision via clock_gettime(CLOCK_REALTIME)
    - **macOS/Darwin**: Nanosecond precision via clock_gettime(CLOCK_REALTIME)
    - **Windows**: 100 nanosecond intervals via GetSystemTimePreciseAsFileTime
    - **WASI**: Platform-dependent via __wasi_clock_time_get
    
    ## Clock Adjustments
    
    SystemTime can jump forwards or backwards due to:
    - NTP time synchronization
    - Manual clock adjustments
    - Daylight saving time changes
    - Leap seconds
    
    For measuring durations, use [Instant] which is immune to these changes.
*)

type t
(** A point in wall-clock time from the system's real-time clock.
    Can be used for timestamps but may not be monotonic. *)

(** {1 Creation} *)

val now : unit -> t
(** Returns the current system time from the real-time clock.
    
    ## Examples
    
    ```ocaml
    let timestamp = SystemTime.now () in
    process_event ~timestamp event
    ```
    
    ## Platform Calls
    
    - Linux/Unix: clock_gettime(CLOCK_REALTIME)
    - macOS: clock_gettime(CLOCK_REALTIME)
    - Windows: GetSystemTimePreciseAsFileTime
    - WASI: __wasi_clock_time_get(REALTIME)
    
    ## Note
    
    This can go backwards if the system clock is adjusted.
*)

(** {1 Duration Operations} *)

val duration_since : earlier:t -> t -> Duration.t
(** Returns the time elapsed from [earlier] to the given time.
    
    ## Examples
    
    ```ocaml
    let start = SystemTime.now () in
    perform_work ();
    let finish = SystemTime.now () in
    let elapsed = SystemTime.duration_since ~earlier:start finish
    ```
    
    ## Warning
    
    If the system clock is adjusted backwards between measurements,
    this may panic or return an incorrect duration. For reliable
    duration measurement, use [Instant.duration_since] instead.
*)

val elapsed : t -> Duration.t
(** Returns the time elapsed since this system time.
    Equivalent to [duration_since ~earlier:self (now ())].
    
    ## Examples
    
    ```ocaml
    let created = SystemTime.now () in
    
    (* Much later... *)
    let age = SystemTime.elapsed created in
    if Duration.to_secs age > 3600 then
      Log.info "Cache entry is over 1 hour old"
    ```
    
    ## Warning
    
    Subject to clock adjustments. Use [Instant.elapsed] for
    monotonic time measurement.
*)

(** {1 Arithmetic Operations} *)

val add : t -> Duration.t -> t
(** Adds a duration to a system time, returning a future time.
    
    ## Examples
    
    ```ocaml
    let now = SystemTime.now () in
    let expiry = SystemTime.add now (Duration.from_hours 24) in
    (* expiry is 24 hours from now *)
    ```
*)

val sub : t -> Duration.t -> t
(** Subtracts a duration from a system time, returning a past time.
    
    ## Examples
    
    ```ocaml
    let now = SystemTime.now () in
    let yesterday = SystemTime.sub now (Duration.from_days 1)
    ```
*)

(** {1 Checked Operations} *)

val checked_add : t -> Duration.t -> t option
(** Adds a duration if the result can be represented, returns [None] on overflow.
    
    ## Examples
    
    ```ocaml
    match SystemTime.checked_add time Duration.max with
    | Some future -> (* OK *)
    | None -> (* Overflow - time too far in future *)
    ```
*)

val checked_sub : t -> Duration.t -> t option
(** Subtracts a duration if the result can be represented, returns [None]
    if it would underflow.
    
    ## Examples
    
    ```ocaml
    match SystemTime.checked_sub time (Duration.from_secs 1000000) with
    | Some past -> (* OK *)
    | None -> (* Would be before epoch *)
    ```
*)

(** {1 Comparison} *)

val compare : t -> t -> int
(** Compares two system times. Returns negative if first < second,
    0 if equal, positive if first > second.
    
    ## Examples
    
    ```ocaml
    let t1 = SystemTime.now () in
    sleep 0.1;
    let t2 = SystemTime.now () in
    SystemTime.compare t1 t2  (* < 0, t1 is earlier *)
    ```
*)

val equal : t -> t -> bool
(** Tests equality of two system times.
    
    ## Examples
    
    ```ocaml
    let t1 = SystemTime.now () in
    SystemTime.equal t1 t1  (* true *)
    ```
*)

val min : t -> t -> t
(** Returns the earlier of two system times.
    
    ## Examples
    
    ```ocaml
    let earliest = SystemTime.min time1 time2
    ```
*)

val max : t -> t -> t
(** Returns the later of two system times.
    
    ## Examples
    
    ```ocaml
    let latest = SystemTime.max time1 time2
    ```
*)
