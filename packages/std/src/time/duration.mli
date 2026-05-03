(**
   Spans of time.

   A type representing a span of time with nanosecond precision, typically used
   for timeouts, delays, and measuring elapsed time.

   ## Examples

   Basic duration creation and conversion:

   ```ocaml open Std.Time

   (* Create durations *) let timeout = Duration.from_secs 30 in let interval =
   Duration.from_millis 100 in let delay = Duration.make ~secs:1
   ~nanos:500_000_000 in

   (* Convert to different units *) Duration.to_millis timeout (* 30000 *)
   Duration.to_secs_float delay (* 1.5 *) ```

   Arithmetic operations:

   ```ocaml let d1 = Duration.from_secs 5 in let d2 = Duration.from_secs 3 in

   let total = Duration.add d1 d2 in (* 8 seconds *) let diff = Duration.sub d1
   d2 in (* 2 seconds *) let doubled = Duration.mul d1 2 in (* 10 seconds *)
   ```

   Safe operations with overflow protection:

   ```ocaml let huge = Duration.from_secs 1_000_000_000 in

   (* Checked operations return None on overflow *) match Duration.checked_add
   huge huge with | Some d -> Printf.printf "OK: %d\n" (Duration.to_secs d) |
   None -> Printf.printf "Overflow\n"

   (* Saturating operations clamp to max/zero *) let maxed =
   Duration.saturating_add huge huge in Duration.equal maxed Duration.max (*
   true *) ```

   ## Precision

   Each duration is composed of whole seconds and a fractional part in
   nanoseconds. If the underlying system doesn't support nanosecond precision,
   APIs will typically round up.

   ## Common Use Cases

   - Setting timeouts for operations
   - Implementing retry delays with backoff
   - Measuring performance with [Instant]
   - Rate limiting and throttling
*)

(** A span of time stored as seconds + nanoseconds. Always non-negative. *)
type t

(**
   A duration of zero time.

   ## Examples

   ```ocaml Duration.is_zero Duration.zero (* true *) Duration.to_secs
   Duration.zero (* 0 *) ```
*)
val zero: t

(**
   The maximum representable duration (~584 billion years).

   ## Examples

   ```ocaml let huge = Duration.from_secs 999_999_999_999 in
   Duration.saturating_add huge huge |> Duration.equal Duration.max_duration (* true *)
   ```
*)
val max_duration: t

(**
   Creates a duration from seconds and nanoseconds.

   ## Examples

   ```ocaml (* 1.5 seconds *) Duration.make ~secs:1 ~nanos:500_000_000

   (* 0.001 seconds = 1 millisecond *) Duration.make ~secs:0 ~nanos:1_000_000
   ```

   ## Note

   Nanoseconds should be in range 0-999,999,999. Values outside this range will
   be normalized into the seconds component.
*)
val make: secs:int -> nanos:int -> t

(**
   Creates a duration from a number of days (24-hour periods).

   ## Examples

   ```ocaml let week = Duration.from_days 7 in Duration.to_secs week (* 604800
   *) ```
*)
val from_days: int -> t

(**
   Creates a duration from hours.

   ## Examples

   ```ocaml Duration.from_hours 2 |> Duration.to_secs (* 7200 *) ```
*)
val from_hours: int -> t

(**
   Creates a duration from minutes.

   ## Examples

   ```ocaml Duration.from_mins 30 |> Duration.to_secs (* 1800 *) ```
*)
val from_mins: int -> t

(**
   Creates a duration from seconds.

   ## Examples

   ```ocaml let timeout = Duration.from_secs 60 in ```
*)
val from_secs: int -> t

(**
   Creates a duration from milliseconds.

   ## Examples

   ```ocaml Duration.from_millis 1500 |> Duration.to_secs_float (* 1.5 *) ```
*)
val from_millis: int -> t

(**
   Creates a duration from microseconds.

   ## Examples

   ```ocaml Duration.from_micros 1_000_000 |> Duration.to_secs (* 1 *) ```
*)
val from_micros: int -> t

(**
   Creates a duration from nanoseconds.

   ## Examples

   ```ocaml Duration.from_nanos 1_000_000_000 |> Duration.to_secs (* 1 *) ```
*)
val from_nanos: int -> t

(**
   Creates a duration from weeks (7-day periods).

   ## Examples

   ```ocaml Duration.from_weeks 2 |> Duration.to_days (* 14 *) ```
*)
val from_weeks: int -> t

(**
   Creates a duration from floating-point seconds.

   ## Examples

   ```ocaml let d = Duration.from_secs_float 1.5 in Duration.to_millis d (*
   1500 *) ```
*)
val from_secs_float: float -> t

(**
   Extracts the whole seconds component, discarding fractional part.

   ## Examples

   ```ocaml let d = Duration.from_millis 1500 in Duration.to_secs d (* 1 -
   fractional part discarded *) ```
*)
val to_secs: t -> int

(**
   Converts to floating-point seconds with fractional precision.

   ## Examples

   ```ocaml let d = Duration.make ~secs:1 ~nanos:500_000_000 in
   Duration.to_secs_float d (* 1.5 *) ```
*)
val to_secs_float: t -> float

(**
   Converts to a string representation of seconds with specified decimal precision.

   ## Examples

   ```ocaml let d = Duration.from_secs_float 1.23456 in
   Duration.to_secs_string d (* "1.23" - default precision 2 *)
   Duration.to_secs_string ~precision:4 d (* "1.2346" *)
   Duration.to_secs_string ~precision:0 d (* "1" *) ```
*)
val to_secs_string: ?precision:int -> t -> string

(**
   Converts to total milliseconds.

   ## Examples

   ```ocaml Duration.from_secs 2 |> Duration.to_millis (* 2000 *) ```
*)
val to_millis: t -> int

(**
   Converts to total microseconds.

   ## Examples

   ```ocaml Duration.from_millis 1 |> Duration.to_micros (* 1000 *) ```
*)
val to_micros: t -> int

(**
   Converts to total nanoseconds.

   ## Examples

   ```ocaml Duration.from_secs 1 |> Duration.to_nanos (* 1_000_000_000 *) ```
*)
val to_nanos: t -> int64

(**
   Returns only the fractional milliseconds (0-999).

   ## Examples

   ```ocaml let d = Duration.make ~secs:5 ~nanos:123_456_789 in
   Duration.subsec_millis d (* 123 *) ```
*)
val subsec_millis: t -> int

(**
   Returns only the fractional microseconds (0-999,999).

   ## Examples

   ```ocaml let d = Duration.make ~secs:5 ~nanos:123_456_789 in
   Duration.subsec_micros d (* 123_456 *) ```
*)
val subsec_micros: t -> int

(**
   Returns only the fractional nanoseconds (0-999,999,999).

   ## Examples

   ```ocaml let d = Duration.make ~secs:5 ~nanos:123_456_789 in
   Duration.subsec_nanos d (* 123_456_789 *) Duration.to_secs d (* 5 *) ```
*)
val subsec_nanos: t -> int

(**
   Returns [true] if duration is zero.

   ## Examples

   ```ocaml Duration.is_zero Duration.zero (* true *) Duration.is_zero
   (Duration.from_secs 0) (* true *) Duration.is_zero (Duration.from_nanos 1)
   (* false *) ```
*)
val is_zero: t -> bool

(**
   Adds two durations. Panics on overflow.

   ## Examples

   ```ocaml let d1 = Duration.from_secs 5 in let d2 = Duration.from_millis 500
   in Duration.add d1 d2 |> Duration.to_secs_float (* 5.5 *) ```

   ## See Also

   - [checked_add] for overflow detection
   - [saturating_add] for clamping behavior
*)
val add: t -> t -> t

(**
   Subtracts durations. Panics if result would be negative.

   ## Examples

   ```ocaml let d1 = Duration.from_secs 10 in let d2 = Duration.from_secs 3 in
   Duration.sub d1 d2 |> Duration.to_secs (* 7 *) ```
*)
val sub: t -> t -> t

(**
   Multiplies duration by an integer. Panics on overflow.

   ## Examples

   ```ocaml Duration.from_secs 5 |> Duration.mul 3 |> Duration.to_secs (* 15 *)
   ```
*)
val mul: t -> int -> t

(**
   Divides duration by an integer. Panics if divisor is zero.

   ## Examples

   ```ocaml Duration.from_secs 10 |> Duration.div 2 |> Duration.to_secs (* 5 *)
   ```
*)
val div: t -> int -> t

(**
   Returns [Some result] if addition doesn't overflow, [None] otherwise.

   ## Examples

   ```ocaml Duration.checked_add Duration.max_duration (Duration.from_secs 1) (* None *)
   Duration.checked_add (Duration.from_secs 5) (Duration.from_secs 3) (* Some
   8s *) ```
*)
val checked_add: t -> t -> t option

(**
   Returns [Some result] if subtraction is positive, [None] if negative.

   ## Examples

   ```ocaml let d1 = Duration.from_secs 5 in let d2 = Duration.from_secs 10 in
   Duration.checked_sub d1 d2 (* None - would be negative *)
   Duration.checked_sub d2 d1 (* Some 5s *) ```
*)
val checked_sub: t -> t -> t option

(**
   Returns [Some result] if multiplication doesn't overflow, [None] otherwise.

   ## Examples

   ```ocaml Duration.checked_mul Duration.max 2 (* None - overflow *) ```
*)
val checked_mul: t -> int -> t option

(**
   Returns [Some result] if divisor is non-zero, [None] for division by zero.

   ## Examples

   ```ocaml Duration.checked_div (Duration.from_secs 10) 0 (* None *)
   Duration.checked_div (Duration.from_secs 10) 2 (* Some 5s *) ```
*)
val checked_div: t -> int -> t option

(**
   Adds durations, clamping to [max_duration] on overflow.

   ## Examples

   ```ocaml let result = Duration.saturating_add Duration.max_duration
   (Duration.from_secs 1) in Duration.equal result Duration.max_duration (* true *) ```
*)
val saturating_add: t -> t -> t

(**
   Subtracts durations, clamping to [zero] if result would be negative.

   ## Examples

   ```ocaml let d1 = Duration.from_secs 5 in let d2 = Duration.from_secs 10 in
   let result = Duration.saturating_sub d1 d2 in Duration.is_zero result (*
   true *) ```
*)
val saturating_sub: t -> t -> t

(**
   Multiplies duration, clamping to [max_duration] on overflow.

   ## Examples

   ```ocaml Duration.saturating_mul Duration.max_duration 1000 |> Duration.equal
   Duration.max_duration (* true *) ```
*)
val saturating_mul: t -> int -> t

(**
   Multiplies duration by a floating-point factor.

   ## Examples

   ```ocaml Duration.from_secs 10 |> Duration.mul_f64 1.5 |> Duration.to_secs
   (* 15 *) ```
*)
val mul_f64: t -> float -> t

(**
   Divides duration by a floating-point divisor.

   ## Examples

   ```ocaml Duration.from_secs 10 |> Duration.div_f64 2.5 |> Duration.to_secs
   (* 4 *) ```
*)
val div_f64: t -> float -> t

(**
   Returns the absolute difference between two durations.

   ## Examples

   ```ocaml let d1 = Duration.from_secs 5 in let d2 = Duration.from_secs 10 in
   Duration.abs_diff d1 d2 |> Duration.to_secs (* 5 *) Duration.abs_diff d2 d1
   |> Duration.to_secs (* 5 - same *) ```
*)
val abs_diff: t -> t -> t

(**
   Returns the smaller of two durations.

   ## Examples

   ```ocaml Duration.min (Duration.from_secs 5) (Duration.from_secs 10) |>
   Duration.to_secs (* 5 *) ```
*)
val min: t -> t -> t

(**
   Returns the larger of two durations.

   ## Examples

   ```ocaml Duration.max (Duration.from_secs 5) (Duration.from_secs 10) |>
   Duration.to_secs (* 10 *) ```
*)
val max: t -> t -> t

(**
   Compares two durations. Returns negative if first < second, 0 if equal,
   positive if first > second.

   ## Examples

   ```ocaml Duration.compare (Duration.from_secs 5) (Duration.from_secs 10) (*
   < 0 *) ```
*)
val compare: t -> t -> Order.t

(**
   Tests equality of two durations.

   ## Examples

   ```ocaml Duration.equal (Duration.from_secs 5) (Duration.from_millis 5000)
   (* true *) ```
*)
val equal: t -> t -> bool
