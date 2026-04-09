open Std
module Test = Std.Test
module Kernel = Kernel_new

let ( let* ) = Result.and_then

let lift_system_time result =
  match result with
  | Kernel.Result.Ok value -> Ok value
  | Kernel.Result.Error error -> Error (Kernel.Error.to_string
    (Kernel.Error.of_time_system_time error))

let lift_monotonic result =
  match result with
  | Kernel.Result.Ok value -> Ok value
  | Kernel.Result.Error error -> Error (Kernel.Error.to_string (Kernel.Error.of_time_monotonic error))

let lift_timer result =
  match result with
  | Kernel.Result.Ok value -> Ok value
  | Kernel.Result.Error error -> Error (Kernel.Error.to_string (Kernel.Error.of_time_timer error))

let lift_async result =
  match result with
  | Kernel.Result.Ok value -> Ok value
  | Kernel.Result.Error error -> Error (Kernel.Error.to_string (Kernel.Error.of_async error))

let protect = fun ~finally fn ->
  try
    let value = fn () in
    finally ();
    value
  with
  | error ->
      finally ();
      raise error

let with_poll = fun fn ->
  let* poll = lift_async (Kernel.Async.Poll.make ()) in
  protect
    ~finally:(fun () ->
      let _ = Kernel.Async.Poll.close poll in
      ())
    (fun () -> fn poll)

let wait_for_timer = fun poll ~token timer ->
  let source = Kernel.Time.Timer.to_source timer in
  let* () = lift_async (Kernel.Async.Poll.register poll token Kernel.Async.Interest.readable source) in
  protect
    ~finally:(fun () ->
      let _ = Kernel.Async.Poll.deregister poll source in
      ())
    (fun () ->
      let* events = lift_async (Kernel.Async.Poll.poll ~timeout:100_000_000L ~max_events:8 poll) in
      if
        List.exists
          (fun event ->
            Kernel.Async.Token.equal token (Kernel.Async.Event.token event)
            && Kernel.Async.Event.is_readable event)
          events
      then
        Ok ()
      else
        Error "expected timer source to wake the poller")

let has_readable_token = fun token events ->
  List.exists
    (fun event ->
      Kernel.Async.Token.equal token (Kernel.Async.Event.token event)
      && Kernel.Async.Event.is_readable event)
    events

let test_of_parts_roundtrips = fun _ctx ->
  let* value = lift_system_time (Kernel.Time.SystemTime.of_parts ~secs:42 ~nanos:123_456_789) in
  let (secs, nanos) = Kernel.Time.SystemTime.to_parts value in
  if secs = 42 && nanos = 123_456_789 then
    Ok ()
  else
    Error "expected system time parts roundtrip"

let test_now_is_at_or_after_epoch = fun _ctx ->
  let* now = lift_system_time (Kernel.Time.SystemTime.now ()) in
  if Kernel.Time.SystemTime.compare now Kernel.Time.SystemTime.epoch >= 0 then
    Ok ()
  else
    Error "expected current system time to be at or after the epoch"

let test_now_has_valid_nanoseconds = fun _ctx ->
  let* now = lift_system_time (Kernel.Time.SystemTime.now ()) in
  let nanos = Kernel.Time.SystemTime.subsec_nanos now in
  if nanos >= 0 && nanos < 1_000_000_000 then
    Ok ()
  else
    Error "expected current system time nanoseconds to be normalized"

let test_system_time_diff_ns_matches_parts = fun _ctx ->
  let* later = lift_system_time (Kernel.Time.SystemTime.of_parts ~secs:12 ~nanos:250_000_000) in
  let* earlier = lift_system_time (Kernel.Time.SystemTime.of_parts ~secs:10 ~nanos:125_000_000) in
  if Kernel.Time.SystemTime.diff_ns later earlier = 2_125_000_000L then
    Ok ()
  else
    Error "expected system time diff_ns to preserve raw nanosecond deltas"

let test_monotonic_now_is_non_decreasing = fun _ctx ->
  let* earlier = lift_monotonic (Kernel.Time.Monotonic.now ()) in
  let* later = lift_monotonic (Kernel.Time.Monotonic.now ()) in
  if Kernel.Time.Monotonic.compare earlier later <= 0 then
    Ok ()
  else
    Error "expected monotonic now to be non-decreasing"

let test_monotonic_now_has_valid_nanoseconds = fun _ctx ->
  let* now = lift_monotonic (Kernel.Time.Monotonic.now ()) in
  let nanos = Kernel.Time.Monotonic.subsec_nanos now in
  if nanos >= 0 && nanos < 1_000_000_000 then
    Ok ()
  else
    Error "expected monotonic now nanoseconds to be normalized"

let test_monotonic_diff_ns_matches_parts = fun _ctx ->
  let* earlier = lift_monotonic (Kernel.Time.Monotonic.now ()) in
  let* later = lift_monotonic (Kernel.Time.Monotonic.now ()) in
  let earlier_secs, earlier_nanos = Kernel.Time.Monotonic.to_parts earlier in
  let later_secs, later_nanos = Kernel.Time.Monotonic.to_parts later in
  let expected = Int64.add
    (Int64.mul (Int64.of_int (later_secs - earlier_secs)) 1_000_000_000L)
    (Int64.of_int (later_nanos - earlier_nanos)) in
  if Kernel.Time.Monotonic.diff_ns later earlier = expected then
    Ok ()
  else
    Error "expected monotonic diff_ns to preserve raw nanosecond deltas"

let test_timer_rejects_non_positive_timeout = fun _ctx ->
  match Kernel.Time.Timer.after_ns 0L with
  | Kernel.Result.Error (Kernel.Time.Timer.InvalidTimeoutNs { timeout_ns=0L }) -> Ok ()
  | Kernel.Result.Error error -> Error (Kernel.Time.Timer.error_to_string error)
  | Kernel.Result.Ok _ -> Error "expected timer source construction to reject a non-positive timeout"

let test_timer_after_ns_wakes_poll = fun _ctx ->
  with_poll
    (fun poll ->
      let* timer = lift_timer (Kernel.Time.Timer.after_ns 5_000_000L) in
      wait_for_timer poll ~token:(Kernel.Async.Token.make 700) timer)

let test_timer_every_ns_repeats = fun _ctx ->
  with_poll
    (fun poll ->
      let* timer = lift_timer (Kernel.Time.Timer.every_ns 5_000_000L) in
      let source = Kernel.Time.Timer.to_source timer in
      let token = Kernel.Async.Token.make 701 in
      let* () = lift_async
        (Kernel.Async.Poll.register poll token Kernel.Async.Interest.readable source) in
      protect
        ~finally:(fun () ->
          let _ = Kernel.Async.Poll.deregister poll source in
          ())
        (fun () ->
          let rec poll_twice remaining =
            if remaining = 0 then
              Ok ()
            else
              let* events = lift_async
                (Kernel.Async.Poll.poll ~timeout:100_000_000L ~max_events:8 poll) in
              if
                List.exists
                  (fun event ->
                    Kernel.Async.Token.equal token (Kernel.Async.Event.token event)
                    && Kernel.Async.Event.is_readable event)
                  events
              then
                poll_twice (remaining - 1)
              else
                Error "expected repeating timer to remain readable across multiple polls"
          in
          poll_twice 2))

let test_timer_deregister_after_first_tick_stops_future_events = fun _ctx ->
  with_poll
    (fun poll ->
      let* timer = lift_timer (Kernel.Time.Timer.every_ns 5_000_000L) in
      let source = Kernel.Time.Timer.to_source timer in
      let token = Kernel.Async.Token.make 702 in
      let* () = lift_async
        (Kernel.Async.Poll.register poll token Kernel.Async.Interest.readable source) in
      protect
        ~finally:(fun () ->
          let _ = Kernel.Async.Poll.deregister poll source in
          ())
        (fun () ->
          let* first = lift_async (Kernel.Async.Poll.poll ~timeout:100_000_000L ~max_events:8 poll) in
          if not (has_readable_token token first) then
            Error "expected repeating timer to fire before deregistration"
          else
            let* () = lift_async (Kernel.Async.Poll.deregister poll source) in
            let* second = lift_async (Kernel.Async.Poll.poll ~timeout:20_000_000L ~max_events:8 poll) in
            if has_readable_token token second then
              Error "expected deregistered timer to stop producing events"
            else
              Ok ()))

let test_timer_every_ns_spacing_is_reasonable = fun _ctx ->
  with_poll
    (fun poll ->
      let interval_ns = 20_000_000L in
      let* timer = lift_timer (Kernel.Time.Timer.every_ns interval_ns) in
      let source = Kernel.Time.Timer.to_source timer in
      let token = Kernel.Async.Token.make 703 in
      let* start = lift_monotonic (Kernel.Time.Monotonic.now ()) in
      let* () = lift_async
        (Kernel.Async.Poll.register poll token Kernel.Async.Interest.readable source) in
      protect
        ~finally:(fun () ->
          let _ = Kernel.Async.Poll.deregister poll source in
          ())
        (fun () ->
          let rec wait_for_ticks seen =
            if seen = 3 then
              let* finish = lift_monotonic (Kernel.Time.Monotonic.now ()) in
              let elapsed = Kernel.Time.Monotonic.diff_ns finish start in
              if elapsed >= 10_000_000L && elapsed < 1_000_000_000L then
                Ok ()
              else
                Error "expected repeating timer ticks to stay within a reasonable tolerance"
            else
              let* events = lift_async
                (Kernel.Async.Poll.poll ~timeout:200_000_000L ~max_events:8 poll) in
              if has_readable_token token events then
                wait_for_ticks (seen + 1)
              else
                Error "expected repeating timer to keep producing ticks over time"
          in
          wait_for_ticks 0))

let tests = [
  Test.case "Time.SystemTime of_parts roundtrips" test_of_parts_roundtrips;
  Test.case "Time.SystemTime now is at or after epoch" test_now_is_at_or_after_epoch;
  Test.case "Time.SystemTime now has normalized nanoseconds" test_now_has_valid_nanoseconds;
  Test.case "Time.SystemTime diff_ns matches raw parts" test_system_time_diff_ns_matches_parts;
  Test.case "Time.Monotonic now is non-decreasing" test_monotonic_now_is_non_decreasing;
  Test.case "Time.Monotonic now has normalized nanoseconds" test_monotonic_now_has_valid_nanoseconds;
  Test.case "Time.Monotonic diff_ns matches raw parts" test_monotonic_diff_ns_matches_parts;
  Test.case "Time.Timer rejects non-positive timeout" test_timer_rejects_non_positive_timeout;
  Test.case "Time.Timer after_ns wakes poll" test_timer_after_ns_wakes_poll;
  Test.case "Time.Timer every_ns repeats" test_timer_every_ns_repeats;
  Test.case "Time.Timer deregister after first tick stops future events" test_timer_deregister_after_first_tick_stops_future_events;
  Test.case "Time.Timer every_ns spacing stays within a reasonable tolerance" test_timer_every_ns_spacing_is_reasonable;
]

let main = fun ~args -> Test.Cli.main ~name:"kernel_new_system_time_tests" ~tests ~args

let () = Actors.run ~main ~args:Env.args ()
