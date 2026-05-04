open Std

module Test = Std.Test
module Kernel = Kernel

let ( let* ) value fn = Result.and_then value ~fn

let lift_system_time result =
  match result with
  | Kernel.Result.Ok value -> Ok value
  | Kernel.Result.Error error ->
      Error (Kernel.Error.to_string (Kernel.Error.from_time_system_time error))

let lift_monotonic result =
  match result with
  | Kernel.Result.Ok value -> Ok value
  | Kernel.Result.Error error ->
      Error (Kernel.Error.to_string (Kernel.Error.from_time_monotonic error))

let lift_timer result =
  match result with
  | Kernel.Result.Ok value -> Ok value
  | Kernel.Result.Error error -> Error (Kernel.Error.to_string (Kernel.Error.from_time_timer error))

let lift_async result =
  match result with
  | Kernel.Result.Ok value -> Ok value
  | Kernel.Result.Error error -> Error (Kernel.Error.to_string (Kernel.Error.from_async error))

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
        List.any
          events
          ~fn:(fun event ->
            Kernel.Async.Token.equal token (Kernel.Async.Event.token event)
            && Kernel.Async.Event.is_readable event)
      then
        Ok ()
      else
        Error "expected timer source to wake the poller")

let has_readable_token = fun token events ->
  List.any
    events
    ~fn:(fun event ->
      Kernel.Async.Token.equal token (Kernel.Async.Event.token event)
      && Kernel.Async.Event.is_readable event)

let wait_for_readable_token = fun poll ~token ->
  let rec loop attempts =
    if attempts = 0 then
      Error "expected timer source to report readability"
    else
      let* events = lift_async (Kernel.Async.Poll.poll ~timeout:100_000_000L ~max_events:8 poll) in
      if has_readable_token token events then
        Ok ()
      else
        loop (attempts - 1)
  in
  loop 8

let test_of_parts_roundtrips = fun _ctx ->
  let* value = lift_system_time (Kernel.Time.SystemTime.from_parts ~secs:42 ~nanos:123_456_789) in
  let (secs, nanos) = Kernel.Time.SystemTime.to_parts value in
  if secs = 42 && nanos = 123_456_789 then
    Ok ()
  else
    Error "expected system time parts roundtrip"

let test_now_is_at_or_after_epoch = fun _ctx ->
  let* now = lift_system_time (Kernel.Time.SystemTime.now ()) in
  if match Kernel.Time.SystemTime.compare now Kernel.Time.SystemTime.epoch with
  | Kernel.Order.LT -> false
  | Kernel.Order.EQ
  | Kernel.Order.GT -> true then
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
  let* later = lift_system_time (Kernel.Time.SystemTime.from_parts ~secs:12 ~nanos:250_000_000) in
  let* earlier = lift_system_time (Kernel.Time.SystemTime.from_parts ~secs:10 ~nanos:125_000_000) in
  if Kernel.Time.SystemTime.diff_ns later earlier = 2_125_000_000L then
    Ok ()
  else
    Error "expected system time diff_ns to preserve raw nanosecond deltas"

let test_monotonic_now_is_non_decreasing = fun _ctx ->
  let* earlier = lift_monotonic (Kernel.Time.Monotonic.now ()) in
  let* later = lift_monotonic (Kernel.Time.Monotonic.now ()) in
  if match Kernel.Time.Monotonic.compare earlier later with
  | Kernel.Order.LT
  | Kernel.Order.EQ -> true
  | Kernel.Order.GT -> false then
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
  let (earlier_secs, earlier_nanos) = Kernel.Time.Monotonic.to_parts earlier in
  let (later_secs, later_nanos) = Kernel.Time.Monotonic.to_parts later in
  let expected =
    Int64.add
      (Int64.mul (Int64.from_int (later_secs - earlier_secs)) 1_000_000_000L)
      (Int64.from_int (later_nanos - earlier_nanos))
  in
  if Kernel.Time.Monotonic.diff_ns later earlier = expected then
    Ok ()
  else
    Error "expected monotonic diff_ns to preserve raw nanosecond deltas"

let test_timer_rejects_non_positive_timeout = fun _ctx ->
  match Kernel.Time.Timer.after_ns 0L with
  | Kernel.Result.Error (Kernel.Time.Timer.InvalidTimeoutNs { timeout_ns = 0L }) -> Ok ()
  | Kernel.Result.Error error -> Error (Kernel.Time.Timer.error_to_string error)
  | Kernel.Result.Ok _ ->
      Error "expected timer source construction to reject a non-positive timeout"

let test_timer_after_ns_wakes_poll = fun _ctx ->
  with_poll
    (fun poll ->
      let* timer = lift_timer (Kernel.Time.Timer.after_ns 5_000_000L) in
      wait_for_timer poll ~token:(Kernel.Async.Token.make 700) timer)

let test_timer_after_ns_fires_only_once_per_registration = fun _ctx ->
  with_poll
    (fun poll ->
      let* timer = lift_timer (Kernel.Time.Timer.after_ns 5_000_000L) in
      let source = Kernel.Time.Timer.to_source timer in
      let token = Kernel.Async.Token.make 704 in
      let* () =
        lift_async (Kernel.Async.Poll.register poll token Kernel.Async.Interest.readable source)
      in
      protect
        ~finally:(fun () ->
          let _ = Kernel.Async.Poll.deregister poll source in
          ())
        (fun () ->
          let* () = wait_for_readable_token poll ~token in
          let* quiet = lift_async (Kernel.Async.Poll.poll ~timeout:20_000_000L ~max_events:8 poll) in
          if has_readable_token token quiet then
            Error "expected one-shot timer to stay quiet after its first event"
          else
            Ok ()))

let test_timer_after_ns_can_be_rearmed_after_fire = fun _ctx ->
  with_poll
    (fun poll ->
      let* timer = lift_timer (Kernel.Time.Timer.after_ns 5_000_000L) in
      let source = Kernel.Time.Timer.to_source timer in
      let first_token = Kernel.Async.Token.make "first-after-ns" in
      let second_token = Kernel.Async.Token.make "second-after-ns" in
      let* () =
        lift_async
          (Kernel.Async.Poll.register poll first_token Kernel.Async.Interest.readable source)
      in
      protect
        ~finally:(fun () ->
          let _ = Kernel.Async.Poll.deregister poll source in
          ())
        (fun () ->
          let* () = wait_for_readable_token poll ~token:first_token in
          let* () =
            lift_async
              (Kernel.Async.Poll.reregister poll second_token Kernel.Async.Interest.readable source)
          in
          wait_for_readable_token poll ~token:second_token))

let test_many_same_tick_timers_wake_the_poller = fun _ctx ->
  with_poll
    (fun poll ->
      let rec create remaining acc =
        if remaining = 0 then
          Ok (List.reverse acc)
        else
          let* timer = lift_timer (Kernel.Time.Timer.after_ns 5_000_000L) in
          create (remaining - 1) (timer :: acc)
      in
      let* timers = create 12 [] in
      let rec register index = fun __tmp1 ->
        match __tmp1 with
        | [] -> Ok ()
        | timer :: rest ->
            let* () =
              lift_async
                (Kernel.Async.Poll.register
                  poll
                  (Kernel.Async.Token.make index)
                  Kernel.Async.Interest.readable
                  (Kernel.Time.Timer.to_source timer))
            in
            register (index + 1) rest
      in
      let seen = Kernel.Array.make ~count:12 ~value:false in
      let rec mark = fun __tmp1 ->
        match __tmp1 with
        | [] -> ()
        | event :: rest ->
            if Kernel.Async.Event.is_readable event then
              let token = Kernel.Async.Token.unsafe_value (Kernel.Async.Event.token event) in
              if token >= 0 && token < 12 then
                Kernel.Array.set seen ~at:token ~value:true;
            mark rest
      in
      let rec all_seen index =
        if index = 12 then
          true
        else if Kernel.Array.get_unchecked seen ~at:index then
          all_seen (index + 1)
        else
          false
      in
      let* () = register 0 timers in
      let rec poll_until attempts =
        if attempts = 0 then
          Error "expected many same-tick timers to wake the poller"
        else
          let* events =
            lift_async (Kernel.Async.Poll.poll ~timeout:100_000_000L ~max_events:32 poll)
          in
          mark events;
        if all_seen 0 then
          Ok ()
        else
          poll_until (attempts - 1)
      in
      poll_until 8)

let test_timer_every_ns_repeats = fun _ctx ->
  with_poll
    (fun poll ->
      let* timer = lift_timer (Kernel.Time.Timer.every_ns 5_000_000L) in
      let source = Kernel.Time.Timer.to_source timer in
      let token = Kernel.Async.Token.make 701 in
      let* () =
        lift_async (Kernel.Async.Poll.register poll token Kernel.Async.Interest.readable source)
      in
      protect
        ~finally:(fun () ->
          let _ = Kernel.Async.Poll.deregister poll source in
          ())
        (fun () ->
          let rec poll_twice remaining =
            if remaining = 0 then
              Ok ()
            else
              let* events =
                lift_async (Kernel.Async.Poll.poll ~timeout:100_000_000L ~max_events:8 poll)
              in
              if
                List.any
                  events
                  ~fn:(fun event ->
                    Kernel.Async.Token.equal token (Kernel.Async.Event.token event)
                    && Kernel.Async.Event.is_readable event)
              then
                poll_twice (remaining - 1)
              else
                Error "expected repeating timer to remain readable across multiple polls"
          in
          poll_twice 2))

let test_timer_reregister_updates_token = fun _ctx ->
  with_poll
    (fun poll ->
      let* timer = lift_timer (Kernel.Time.Timer.after_ns 5_000_000L) in
      let source = Kernel.Time.Timer.to_source timer in
      let token_a = Kernel.Async.Token.make "first-timer-token" in
      let token_b = Kernel.Async.Token.make "second-timer-token" in
      let* () =
        lift_async (Kernel.Async.Poll.register poll token_a Kernel.Async.Interest.readable source)
      in
      protect
        ~finally:(fun () ->
          let _ = Kernel.Async.Poll.deregister poll source in
          ())
        (fun () ->
          let* () =
            lift_async
              (Kernel.Async.Poll.reregister poll token_b Kernel.Async.Interest.readable source)
          in
          let* () = wait_for_readable_token poll ~token:token_b in
          Ok ()))

let test_timer_repeated_register_reregister_and_deregister_stays_healthy = fun _ctx ->
  with_poll
    (fun poll ->
      let* timer = lift_timer (Kernel.Time.Timer.every_ns 5_000_000L) in
      let source = Kernel.Time.Timer.to_source timer in
      let rec loop remaining =
        if remaining = 0 then
          Ok ()
        else
          let token_a = Kernel.Async.Token.make ("timer", remaining) in
          let token_b = Kernel.Async.Token.make ("timer-reregistered", remaining) in
          let* () =
            lift_async
              (Kernel.Async.Poll.register poll token_a Kernel.Async.Interest.readable source)
          in
          let* () =
            lift_async
              (Kernel.Async.Poll.reregister poll token_b Kernel.Async.Interest.readable source)
          in
          let* () = wait_for_readable_token poll ~token:token_b in
          let* () = lift_async (Kernel.Async.Poll.deregister poll source) in
          let* quiet = lift_async (Kernel.Async.Poll.poll ~timeout:20_000_000L ~max_events:8 poll) in
          if has_readable_token token_b quiet then
            Error "expected deregistered timer to stop producing events between cycles"
          else
            loop (remaining - 1)
      in
      loop 16)

let test_timer_source_can_be_reused_after_deregister = fun _ctx ->
  with_poll
    (fun poll ->
      let* timer = lift_timer (Kernel.Time.Timer.after_ns 5_000_000L) in
      let source = Kernel.Time.Timer.to_source timer in
      let first_token = Kernel.Async.Token.make "first-timer-registration" in
      let second_token = Kernel.Async.Token.make "second-timer-registration" in
      let* () =
        lift_async
          (Kernel.Async.Poll.register poll first_token Kernel.Async.Interest.readable source)
      in
      let* () = lift_async (Kernel.Async.Poll.deregister poll source) in
      let* quiet = lift_async (Kernel.Async.Poll.poll ~timeout:20_000_000L ~max_events:8 poll) in
      if has_readable_token first_token quiet then
        Error "expected deregistered timer source to stay quiet before reuse"
      else
        let* () =
          lift_async
            (Kernel.Async.Poll.register poll second_token Kernel.Async.Interest.readable source)
        in
        protect
          ~finally:(fun () ->
            let _ = Kernel.Async.Poll.deregister poll source in
            ())
          (fun () -> wait_for_readable_token poll ~token:second_token))

let test_timer_deregister_after_after_ns_fire_is_harmless = fun _ctx ->
  with_poll
    (fun poll ->
      let* timer = lift_timer (Kernel.Time.Timer.after_ns 5_000_000L) in
      let source = Kernel.Time.Timer.to_source timer in
      let token = Kernel.Async.Token.make "after-ns-deregistered" in
      let* () =
        lift_async (Kernel.Async.Poll.register poll token Kernel.Async.Interest.readable source)
      in
      let* () = wait_for_readable_token poll ~token in
      let* () = lift_async (Kernel.Async.Poll.deregister poll source) in
      let* quiet = lift_async (Kernel.Async.Poll.poll ~timeout:20_000_000L ~max_events:8 poll) in
      if has_readable_token token quiet then
        Error "expected deregistered one-shot timer to stay quiet after firing"
      else
        Ok ())

let test_timer_deregister_after_first_tick_stops_future_events = fun _ctx ->
  with_poll
    (fun poll ->
      let* timer = lift_timer (Kernel.Time.Timer.every_ns 5_000_000L) in
      let source = Kernel.Time.Timer.to_source timer in
      let token = Kernel.Async.Token.make 702 in
      let* () =
        lift_async (Kernel.Async.Poll.register poll token Kernel.Async.Interest.readable source)
      in
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
            let* second =
              lift_async (Kernel.Async.Poll.poll ~timeout:20_000_000L ~max_events:8 poll)
            in
            if has_readable_token token second then
              Error "expected deregistered timer to stop producing events"
            else
              Ok ()))

let test_timer_deregister_before_first_tick_stays_quiet = fun _ctx ->
  with_poll
    (fun poll ->
      let* timer = lift_timer (Kernel.Time.Timer.every_ns 5_000_000L) in
      let source = Kernel.Time.Timer.to_source timer in
      let token = Kernel.Async.Token.make "timer-deregister-before-first-tick" in
      let* () =
        lift_async (Kernel.Async.Poll.register poll token Kernel.Async.Interest.readable source)
      in
      let* () = lift_async (Kernel.Async.Poll.deregister poll source) in
      let* quiet = lift_async (Kernel.Async.Poll.poll ~timeout:20_000_000L ~max_events:8 poll) in
      if has_readable_token token quiet then
        Error "expected deregistered repeating timer to stay quiet before its first tick"
      else
        Ok ())

let test_timer_every_ns_spacing_is_reasonable = fun _ctx ->
  with_poll
    (fun poll ->
      let interval_ns = 20_000_000L in
      let* timer = lift_timer (Kernel.Time.Timer.every_ns interval_ns) in
      let source = Kernel.Time.Timer.to_source timer in
      let token = Kernel.Async.Token.make 703 in
      let* start = lift_monotonic (Kernel.Time.Monotonic.now ()) in
      let* () =
        lift_async (Kernel.Async.Poll.register poll token Kernel.Async.Interest.readable source)
      in
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
              let* events =
                lift_async (Kernel.Async.Poll.poll ~timeout:200_000_000L ~max_events:8 poll)
              in
              if has_readable_token token events then
                wait_for_ticks (seen + 1)
              else
                Error "expected repeating timer to keep producing ticks over time"
          in
          wait_for_ticks 0))

let test_timer_after_ns_elapsed_time_is_reasonable = fun _ctx ->
  with_poll
    (fun poll ->
      let interval_ns = 20_000_000L in
      let* timer = lift_timer (Kernel.Time.Timer.after_ns interval_ns) in
      let source = Kernel.Time.Timer.to_source timer in
      let token = Kernel.Async.Token.make 705 in
      let* start = lift_monotonic (Kernel.Time.Monotonic.now ()) in
      let* () =
        lift_async (Kernel.Async.Poll.register poll token Kernel.Async.Interest.readable source)
      in
      protect
        ~finally:(fun () ->
          let _ = Kernel.Async.Poll.deregister poll source in
          ())
        (fun () ->
          let* () = wait_for_readable_token poll ~token in
          let* finish = lift_monotonic (Kernel.Time.Monotonic.now ()) in
          let elapsed = Kernel.Time.Monotonic.diff_ns finish start in
          if elapsed >= 10_000_000L && elapsed < 1_000_000_000L then
            Ok ()
          else
            Error "expected one-shot timer latency to stay within a reasonable tolerance"))

let test_system_time_rejects_negative_nanoseconds = fun _ctx ->
  match Kernel.Time.SystemTime.from_parts ~secs:1 ~nanos:(-1) with
  | Kernel.Result.Error (
    Kernel.Time.SystemTime.InvalidNanoseconds { nanos = (-1) }
  ) ->
      Ok ()
  | Kernel.Result.Error error -> Error (Kernel.Time.SystemTime.error_to_string error)
  | Kernel.Result.Ok _ -> Error "expected SystemTime.from_parts to reject negative nanoseconds"

let test_system_time_rejects_upper_bound_nanoseconds = fun _ctx ->
  match Kernel.Time.SystemTime.from_parts ~secs:1 ~nanos:1_000_000_000 with
  | Kernel.Result.Error (
    Kernel.Time.SystemTime.InvalidNanoseconds { nanos = 1_000_000_000 }
  ) ->
      Ok ()
  | Kernel.Result.Error error -> Error (Kernel.Time.SystemTime.error_to_string error)
  | Kernel.Result.Ok _ ->
      Error "expected SystemTime.from_parts to keep the nanosecond upper bound exclusive"

let test_system_time_accessors_match_the_constructor = fun _ctx ->
  let* value = lift_system_time (Kernel.Time.SystemTime.from_parts ~secs:17 ~nanos:123_456_789) in
  let (secs, nanos) = Kernel.Time.SystemTime.to_parts value in
  if
    secs = 17
    && nanos = 123_456_789
    && Kernel.Time.SystemTime.secs value = 17
    && Kernel.Time.SystemTime.subsec_nanos value = 123_456_789
  then
    Ok ()
  else
    Error "expected SystemTime accessors to preserve constructor inputs exactly"

let test_system_time_compare_and_equal_are_antisymmetric = fun _ctx ->
  let* earlier = lift_system_time (Kernel.Time.SystemTime.from_parts ~secs:1 ~nanos:5) in
  let* same = lift_system_time (Kernel.Time.SystemTime.from_parts ~secs:1 ~nanos:5) in
  let* later = lift_system_time (Kernel.Time.SystemTime.from_parts ~secs:1 ~nanos:6) in
  if
    Kernel.Time.SystemTime.equal earlier same
    && Kernel.Time.SystemTime.compare earlier same = Kernel.Order.EQ
    && Kernel.Time.SystemTime.compare earlier later = Kernel.Order.LT
    && Kernel.Time.SystemTime.compare later earlier = Kernel.Order.GT
  then
    Ok ()
  else
    Error "expected SystemTime compare and equal to preserve antisymmetric ordering"

let test_system_time_diff_ns_is_antisymmetric = fun _ctx ->
  let* left = lift_system_time (Kernel.Time.SystemTime.from_parts ~secs:10 ~nanos:5) in
  let* right = lift_system_time (Kernel.Time.SystemTime.from_parts ~secs:7 ~nanos:10) in
  if
    Kernel.Time.SystemTime.diff_ns left right
    = Int64.neg (Kernel.Time.SystemTime.diff_ns right left)
  then
    Ok ()
  else
    Error "expected SystemTime.diff_ns to be antisymmetric"

let test_monotonic_accessors_are_consistent_on_now = fun _ctx ->
  let* now = lift_monotonic (Kernel.Time.Monotonic.now ()) in
  let (secs, nanos) = Kernel.Time.Monotonic.to_parts now in
  if secs = Kernel.Time.Monotonic.secs now && nanos = Kernel.Time.Monotonic.subsec_nanos now then
    Ok ()
  else
    Error "expected Monotonic accessors to agree with to_parts"

let test_timer_accessors_report_timeout_and_repeat_flag = fun _ctx ->
  let* one_shot = lift_timer (Kernel.Time.Timer.after_ns 7L) in
  let* repeating = lift_timer (Kernel.Time.Timer.every_ns 11L) in
  if
    Kernel.Time.Timer.timeout_ns one_shot = 7L
    && not (Kernel.Time.Timer.repeats one_shot)
    && Kernel.Time.Timer.timeout_ns repeating = 11L
    && Kernel.Time.Timer.repeats repeating
  then
    Ok ()
  else
    Error "expected Timer accessors to preserve the configured timeout and repeat flag"

let test_one_shot_timer_stays_quiet_before_deadline = fun _ctx ->
  with_poll
    (fun poll ->
      let* timer = lift_timer (Kernel.Time.Timer.after_ns 20_000_000L) in
      let token = Kernel.Async.Token.make 15 in
      let source = Kernel.Time.Timer.to_source timer in
      let* () =
        lift_async (Kernel.Async.Poll.register poll token Kernel.Async.Interest.readable source)
      in
      protect
        ~finally:(fun () ->
          let _ = Kernel.Async.Poll.deregister poll source in
          ())
        (fun () ->
          let* events = lift_async (Kernel.Async.Poll.poll ~timeout:0L poll) in
          if has_readable_token token events then
            Error "expected a one-shot timer to stay quiet before its deadline"
          else
            Ok ()))

let test_reregistering_one_shot_before_fire_resets_the_deadline = fun _ctx ->
  with_poll
    (fun poll ->
      let* timer = lift_timer (Kernel.Time.Timer.after_ns 25_000_000L) in
      let source = Kernel.Time.Timer.to_source timer in
      let token = Kernel.Async.Token.make 16 in
      let replacement = Kernel.Async.Token.make 17 in
      let* () =
        lift_async (Kernel.Async.Poll.register poll token Kernel.Async.Interest.readable source)
      in
      protect
        ~finally:(fun () ->
          let _ = Kernel.Async.Poll.deregister poll source in
          ())
        (fun () ->
          let* quiet_before = lift_async (Kernel.Async.Poll.poll ~timeout:15_000_000L poll) in
          if has_readable_token token quiet_before then
            Error "expected the original timer deadline to still be in the future"
          else
            let* () =
              lift_async
                (Kernel.Async.Poll.reregister poll replacement Kernel.Async.Interest.readable source)
            in
            let* quiet_after = lift_async (Kernel.Async.Poll.poll ~timeout:15_000_000L poll) in
            if has_readable_token replacement quiet_after then
              Error "expected reregister to reset the one-shot deadline from the new call site"
            else
              let* ready = lift_async (Kernel.Async.Poll.poll ~timeout:40_000_000L poll) in
              if has_readable_token replacement ready then
                Ok ()
              else
                Error "expected the rearmed one-shot timer to fire after the reset deadline"))

let tests = [
  Test.case "Time.SystemTime from_parts roundtrips" test_of_parts_roundtrips;
  Test.case "Time.SystemTime now is at or after epoch" test_now_is_at_or_after_epoch;
  Test.case "Time.SystemTime now has normalized nanoseconds" test_now_has_valid_nanoseconds;
  Test.case "Time.SystemTime diff_ns matches raw parts" test_system_time_diff_ns_matches_parts;
  Test.case "Time.Monotonic now is non-decreasing" test_monotonic_now_is_non_decreasing;
  Test.case "Time.Monotonic now has normalized nanoseconds" test_monotonic_now_has_valid_nanoseconds;
  Test.case "Time.Monotonic diff_ns matches raw parts" test_monotonic_diff_ns_matches_parts;
  Test.case "Time.Timer rejects non-positive timeout" test_timer_rejects_non_positive_timeout;
  Test.case "Time.Timer after_ns wakes poll" test_timer_after_ns_wakes_poll;
  Test.case
    "Time.Timer after_ns fires only once per registration"
    test_timer_after_ns_fires_only_once_per_registration;
  Test.case
    "Time.Timer after_ns can be rearmed after fire"
    test_timer_after_ns_can_be_rearmed_after_fire;
  Test.case
    "Time.Timer many same-tick timers wake the poller"
    test_many_same_tick_timers_wake_the_poller;
  Test.case "Time.Timer every_ns repeats" test_timer_every_ns_repeats;
  Test.case "Time.Timer reregister updates token" test_timer_reregister_updates_token;
  Test.case
    "Time.Timer deregister after first tick stops future events"
    test_timer_deregister_after_first_tick_stops_future_events;
  Test.case
    "Time.Timer deregister before first tick stays quiet"
    test_timer_deregister_before_first_tick_stays_quiet;
  Test.case
    "Time.Timer repeated register, reregister, and deregister stays healthy"
    test_timer_repeated_register_reregister_and_deregister_stays_healthy;
  Test.case
    "Time.Timer source can be reused after deregister"
    test_timer_source_can_be_reused_after_deregister;
  Test.case
    "Time.Timer deregister after after_ns fire is harmless"
    test_timer_deregister_after_after_ns_fire_is_harmless;
  Test.case
    "Time.Timer every_ns spacing stays within a reasonable tolerance"
    test_timer_every_ns_spacing_is_reasonable;
  Test.case
    "Time.Timer after_ns latency stays within a reasonable tolerance"
    test_timer_after_ns_elapsed_time_is_reasonable;
  Test.case
    "SystemTime.from_parts rejects negative nanoseconds"
    test_system_time_rejects_negative_nanoseconds;
  Test.case
    "SystemTime.from_parts rejects the exclusive upper nanosecond bound"
    test_system_time_rejects_upper_bound_nanoseconds;
  Test.case
    "SystemTime accessors match the constructor"
    test_system_time_accessors_match_the_constructor;
  Test.case
    "SystemTime compare and equal are antisymmetric"
    test_system_time_compare_and_equal_are_antisymmetric;
  Test.case "SystemTime.diff_ns is antisymmetric" test_system_time_diff_ns_is_antisymmetric;
  Test.case
    "Monotonic accessors are consistent on now"
    test_monotonic_accessors_are_consistent_on_now;
  Test.case
    "Timer accessors report timeout and repeat flag"
    test_timer_accessors_report_timeout_and_repeat_flag;
  Test.case
    "One-shot timers stay quiet before their deadline"
    test_one_shot_timer_stays_quiet_before_deadline;
  Test.case
    "Reregistering a one-shot before fire resets the deadline"
    test_reregistering_one_shot_before_fire_resets_the_deadline;
]

let main ~args =
  Test.Cli.main ~execution_mode:Test.Cli.Linear ~name:"kernel_new_system_time_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
