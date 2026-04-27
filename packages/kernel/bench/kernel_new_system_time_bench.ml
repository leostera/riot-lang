open Std

module Kernel = Kernel

let panic_async = fun error ->
  Kernel.SystemError.panic
    (Kernel.Error.to_string (Kernel.Error.from_async error))

let lift_async result =
  match result with
  | Kernel.Result.Ok value -> value
  | Kernel.Result.Error error -> panic_async error

let bench_now = fun () ->
  match Kernel.Time.SystemTime.now () with
  | Kernel.Result.Ok _ -> ()
  | Kernel.Result.Error error ->
      Kernel.SystemError.panic (Kernel.Error.to_string (Kernel.Error.from_time_system_time error))

let bench_compare = fun () ->
  match (Kernel.Time.SystemTime.now (), Kernel.Time.SystemTime.now ()) with
  | (Kernel.Result.Ok left, Kernel.Result.Ok right) ->
      let _ = Kernel.Time.SystemTime.compare left right in
      ()
  | (Kernel.Result.Error error, _)
  | (_, Kernel.Result.Error error) ->
      Kernel.SystemError.panic (Kernel.Error.to_string (Kernel.Error.from_time_system_time error))

let bench_monotonic_now = fun () ->
  match Kernel.Time.Monotonic.now () with
  | Kernel.Result.Ok _ -> ()
  | Kernel.Result.Error error ->
      Kernel.SystemError.panic (Kernel.Error.to_string (Kernel.Error.from_time_monotonic error))

let bench_monotonic_compare = fun () ->
  match (Kernel.Time.Monotonic.now (), Kernel.Time.Monotonic.now ()) with
  | (Kernel.Result.Ok left, Kernel.Result.Ok right) ->
      let _ = Kernel.Time.Monotonic.compare left right in
      ()
  | (Kernel.Result.Error error, _)
  | (_, Kernel.Result.Error error) ->
      Kernel.SystemError.panic (Kernel.Error.to_string (Kernel.Error.from_time_monotonic error))

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
  match Kernel.Async.Poll.make () with
  | Kernel.Result.Ok poll ->
      protect
        ~finally:(fun () ->
          let _ = Kernel.Async.Poll.close poll in
          ())
        (fun () -> fn poll)
  | Kernel.Result.Error error -> panic_async error

let bench_timer_after_ns_latency = fun () ->
  with_poll
    (fun poll ->
      match Kernel.Time.Timer.after_ns 1_000_000L with
      | Kernel.Result.Error error ->
          Kernel.SystemError.panic (Kernel.Error.to_string (Kernel.Error.from_time_timer error))
      | Kernel.Result.Ok timer ->
          let source = Kernel.Time.Timer.to_source timer in
          let token = Kernel.Async.Token.make "system-time-bench-timer" in
          let _ = Kernel.Async.Poll.register poll token Kernel.Async.Interest.readable source in
          protect
            ~finally:(fun () ->
              let _ = Kernel.Async.Poll.deregister poll source in
              ())
            (fun () ->
              let _ = Kernel.Async.Poll.poll ~timeout:100_000_000L poll in
              ()))

let bench_timer_many_same_tick_wakeups = fun () ->
  with_poll
    (fun poll ->
      let rec create remaining acc =
        if remaining = 0 then
          List.reverse acc
        else
          match Kernel.Time.Timer.after_ns 1_000_000L with
          | Kernel.Result.Ok timer -> create (remaining - 1) (timer :: acc)
          | Kernel.Result.Error error ->
              Kernel.SystemError.panic (Kernel.Error.to_string (Kernel.Error.from_time_timer error))
      in
      let timers = create 16 [] in
      let sources = List.map timers ~fn:Kernel.Time.Timer.to_source in
      let rec deregister_all = function
        | [] -> ()
        | source :: rest ->
            let _ = Kernel.Async.Poll.deregister poll source in
            deregister_all rest
      in
      protect
        ~finally:(fun () -> deregister_all sources)
        (fun () ->
          let rec register index = function
            | [] -> ()
            | timer :: rest ->
                let _ =
                  Kernel.Async.Poll.register
                    poll
                    (Kernel.Async.Token.make index)
                    Kernel.Async.Interest.readable
                    (Kernel.Time.Timer.to_source timer)
                in
                register (index + 1) rest
          in
          let seen = Kernel.Array.make ~count:16 ~value:false in
          let rec mark = function
            | [] -> ()
            | event :: rest ->
                if Kernel.Async.Event.is_readable event then
                  let token = Kernel.Async.Token.unsafe_value (Kernel.Async.Event.token event) in
                  if token >= 0 && token < 16 then
                    Kernel.Array.set seen ~at:token ~value:true;
                mark rest
          in
          let rec all_seen index =
            if index = 16 then
              true
            else if Kernel.Array.get_unchecked seen ~at:index then
              all_seen (index + 1)
            else
              false
          in
          let rec poll_until attempts =
            if all_seen 0 then
              ()
            else if attempts = 0 then
              Kernel.SystemError.panic "expected many same-tick timers to wake the poller"
            else
              let events =
                lift_async (Kernel.Async.Poll.poll ~timeout:100_000_000L ~max_events:32 poll)
              in
              mark events;
            poll_until (attempts - 1)
          in
          register 0 timers;
          poll_until 8))

let benchmarks =
  Bench.[
    with_config ~config:{ iterations = 100; warmup = 10 } "system_time now" bench_now;
    with_config ~config:{ iterations = 100; warmup = 10 } "system_time compare" bench_compare;
    with_config ~config:{ iterations = 100; warmup = 10 } "monotonic now" bench_monotonic_now;
    with_config
      ~config:{ iterations = 100; warmup = 10 }
      "monotonic compare"
      bench_monotonic_compare;
    with_config
      ~config:{ iterations = 25; warmup = 5 }
      "time timer after_ns latency"
      bench_timer_after_ns_latency;
    with_config
      ~config:{ iterations = 20; warmup = 5 }
      "time timer same-tick wakeups: 16"
      bench_timer_many_same_tick_wakeups;
  ]

let main ~args = Bench.Cli.main ~name:"kernel_new_system_time_bench" ~benchmarks ~args

let () = Runtime.run ~main ~args:Env.args ()
