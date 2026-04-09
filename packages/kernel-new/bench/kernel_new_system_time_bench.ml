open Std
module Kernel = Kernel_new

let panic_async = fun error ->
  Kernel.SystemError.panic (Kernel.Error.to_string (Kernel.Error.of_async error))

let bench_now = fun () ->
  match Kernel.Time.SystemTime.now () with
  | Kernel.Result.Ok _ -> ()
  | Kernel.Result.Error error -> Kernel.SystemError.panic
    (Kernel.Error.to_string (Kernel.Error.of_time_system_time error))

let bench_compare = fun () ->
  match Kernel.Time.SystemTime.now (), Kernel.Time.SystemTime.now () with
  | Kernel.Result.Ok left, Kernel.Result.Ok right ->
      let _ = Kernel.Time.SystemTime.compare left right in
      ()
  | (Kernel.Result.Error error, _)
  | (_, Kernel.Result.Error error) -> Kernel.SystemError.panic
    (Kernel.Error.to_string (Kernel.Error.of_time_system_time error))

let bench_monotonic_now = fun () ->
  match Kernel.Time.Monotonic.now () with
  | Kernel.Result.Ok _ -> ()
  | Kernel.Result.Error error -> Kernel.SystemError.panic
    (Kernel.Error.to_string (Kernel.Error.of_time_monotonic error))

let bench_monotonic_compare = fun () ->
  match Kernel.Time.Monotonic.now (), Kernel.Time.Monotonic.now () with
  | Kernel.Result.Ok left, Kernel.Result.Ok right ->
      let _ = Kernel.Time.Monotonic.compare left right in
      ()
  | (Kernel.Result.Error error, _)
  | (_, Kernel.Result.Error error) -> Kernel.SystemError.panic
    (Kernel.Error.to_string (Kernel.Error.of_time_monotonic error))

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
      | Kernel.Result.Error error -> Kernel.SystemError.panic
        (Kernel.Error.to_string (Kernel.Error.of_time_timer error))
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

let benchmarks =
  Bench.[
    with_config ~config:{ iterations = 100; warmup = 10 } "system_time now" bench_now;
    with_config ~config:{ iterations = 100; warmup = 10 } "system_time compare" bench_compare;
    with_config ~config:{ iterations = 100; warmup = 10 } "monotonic now" bench_monotonic_now;
    with_config ~config:{ iterations = 100; warmup = 10 } "monotonic compare" bench_monotonic_compare;
    with_config ~config:{ iterations = 25; warmup = 5 } "time timer after_ns latency" bench_timer_after_ns_latency;
  ]

let () =
  Actors.run
    ~main:(fun ~args -> Bench.Cli.main ~name:"kernel_new_system_time_bench" ~benchmarks ~args)
    ~args:Env.args
    ()
