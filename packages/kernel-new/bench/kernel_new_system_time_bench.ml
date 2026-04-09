open Std
module Kernel = Kernel_new

let bench_now = fun () ->
  match Kernel.Time.SystemTime.now () with
  | Kernel.Result.Ok _ -> ()
  | Kernel.Result.Error error -> Kernel.Error.panic
    (Kernel.Error.to_string (Kernel.Error.of_time_system_time error))

let bench_compare = fun () ->
  match Kernel.Time.SystemTime.now (), Kernel.Time.SystemTime.now () with
  | Kernel.Result.Ok left, Kernel.Result.Ok right ->
      let _ = Kernel.Time.SystemTime.compare left right in
      ()
  | (Kernel.Result.Error error, _)
  | (_, Kernel.Result.Error error) -> Kernel.Error.panic
    (Kernel.Error.to_string (Kernel.Error.of_time_system_time error))

let bench_monotonic_now = fun () ->
  match Kernel.Time.Monotonic.now () with
  | Kernel.Result.Ok _ -> ()
  | Kernel.Result.Error error -> Kernel.Error.panic
    (Kernel.Error.to_string (Kernel.Error.of_time_monotonic error))

let bench_monotonic_compare = fun () ->
  match Kernel.Time.Monotonic.now (), Kernel.Time.Monotonic.now () with
  | Kernel.Result.Ok left, Kernel.Result.Ok right ->
      let _ = Kernel.Time.Monotonic.compare left right in
      ()
  | (Kernel.Result.Error error, _)
  | (_, Kernel.Result.Error error) -> Kernel.Error.panic
    (Kernel.Error.to_string (Kernel.Error.of_time_monotonic error))

let benchmarks =
  Bench.[
    with_config ~config:{ iterations = 100; warmup = 10 } "system_time now" bench_now;
    with_config ~config:{ iterations = 100; warmup = 10 } "system_time compare" bench_compare;
    with_config ~config:{ iterations = 100; warmup = 10 } "monotonic now" bench_monotonic_now;
    with_config ~config:{ iterations = 100; warmup = 10 } "monotonic compare" bench_monotonic_compare;
  ]

let () =
  Actors.run
    ~main:(fun ~args -> Bench.Cli.main ~name:"kernel_new_system_time_bench" ~benchmarks ~args)
    ~args:Env.args
    ()
