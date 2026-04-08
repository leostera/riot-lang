open Std
module Kernel = Kernel_new

let bench_now = fun () ->
  match Kernel.Time.SystemTime.now () with
  | Kernel.Result.Ok _ -> ()
  | Kernel.Result.Error error -> Kernel.Error.panic (Kernel.Error.to_string error)

let bench_compare = fun () ->
  match Kernel.Time.SystemTime.now (), Kernel.Time.SystemTime.now () with
  | Kernel.Result.Ok left, Kernel.Result.Ok right ->
      let _ = Kernel.Time.SystemTime.compare left right in
      ()
  | (Kernel.Result.Error error, _)
  | (_, Kernel.Result.Error error) -> Kernel.Error.panic (Kernel.Error.to_string error)

let benchmarks =
  Bench.[
    with_config ~config:{ iterations = 100; warmup = 10 } "system_time now" bench_now;
    with_config ~config:{ iterations = 100; warmup = 10 } "system_time compare" bench_compare;
  ]

let () =
  Actors.run
    ~main:(fun ~args -> Bench.Cli.main ~name:"kernel_new_system_time_bench" ~benchmarks ~args)
    ~args:Env.args
    ()
