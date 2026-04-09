open Std
module Kernel = Kernel_new

let lift = function
  | Kernel.Result.Ok value -> value
  | Kernel.Result.Error error -> Kernel.Error.panic (Kernel.Env.error_to_string error)

let env_name = "RIOT_KERNEL_NEW_ENV_BENCH"

let () =
  let _ = Kernel.Env.set_var ~name:env_name ~value:"kernel-new" in
  ()

let bench_get_var = fun () ->
  ignore (Kernel.Env.get env_name)

let bench_current_dir = fun () ->
  ignore (lift (Kernel.Env.current_dir ()))

let bench_vars = fun () ->
  ignore (Kernel.Env.vars ())

let benchmarks =
  Bench.[
    with_config
      ~config:{ iterations = 100; warmup = 10 }
      "env get existing var"
      bench_get_var;
    with_config
      ~config:{ iterations = 100; warmup = 10 }
      "env current_dir"
      bench_current_dir;
    with_config
      ~config:{ iterations = 50; warmup = 10 }
      "env vars snapshot"
      bench_vars;
  ]

let () =
  Actors.run
    ~main:(fun ~args ->
      Bench.Cli.main ~name:"kernel_new_env_bench" ~benchmarks ~args)
    ~args:Env.args
    ()
