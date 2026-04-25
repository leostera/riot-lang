open Std
module Kernel = Kernel

let lift result =
  match result with
  | Kernel.Result.Ok value -> value
  | Kernel.Result.Error error -> Kernel.SystemError.panic (Kernel.Env.error_to_string error)

let env_name = "RIOT_KERNEL_NEW_ENV_BENCH"

let () =
  let _ = Kernel.Env.set ~var:env_name ~value:"kernel-new" in
  ()

let bench_get_var = fun () ->
  let _ = Kernel.Env.get ~var:env_name in
  ()

let bench_current_dir = fun () ->
  let _ = lift (Kernel.Env.current_dir ()) in
  ()

let bench_vars = fun () ->
  let _ = Kernel.Env.vars () in
  ()

let benchmarks =
  Bench.[
    with_config ~config:{ iterations = 100; warmup = 10 } "env get existing var" bench_get_var;
    with_config ~config:{ iterations = 100; warmup = 10 } "env current_dir" bench_current_dir;
    with_config ~config:{ iterations = 50; warmup = 10 } "env vars snapshot" bench_vars;
  ]

let main ~args = Bench.Cli.main ~name:"kernel_new_env_bench" ~benchmarks ~args

let () = Runtime.run ~main ~args:Env.args ()
