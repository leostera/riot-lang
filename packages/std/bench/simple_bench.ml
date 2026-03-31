open Std

(* Simple benchmark functions *)

let bench_addition = fun () ->
  let _ = 1 + 1 in
  ()

let bench_string_concat = fun () ->
  let _ = "hello" ^ " " ^ "world" in
  ()

let bench_list_creation = fun () ->
  let _ = [ 1; 2; 3; 4; 5 ] in
  ()

(* Benchmark suite *)

let benchmarks =
  Bench.[
    case "simple addition" bench_addition;
    case "string concatenation" bench_string_concat;
    with_config ~config:{iterations = 200; warmup = 20} "list creation" bench_list_creation;

  ]

(* Main entry point using new Bench.Cli *)

let () =
  Miniriot.run
  ~main:(fun ~args -> Bench.Cli.main ~name:"Simple Benchmarks" ~benchmarks ~args)
  ~args:Env.args
  ()
