open Std
open Std.Collections

(* Benchmark: Push operations - build entire vector from scratch *)

let bench_push_100 = fun () ->
  let v = Vector.create () in
  for i = 1 to 100 do
    Vector.push v i
  done

let bench_push_10k = fun () ->
  let v = Vector.create () in
  for i = 1 to 10_000 do
    Vector.push v i
  done

let bench_push_100k = fun () ->
  let v = Vector.create () in
  for i = 1 to 100_000 do
    Vector.push v i
  done

let bench_push_1m = fun () ->
  let v = Vector.create () in
  for i = 1 to 1_000_000 do
    Vector.push v i
  done

(* Benchmark: Access operations - single get from different sized vectors *)

let bench_get_from_100 = fun () ->
  let v = Vector.create () in
  for i = 1 to 100 do
    Vector.push v i
  done;
  let _ = Vector.get v 50 in
  ()

let bench_get_from_10k = fun () ->
  let v = Vector.create () in
  for i = 1 to 10_000 do
    Vector.push v i
  done;
  let _ = Vector.get v 5_000 in
  ()

let bench_get_from_100k = fun () ->
  let v = Vector.create () in
  for i = 1 to 100_000 do
    Vector.push v i
  done;
  let _ = Vector.get v 50_000 in
  ()

let bench_get_from_1m = fun () ->
  let v = Vector.create () in
  for i = 1 to 1_000_000 do
    Vector.push v i
  done;
  let _ = Vector.get v 500_000 in
  ()

(* Benchmark: Pop operations - pop from end *)

let bench_pop_from_100 = fun () ->
  let v = Vector.create () in
  for i = 1 to 100 do
    Vector.push v i
  done;
  let _ = Vector.pop v in
  ()

let bench_pop_from_10k = fun () ->
  let v = Vector.create () in
  for i = 1 to 10_000 do
    Vector.push v i
  done;
  let _ = Vector.pop v in
  ()

let bench_pop_from_100k = fun () ->
  let v = Vector.create () in
  for i = 1 to 100_000 do
    Vector.push v i
  done;
  let _ = Vector.pop v in
  ()

(* Benchmark: Iteration - iterate over entire vector *)

let bench_iter_100 = fun () ->
  let v = Vector.create () in
  for i = 1 to 100 do
    Vector.push v i
  done;
  Vector.iter (fun _x -> ()) v

let bench_iter_10k = fun () ->
  let v = Vector.create () in
  for i = 1 to 10_000 do
    Vector.push v i
  done;
  Vector.iter (fun _x -> ()) v

let bench_iter_100k = fun () ->
  let v = Vector.create () in
  for i = 1 to 100_000 do
    Vector.push v i
  done;
  Vector.iter (fun _x -> ()) v

(* Benchmark: Sort operations *)

let bench_sort_100 = fun () ->
  let v = Vector.create () in
  (* Fill with reverse order *)
  for i = 100 downto 1 do
    Vector.push v i
  done;
  Vector.sort v

let bench_sort_10k = fun () ->
  let v = Vector.create () in
  for i = 10_000 downto 1 do
    Vector.push v i
  done;
  Vector.sort v

let bench_sort_100k = fun () ->
  let v = Vector.create () in
  for i = 100_000 downto 1 do
    Vector.push v i
  done;
  Vector.sort v

let benchmarks =
  Bench.[
    case "push: 100 items" bench_push_100;
    with_config ~config:{ iterations = 10; warmup = 2 } "push: 10k items" bench_push_10k;
    with_config ~config:{ iterations = 5; warmup = 1 } "push: 100k items" bench_push_100k;
    with_config ~config:{ iterations = 3; warmup = 1 } "push: 1M items" bench_push_1m;
    case "get: from 100 items" bench_get_from_100;
    with_config ~config:{ iterations = 50; warmup = 5 } "get: from 10k items" bench_get_from_10k;
    with_config ~config:{ iterations = 20; warmup = 2 } "get: from 100k items" bench_get_from_100k;
    with_config ~config:{ iterations = 10; warmup = 2 } "get: from 1M items" bench_get_from_1m;
    case "pop: from 100 items" bench_pop_from_100;
    with_config ~config:{ iterations = 50; warmup = 5 } "pop: from 10k items" bench_pop_from_10k;
    with_config ~config:{ iterations = 20; warmup = 2 } "pop: from 100k items" bench_pop_from_100k;
    case "iter: 100 items" bench_iter_100;
    with_config ~config:{ iterations = 10; warmup = 2 } "iter: 10k items" bench_iter_10k;
    with_config ~config:{ iterations = 5; warmup = 1 } "iter: 100k items" bench_iter_100k;
    case "sort: 100 items" bench_sort_100;
    with_config ~config:{ iterations = 10; warmup = 2 } "sort: 10k items" bench_sort_10k;
    with_config ~config:{ iterations = 5; warmup = 1 } "sort: 100k items" bench_sort_100k;
  ]

let () =
  Miniriot.run
    ~main:(fun ~args -> Bench.Cli.main ~name:"Vector Benchmarks" ~benchmarks ~args)
    ~args:Env.args
    ()
