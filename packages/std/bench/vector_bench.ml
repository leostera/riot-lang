open Std
open Std.Collections

let fill_vector = fun vector ~count ~start ->
  let rec loop index =
    if index >= count then
      ()
    else (
      Vector.push vector ~value:(start + index);
      loop (index + 1)
    )
  in
  loop 0

let build_vector = fun count ->
  let vector = Vector.create () in
  fill_vector vector ~count ~start:0;
  vector

let build_vector_with_capacity = fun capacity count ->
  let vector = Vector.with_capacity ~size:capacity in
  fill_vector vector ~count ~start:0;
  vector

let bench_push_growing = fun count () ->
  let _ = build_vector count in
  ()

let bench_push_preallocated = fun count () ->
  let _ = build_vector_with_capacity count count in
  ()

let bench_get_middle = fun count () ->
  let vector = build_vector count in
  let _ = Vector.get vector ~at:(count / 2) in
  ()

let bench_iter = fun count () ->
  let vector = build_vector count in
  Vector.for_each vector ~fn:(fun _ -> ())

let bench_append_growing_dst = fun count () ->
  let left = build_vector count in
  let right = build_vector count in
  Vector.append left right

let bench_append_preallocated_dst = fun count () ->
  let left = build_vector_with_capacity (count * 2) count in
  let right = build_vector_with_capacity count count in
  Vector.append left right

let benchmarks =
  Bench.[
    with_config
      ~config:{ iterations = 10; warmup = 2 }
      "push growing: 10k items"
      (bench_push_growing 10_000);
    with_config
      ~config:{ iterations = 10; warmup = 2 }
      "push preallocated: 10k items"
      (bench_push_preallocated 10_000);
    with_config
      ~config:{ iterations = 5; warmup = 1 }
      "push growing: 100k items"
      (bench_push_growing 100_000);
    with_config
      ~config:{ iterations = 5; warmup = 1 }
      "push preallocated: 100k items"
      (bench_push_preallocated 100_000);
    with_config
      ~config:{ iterations = 3; warmup = 1 }
      "push growing: 1M items"
      (bench_push_growing 1_000_000);
    with_config
      ~config:{ iterations = 3; warmup = 1 }
      "push preallocated: 1M items"
      (bench_push_preallocated 1_000_000);
    with_config
      ~config:{ iterations = 50; warmup = 5 }
      "get middle: 10k items"
      (bench_get_middle 10_000);
    with_config
      ~config:{ iterations = 20; warmup = 2 }
      "get middle: 100k items"
      (bench_get_middle 100_000);
    with_config ~config:{ iterations = 10; warmup = 2 } "iter: 100k items" (bench_iter 100_000);
    with_config
      ~config:{ iterations = 10; warmup = 2 }
      "append growing dst: 10k + 10k"
      (bench_append_growing_dst 10_000);
    with_config
      ~config:{ iterations = 10; warmup = 2 }
      "append preallocated dst: 10k + 10k"
      (bench_append_preallocated_dst 10_000);
    with_config
      ~config:{ iterations = 5; warmup = 1 }
      "append growing dst: 100k + 100k"
      (bench_append_growing_dst 100_000);
    with_config
      ~config:{ iterations = 5; warmup = 1 }
      "append preallocated dst: 100k + 100k"
      (bench_append_preallocated_dst 100_000);
  ]

let () =
  Runtime.run
    ~main:(fun ~args -> Bench.Cli.main ~name:"Vector Benchmarks" ~benchmarks ~args)
    ~args:Env.args
    ()
