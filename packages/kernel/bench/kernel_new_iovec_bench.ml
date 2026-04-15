open Std
module Kernel = Kernel

let build_iovec = fun segments segment_size ->
  Kernel.Array.init ~count:segments ~fn:(fun _ -> Kernel.String.make ~len:segment_size ~char:'x')
  |> Kernel.IO.Iovec.from_string_array

let bench_into_string = fun segments segment_size () ->
  let _ = build_iovec segments segment_size |> Kernel.IO.Iovec.to_string in
  ()

let bench_sub = fun segments segment_size () ->
  let iovec = build_iovec segments segment_size in
  let _ = Kernel.IO.Iovec.sub ~pos:(segment_size / 2) ~len:(segments * segment_size / 2) iovec in
  ()

let benchmarks =
  Bench.[
    with_config
      ~config:{ iterations = 20; warmup = 5 }
      "iovec into_string: 32 x 1KiB"
      (bench_into_string 32 1_024);
    with_config
      ~config:{ iterations = 20; warmup = 5 }
      "iovec into_string: 128 x 1KiB"
      (bench_into_string 128 1_024);
    with_config ~config:{ iterations = 20; warmup = 5 } "iovec sub: 32 x 1KiB" (bench_sub 32 1_024);
  ]

let () =
  Actors.run
    ~main:(fun ~args -> Bench.Cli.main ~name:"kernel_new_iovec_bench" ~benchmarks ~args)
    ~args:Env.args
    ()
