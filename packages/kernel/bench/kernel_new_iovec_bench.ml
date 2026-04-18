open Std
module Kernel = Kernel

let build_strings = fun ~count ~segment_size ->
  Kernel.Array.init ~count ~fn:(fun _ -> Kernel.String.make ~len:segment_size ~char:'x')

let build_bytes = fun ~count ~segment_size ->
  build_strings ~count ~segment_size |> Kernel.Array.map ~fn:Kernel.Bytes.from_string

let build_iovec = fun ~count ~segment_size ->
  build_strings ~count ~segment_size |> Kernel.IO.Iovec.from_string_array

let bench_from_string_array = fun payload () ->
  let _ = Kernel.IO.Iovec.from_string_array payload in
  ()

let bench_from_bytes_array = fun payload () ->
  let _ = Kernel.IO.Iovec.from_bytes_array payload in
  ()

let bench_to_string = fun iovec () ->
  let _ = Kernel.IO.Iovec.to_string iovec in
  ()

let bench_to_bytes = fun iovec () ->
  let _ = Kernel.IO.Iovec.to_bytes iovec in
  ()

let bench_sub_middle = fun ~segment_size iovec () ->
  let total = Kernel.IO.Iovec.length iovec in
  let _ = Kernel.IO.Iovec.sub ~pos:(segment_size / 2) ~len:(total / 2) iovec in
  ()

let bench_for_each = fun iovec () ->
  Kernel.IO.Iovec.for_each iovec ~fn:(fun _ -> ())

let small_string_segments = build_strings ~count:4_096 ~segment_size:16
let small_bytes_segments = build_bytes ~count:4_096 ~segment_size:16
let small_iovec = Kernel.IO.Iovec.from_string_array small_string_segments

let medium_string_segments = build_strings ~count:128 ~segment_size:1_024
let medium_bytes_segments = build_bytes ~count:128 ~segment_size:1_024
let medium_iovec = Kernel.IO.Iovec.from_string_array medium_string_segments

let large_string_segments = build_strings ~count:32 ~segment_size:4_096
let large_bytes_segments = build_bytes ~count:32 ~segment_size:4_096
let large_iovec = Kernel.IO.Iovec.from_string_array large_string_segments

let benchmarks =
  Bench.[
    with_config
      ~config:{ iterations = 20; warmup = 5 }
      "iovec from_string_array: 4096 x 16B"
      (bench_from_string_array small_string_segments);
    with_config
      ~config:{ iterations = 10; warmup = 3 }
      "iovec from_string_array: 128 x 1KiB"
      (bench_from_string_array medium_string_segments);
    with_config
      ~config:{ iterations = 10; warmup = 3 }
      "iovec from_string_array: 32 x 4KiB"
      (bench_from_string_array large_string_segments);
    with_config
      ~config:{ iterations = 20; warmup = 5 }
      "iovec from_bytes_array: 4096 x 16B"
      (bench_from_bytes_array small_bytes_segments);
    with_config
      ~config:{ iterations = 10; warmup = 3 }
      "iovec from_bytes_array: 128 x 1KiB"
      (bench_from_bytes_array medium_bytes_segments);
    with_config
      ~config:{ iterations = 10; warmup = 3 }
      "iovec from_bytes_array: 32 x 4KiB"
      (bench_from_bytes_array large_bytes_segments);
    with_config ~config:{ iterations = 20; warmup = 5 } "iovec to_string: 4096 x 16B" (bench_to_string small_iovec);
    with_config ~config:{ iterations = 10; warmup = 3 } "iovec to_string: 128 x 1KiB" (bench_to_string medium_iovec);
    with_config ~config:{ iterations = 10; warmup = 3 } "iovec to_bytes: 4096 x 16B" (bench_to_bytes small_iovec);
    with_config ~config:{ iterations = 10; warmup = 3 } "iovec to_bytes: 128 x 1KiB" (bench_to_bytes medium_iovec);
    with_config
      ~config:{ iterations = 20; warmup = 5 }
      "iovec sub middle: 4096 x 16B"
      (bench_sub_middle ~segment_size:16 small_iovec);
    with_config
      ~config:{ iterations = 10; warmup = 3 }
      "iovec sub middle: 128 x 1KiB"
      (bench_sub_middle ~segment_size:1_024 medium_iovec);
    with_config
      ~config:{ iterations = 5; warmup = 2 }
      "iovec sub middle: 32 x 4KiB"
      (bench_sub_middle ~segment_size:4_096 large_iovec);
    with_config ~config:{ iterations = 20; warmup = 5 } "iovec for_each: 4096 x 16B" (bench_for_each small_iovec);
    with_config ~config:{ iterations = 10; warmup = 3 } "iovec for_each: 128 x 1KiB" (bench_for_each medium_iovec);
  ]

let () =
  Actors.run
    ~main:(fun ~args -> Bench.Cli.main ~name:"kernel_new_iovec_bench" ~benchmarks ~args)
    ~args:Env.args
    ()
