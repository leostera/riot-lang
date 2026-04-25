open Std
open Std.Collections
module Bytes = IO.Bytes

let build_strings = fun ~count ~segment_size ->
  Array.init ~count ~fn:(fun _ -> String.make ~len:segment_size ~char:'x')

let build_bytes = fun ~count ~segment_size ->
  build_strings ~count ~segment_size |> Array.map ~fn:Bytes.from_string

let bench_from_string_array = fun payload () ->
  let _ = IO.IoVec.from_string_array payload |> Result.unwrap in
  ()

let bench_from_bytes_array = fun payload () ->
  let _ = IO.IoVec.from_bytes_array payload |> Result.unwrap in
  ()

let bench_to_string = fun iovec () ->
  let _ = IO.IoVec.to_string iovec in
  ()

let bench_to_bytes = fun iovec () ->
  let _ = IO.IoVec.to_bytes iovec in
  ()

let bench_sub_middle = fun ~segment_size iovec () ->
  let total = IO.IoVec.length iovec in
  let _ = IO.IoVec.sub ~pos:(segment_size / 2) ~len:(total / 2) iovec |> Result.unwrap in
  ()

let bench_for_each = fun iovec () -> IO.IoVec.for_each iovec ~fn:(fun _ -> ())

let small_string_segments = build_strings ~count:4_096 ~segment_size:16

let small_bytes_segments = build_bytes ~count:4_096 ~segment_size:16

let small_iovec = IO.IoVec.from_string_array small_string_segments |> Result.unwrap

let medium_string_segments = build_strings ~count:128 ~segment_size:1_024

let medium_bytes_segments = build_bytes ~count:128 ~segment_size:1_024

let medium_iovec = IO.IoVec.from_string_array medium_string_segments |> Result.unwrap

let large_string_segments = build_strings ~count:32 ~segment_size:4_096

let large_bytes_segments = build_bytes ~count:32 ~segment_size:4_096

let large_iovec = IO.IoVec.from_string_array large_string_segments |> Result.unwrap

let benchmarks =
  Bench.[
    with_config
      ~config:{ iterations = 20; warmup = 5 }
      "std io iovec from_string_array: 4096 x 16B"
      (bench_from_string_array small_string_segments);
    with_config
      ~config:{ iterations = 10; warmup = 3 }
      "std io iovec from_string_array: 128 x 1KiB"
      (bench_from_string_array medium_string_segments);
    with_config
      ~config:{ iterations = 10; warmup = 3 }
      "std io iovec from_string_array: 32 x 4KiB"
      (bench_from_string_array large_string_segments);
    with_config
      ~config:{ iterations = 20; warmup = 5 }
      "std io iovec from_bytes_array: 4096 x 16B"
      (bench_from_bytes_array small_bytes_segments);
    with_config
      ~config:{ iterations = 10; warmup = 3 }
      "std io iovec from_bytes_array: 128 x 1KiB"
      (bench_from_bytes_array medium_bytes_segments);
    with_config
      ~config:{ iterations = 10; warmup = 3 }
      "std io iovec from_bytes_array: 32 x 4KiB"
      (bench_from_bytes_array large_bytes_segments);
    with_config
      ~config:{ iterations = 20; warmup = 5 }
      "std io iovec to_string: 4096 x 16B"
      (bench_to_string small_iovec);
    with_config
      ~config:{ iterations = 10; warmup = 3 }
      "std io iovec to_string: 128 x 1KiB"
      (bench_to_string medium_iovec);
    with_config
      ~config:{ iterations = 10; warmup = 3 }
      "std io iovec to_bytes: 4096 x 16B"
      (bench_to_bytes small_iovec);
    with_config
      ~config:{ iterations = 10; warmup = 3 }
      "std io iovec to_bytes: 128 x 1KiB"
      (bench_to_bytes medium_iovec);
    with_config
      ~config:{ iterations = 20; warmup = 5 }
      "std io iovec sub middle: 4096 x 16B"
      (bench_sub_middle ~segment_size:16 small_iovec);
    with_config
      ~config:{ iterations = 10; warmup = 3 }
      "std io iovec sub middle: 128 x 1KiB"
      (bench_sub_middle ~segment_size:1_024 medium_iovec);
    with_config
      ~config:{ iterations = 5; warmup = 2 }
      "std io iovec sub middle: 32 x 4KiB"
      (bench_sub_middle ~segment_size:4_096 large_iovec);
    with_config
      ~config:{ iterations = 20; warmup = 5 }
      "std io iovec for_each: 4096 x 16B"
      (bench_for_each small_iovec);
    with_config
      ~config:{ iterations = 10; warmup = 3 }
      "std io iovec for_each: 128 x 1KiB"
      (bench_for_each medium_iovec);
  ]

let main ~args = Bench.Cli.main ~name:"std_io_iovec_bench" ~benchmarks ~args

let () = Runtime.run ~main ~args:Env.args ()
