open Std
module Kernel = Kernel

let build_request = fun ~header_count ~body_len ->
  let header_lines = Kernel.Array.init
    ~count:header_count
    ~fn:(fun index -> "x-header-" ^ Int.to_string index ^ ": value-" ^ Int.to_string index ^ "\r\n")
  |> Kernel.Array.fold_left ~acc:"" ~fn:(fun acc line -> acc ^ line) in
  "POST /benchmark HTTP/1.1\r\n" ^ header_lines ^ "\r\n" ^ String.make ~len:body_len ~char:'x'

let small_slice = Kernel.IO.IoVec.IoSlice.from_string (build_request ~header_count:4 ~body_len:128)
|> Result.unwrap

let medium_slice = Kernel.IO.IoVec.IoSlice.from_string
  (build_request ~header_count:32 ~body_len:4_096)
|> Result.unwrap

let large_slice = Kernel.IO.IoVec.IoSlice.from_string
  (build_request ~header_count:64 ~body_len:65_536)
|> Result.unwrap

let bench_index_of_char = fun slice needle () ->
  let _ = Kernel.IO.IoVec.IoSlice.index_char slice needle in
  ()

let bench_index_of_string = fun slice needle () ->
  let _ = Kernel.IO.IoVec.IoSlice.index_string slice needle in
  ()

let bench_starts_with = fun slice prefix () ->
  let _ = Kernel.IO.IoVec.IoSlice.starts_with slice ~prefix in
  ()

let bench_sub_and_advance = fun slice () ->
  let _ = slice
  |> fun slice ->
    Kernel.IO.IoVec.IoSlice.shift slice 5
    |> Result.unwrap
    |> Kernel.IO.IoVec.IoSlice.sub ~off:0 ~len:32
    |> Result.unwrap in
  ()

let bench_to_string = fun slice () ->
  let _ = Kernel.IO.IoVec.IoSlice.to_string slice in
  ()

let benchmarks =
  Bench.[
    with_config
      ~config:{ iterations = 20; warmup = 5 }
      "io_slice starts_with: small request"
      (bench_starts_with small_slice "POST ");
    with_config
      ~config:{ iterations = 20; warmup = 5 }
      "io_slice index_of_char ' ': medium request"
      (bench_index_of_char medium_slice ' ');
    with_config
      ~config:{ iterations = 20; warmup = 5 }
      "io_slice index_of_string CRLFCRLF: medium request"
      (bench_index_of_string medium_slice "\r\n\r\n");
    with_config
      ~config:{ iterations = 10; warmup = 3 }
      "io_slice sub+advance: large request"
      (bench_sub_and_advance large_slice);
    with_config
      ~config:{ iterations = 10; warmup = 3 }
      "io_slice to_string: medium request"
      (bench_to_string medium_slice);
  ]

let main ~args = Bench.Cli.main ~name:"kernel_new_io_slice_bench" ~benchmarks ~args

let () = Runtime.run ~main ~args:Env.args ()
