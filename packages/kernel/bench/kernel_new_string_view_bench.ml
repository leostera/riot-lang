open Std

module Kernel = Kernel

let build_request = fun ~header_count ~body_len ->
  let header_lines =
    Kernel.Array.init
      ~count:header_count
      ~fn:(fun index -> "x-header-" ^ Int.to_string index ^ ": value-" ^ Int.to_string index ^ "\r\n")
    |> Kernel.Array.fold_left ~acc:"" ~fn:(fun acc line -> acc ^ line)
  in
  "POST /benchmark HTTP/1.1\r\n"
  ^ header_lines
  ^ "\r\n"
  ^ String.make ~len:body_len ~char:'x'

let small_view = Kernel.IO.StringView.of_string (build_request ~header_count:4 ~body_len:128)
let medium_view = Kernel.IO.StringView.of_string (build_request ~header_count:32 ~body_len:4_096)
let large_view = Kernel.IO.StringView.of_string (build_request ~header_count:64 ~body_len:65_536)

let bench_index_of_char = fun view needle () ->
  let _ = Kernel.IO.StringView.index_of_char view needle in
  ()

let bench_index_of_string = fun view needle () ->
  let _ = Kernel.IO.StringView.index_of_string view needle in
  ()

let bench_starts_with = fun view prefix () ->
  let _ = Kernel.IO.StringView.starts_with view ~prefix in
  ()

let bench_sub_and_advance = fun view () ->
  let _ =
    view
    |> Kernel.IO.StringView.advance ~by:5
    |> Kernel.IO.StringView.sub ~offset:0 ~len:32
  in
  ()

let bench_to_string = fun view () ->
  let _ = Kernel.IO.StringView.to_string view in
  ()

let benchmarks =
  Bench.[
    with_config
      ~config:{ iterations = 20; warmup = 5 }
      "string_view starts_with: small request"
      (bench_starts_with small_view "POST ");
    with_config
      ~config:{ iterations = 20; warmup = 5 }
      "string_view index_of_char ' ': medium request"
      (bench_index_of_char medium_view ' ');
    with_config
      ~config:{ iterations = 20; warmup = 5 }
      "string_view index_of_string CRLFCRLF: medium request"
      (bench_index_of_string medium_view "\r\n\r\n");
    with_config
      ~config:{ iterations = 10; warmup = 3 }
      "string_view sub+advance: large request"
      (bench_sub_and_advance large_view);
    with_config
      ~config:{ iterations = 10; warmup = 3 }
      "string_view to_string: medium request"
      (bench_to_string medium_view);
  ]

let () =
  Actors.run
    ~main:(fun ~args -> Bench.Cli.main ~name:"kernel_new_string_view_bench" ~benchmarks ~args)
    ~args:Env.args
    ()
