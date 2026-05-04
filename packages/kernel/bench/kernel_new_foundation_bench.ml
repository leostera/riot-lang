open Std

module Kernel = Kernel

let string_payload = Kernel.String.make ~len:4_096 ~char:'x'

let bytes_payload = Kernel.Bytes.from_string string_payload

let bench_bytes_of_string = fun () ->
  let _ = Kernel.Bytes.from_string string_payload in
  ()

let bench_bytes_to_string = fun () ->
  let _ = Kernel.Bytes.to_string bytes_payload in
  ()

let bench_string_to_bytes = fun () ->
  let _ = Kernel.String.to_bytes string_payload in
  ()

let bench_string_of_bytes = fun () ->
  let _ = Kernel.String.from_bytes bytes_payload in
  ()

let benchmarks =
  Bench.[
    with_config
      ~config:{ iterations = 50; warmup = 10 }
      "foundation bytes from_string: 4KiB"
      bench_bytes_of_string;
    with_config
      ~config:{ iterations = 50; warmup = 10 }
      "foundation bytes to_string: 4KiB"
      bench_bytes_to_string;
    with_config
      ~config:{ iterations = 50; warmup = 10 }
      "foundation string to_bytes: 4KiB"
      bench_string_to_bytes;
    with_config
      ~config:{ iterations = 50; warmup = 10 }
      "foundation string from_bytes: 4KiB"
      bench_string_of_bytes;
  ]

let main ~args = Bench.Cli.main ~name:"kernel_new_foundation_bench" ~benchmarks ~args

let () = Runtime.run ~main ~args:Env.args ()
