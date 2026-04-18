open Std

module Kernel = Kernel

let small_chunk = String.make ~len:16 ~char:'x'
let medium_chunk = String.make ~len:1_024 ~char:'y'

let medium_slice =
  let slice = Kernel.IO.Iovec.IoSlice.create ~size:(String.length medium_chunk) in
  Kernel.IO.Iovec.IoSlice.blit_from_string medium_chunk ~src_offset:0 ~dst:slice ~dst_offset:0 ~len:(String.length medium_chunk);
  slice

let bench_append_string = fun ~count ~chunk () ->
  let buffer = Kernel.IO.Buffer.create () in
  for _ = 1 to count do
    Kernel.IO.Buffer.append_string buffer chunk
  done

let bench_append_slice = fun ~count slice () ->
  let buffer = Kernel.IO.Buffer.create () in
  for _ = 1 to count do
    Kernel.IO.Buffer.append_slice buffer slice
  done

let bench_direct_write_commit = fun ~count ~chunk () ->
  let chunk_len = String.length chunk in
  let buffer = Kernel.IO.Buffer.create () in
  for _ = 1 to count do
    let writable = Kernel.IO.Buffer.writable_slice ~size:chunk_len buffer in
    Kernel.IO.Iovec.IoSlice.blit_from_string chunk ~src_offset:0 ~dst:writable ~dst_offset:0 ~len:chunk_len;
    Kernel.IO.Buffer.commit_write buffer ~len:chunk_len
  done

let bench_consume_and_refill = fun ~count ~chunk () ->
  let chunk_len = String.length chunk in
  let buffer = Kernel.IO.Buffer.create ~size:(chunk_len * 2) () in
  for _ = 1 to count do
    Kernel.IO.Buffer.append_string buffer chunk;
    if Kernel.IO.Buffer.length buffer >= chunk_len * 2 then
      Kernel.IO.Buffer.consume buffer ~len:chunk_len
  done

let benchmarks =
  Bench.[
    with_config
      ~config:{ iterations = 20; warmup = 5 }
      "io buffer append_string: 4096 x 16B"
      (bench_append_string ~count:4_096 ~chunk:small_chunk);
    with_config
      ~config:{ iterations = 10; warmup = 3 }
      "io buffer append_slice: 128 x 1KiB"
      (bench_append_slice ~count:128 medium_slice);
    with_config
      ~config:{ iterations = 10; warmup = 3 }
      "io buffer direct write+commit: 128 x 1KiB"
      (bench_direct_write_commit ~count:128 ~chunk:medium_chunk);
    with_config
      ~config:{ iterations = 20; warmup = 5 }
      "io buffer consume+refill: 4096 x 16B"
      (bench_consume_and_refill ~count:4_096 ~chunk:small_chunk);
  ]

let () =
  Actors.run
    ~main:(fun ~args -> Bench.Cli.main ~name:"kernel_new_io_buffer_bench" ~benchmarks ~args)
    ~args:Env.args
    ()
