open Std

module Kernel = Kernel

let panic_async = fun error -> Kernel.SystemError.panic (Kernel.Async.error_to_string error)

let with_poll = fun fn ->
  match Kernel.Async.Poll.make () with
  | Kernel.Result.Error error -> panic_async error
  | Kernel.Result.Ok poll ->
      try
        let result = fn poll in
        let _ = Kernel.Async.Poll.close poll in
        result
      with
      | error ->
          let _ = Kernel.Async.Poll.close poll in
          raise error

let bench_stdin_read_len_zero = fun () ->
  let buffer = Kernel.Bytes.create ~size:16 in
  match Kernel.IO.Stdin.read ~len:0 buffer with
  | Kernel.Result.Ok count ->
      if count != 0 then
        Kernel.SystemError.panic "unexpected stdin read count"
      else
        ()
  | Kernel.Result.Error error -> Kernel.SystemError.panic (Kernel.IO.Stdin.error_to_string error)

let bench_stdout_write_len_zero = fun () ->
  let buffer = Kernel.Bytes.create ~size:16 in
  match Kernel.IO.Stdout.write ~len:0 buffer with
  | Kernel.Result.Ok count ->
      if count != 0 then
        Kernel.SystemError.panic "unexpected stdout write count"
      else
        ()
  | Kernel.Result.Error error -> Kernel.SystemError.panic (Kernel.IO.Stdout.error_to_string error)

let bench_stderr_write_len_zero = fun () ->
  let buffer = Kernel.Bytes.create ~size:16 in
  match Kernel.IO.Stderr.write ~len:0 buffer with
  | Kernel.Result.Ok count ->
      if count != 0 then
        Kernel.SystemError.panic "unexpected stderr write count"
      else
        ()
  | Kernel.Result.Error error -> Kernel.SystemError.panic (Kernel.IO.Stderr.error_to_string error)

let bench_stdin_read_vectored_len_zero = fun () ->
  let iovec =
    Kernel.IO.IoVec.from_bytes_array [|Kernel.Bytes.create ~size:0|]
    |> Result.unwrap
  in
  match Kernel.IO.Stdin.read_vectored iovec with
  | Kernel.Result.Ok count ->
      if count != 0 then
        Kernel.SystemError.panic "unexpected stdin readv count"
      else
        ()
  | Kernel.Result.Error error -> Kernel.SystemError.panic (Kernel.IO.Stdin.error_to_string error)

let bench_stdout_write_vectored_len_zero = fun () ->
  let iovec =
    Kernel.IO.IoVec.from_bytes_array [|Kernel.Bytes.create ~size:0|]
    |> Result.unwrap
  in
  match Kernel.IO.Stdout.write_vectored iovec with
  | Kernel.Result.Ok count ->
      if count != 0 then
        Kernel.SystemError.panic "unexpected stdout writev count"
      else
        ()
  | Kernel.Result.Error error -> Kernel.SystemError.panic (Kernel.IO.Stdout.error_to_string error)

let bench_stderr_write_vectored_len_zero = fun () ->
  let iovec =
    Kernel.IO.IoVec.from_bytes_array [|Kernel.Bytes.create ~size:0|]
    |> Result.unwrap
  in
  match Kernel.IO.Stderr.write_vectored iovec with
  | Kernel.Result.Ok count ->
      if count != 0 then
        Kernel.SystemError.panic "unexpected stderr writev count"
      else
        ()
  | Kernel.Result.Error error -> Kernel.SystemError.panic (Kernel.IO.Stderr.error_to_string error)

let bench_stdin_source_poll = fun () ->
  with_poll
    (fun poll ->
      let source = Kernel.IO.Stdin.to_source () in
      let _ =
        Kernel.Async.Poll.register
          poll
          (Kernel.Async.Token.make "stdin")
          Kernel.Async.Interest.readable
          source
      in
      let _ = Kernel.Async.Poll.poll ~timeout:0L poll in
      let _ = Kernel.Async.Poll.deregister poll source in
      ())

let bench_stdout_source_poll = fun () ->
  with_poll
    (fun poll ->
      let source = Kernel.IO.Stdout.to_source () in
      let _ =
        Kernel.Async.Poll.register
          poll
          (Kernel.Async.Token.make "stdout")
          Kernel.Async.Interest.writable
          source
      in
      let _ = Kernel.Async.Poll.poll ~timeout:0L poll in
      let _ = Kernel.Async.Poll.deregister poll source in
      ())

let bench_stderr_source_poll = fun () ->
  with_poll
    (fun poll ->
      let source = Kernel.IO.Stderr.to_source () in
      let _ =
        Kernel.Async.Poll.register
          poll
          (Kernel.Async.Token.make "stderr")
          Kernel.Async.Interest.writable
          source
      in
      let _ = Kernel.Async.Poll.poll ~timeout:0L poll in
      let _ = Kernel.Async.Poll.deregister poll source in
      ())

let benchmarks =
  Bench.[
    with_config
      ~config:{ iterations = 50; warmup = 10 }
      "io stdin read len 0"
      bench_stdin_read_len_zero;
    with_config
      ~config:{ iterations = 50; warmup = 10 }
      "io stdout write len 0"
      bench_stdout_write_len_zero;
    with_config
      ~config:{ iterations = 50; warmup = 10 }
      "io stderr write len 0"
      bench_stderr_write_len_zero;
    with_config
      ~config:{ iterations = 50; warmup = 10 }
      "io stdin readv len 0"
      bench_stdin_read_vectored_len_zero;
    with_config
      ~config:{ iterations = 50; warmup = 10 }
      "io stdout writev len 0"
      bench_stdout_write_vectored_len_zero;
    with_config
      ~config:{ iterations = 50; warmup = 10 }
      "io stderr writev len 0"
      bench_stderr_write_vectored_len_zero;
    with_config
      ~config:{ iterations = 50; warmup = 10 }
      "io stdin source poll/close"
      bench_stdin_source_poll;
    with_config
      ~config:{ iterations = 50; warmup = 10 }
      "io stdout source poll/close"
      bench_stdout_source_poll;
    with_config
      ~config:{ iterations = 50; warmup = 10 }
      "io stderr source poll/close"
      bench_stderr_source_poll;
  ]

let main ~args = Bench.Cli.main ~name:"kernel_new_io_bench" ~benchmarks ~args

let () = Runtime.run ~main ~args:Env.args ()
