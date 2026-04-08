open Std
module Kernel = Kernel_new

let protect = fun ~finally fn ->
  try
    let value = fn () in
    finally ();
    value
  with
  | error ->
      finally ();
      raise error

let with_pipe = fun fn ->
  match Kernel.Fs.File.pipe () with
  | Error error -> Kernel.Error.panic (Kernel.Error.to_string error)
  | Ok pipe ->
      protect
        ~finally:(fun () ->
          let _ = Kernel.Fs.File.close pipe.read_end in
          let _ = Kernel.Fs.File.close pipe.write_end in
          ())
        (fun () -> fn pipe)

let bench_register_and_deregister = fun () ->
  with_pipe
    (fun pipe ->
      match Kernel.Async.Poll.make () with
      | Error error -> Kernel.Error.panic (Kernel.Error.to_string error)
      | Ok poll ->
          let source = Kernel.Fs.File.to_source pipe.read_end in
          let token = Kernel.Async.Token.make 7 in
          let _ = Kernel.Async.Poll.register poll token Kernel.Async.Interest.readable source in
          let _ = Kernel.Async.Poll.deregister poll source in
          ())

let bench_pipe_wakeup = fun () ->
  with_pipe
    (fun pipe ->
      match Kernel.Async.Poll.make () with
      | Error error -> Kernel.Error.panic (Kernel.Error.to_string error)
      | Ok poll ->
          let source = Kernel.Fs.File.to_source pipe.read_end in
          let token = Kernel.Async.Token.make 9 in
          let _ = Kernel.Async.Poll.register poll token Kernel.Async.Interest.readable source in
          let _ = Kernel.Fs.File.write pipe.write_end (Kernel.Bytes.of_string "x") in
          let _ = Kernel.Async.Poll.poll ~timeout:100_000_000L poll in
          ())

let benchmarks =
  Bench.[
    with_config ~config:{ iterations = 50; warmup = 10 } "async register+deregister pipe source" bench_register_and_deregister;
    with_config ~config:{ iterations = 50; warmup = 10 } "async pipe wakeup" bench_pipe_wakeup;
  ]

let () =
  Actors.run
    ~main:(fun ~args -> Bench.Cli.main ~name:"kernel_new_async_bench" ~benchmarks ~args)
    ~args:Env.args
    ()
