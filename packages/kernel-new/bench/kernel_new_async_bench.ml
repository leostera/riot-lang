open Std
module Kernel = Kernel_new

let panic_file = fun error ->
  Kernel.SystemError.panic (Kernel.Error.to_string (Kernel.Error.of_fs_file error))

let panic_async = fun error ->
  Kernel.SystemError.panic (Kernel.Error.to_string (Kernel.Error.of_async error))

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
  | Error error -> panic_file error
  | Ok pipe ->
      protect
        ~finally:(fun () ->
          let _ = Kernel.Fs.File.close pipe.read_end in
          let _ = Kernel.Fs.File.close pipe.write_end in
          ())
        (fun () -> fn pipe)

let rec close_pipes pipes =
  match pipes with
  | [] -> ()
  | Kernel.Fs.File.{ read_end; write_end } :: rest ->
      let _ = Kernel.Fs.File.close read_end in
      let _ = Kernel.Fs.File.close write_end in
      close_pipes rest

let with_pipes = fun count fn ->
  let rec create remaining acc =
    if remaining = 0 then
      Ok acc
    else
      match Kernel.Fs.File.pipe () with
      | Error error -> Error error
      | Ok pipe -> create (remaining - 1) (pipe :: acc)
  in
  match create count [] with
  | Error error -> panic_file error
  | Ok pipes -> protect ~finally:(fun () -> close_pipes pipes) (fun () -> fn (List.rev pipes))

let with_poll = fun fn ->
  match Kernel.Async.Poll.make () with
  | Error error -> panic_async error
  | Ok poll ->
      protect
        ~finally:(fun () ->
          let _ = Kernel.Async.Poll.close poll in
          ())
        (fun () -> fn poll)

let bench_register_and_deregister = fun () ->
  with_pipe
    (fun pipe ->
      with_poll
        (fun poll ->
          let source = Kernel.Fs.File.to_source pipe.read_end in
          let token = Kernel.Async.Token.make 7 in
          let _ = Kernel.Async.Poll.register poll token Kernel.Async.Interest.readable source in
          let _ = Kernel.Async.Poll.deregister poll source in
          ()))

let bench_pipe_wakeup = fun () ->
  with_pipe
    (fun pipe ->
      with_poll
        (fun poll ->
          let source = Kernel.Fs.File.to_source pipe.read_end in
          let token = Kernel.Async.Token.make 9 in
          let _ = Kernel.Async.Poll.register poll token Kernel.Async.Interest.readable source in
          let _ = Kernel.Fs.File.write pipe.write_end (Kernel.Bytes.of_string "x") in
          let _ = Kernel.Async.Poll.poll ~timeout:100_000_000L poll in
          ()))

let bench_reregister = fun () ->
  with_pipe
    (fun pipe ->
      with_poll
        (fun poll ->
          let source = Kernel.Fs.File.to_source pipe.write_end in
          let _ = Kernel.Async.Poll.register
            poll
            (Kernel.Async.Token.make 11)
            Kernel.Async.Interest.writable
            source in
          let _ = Kernel.Async.Poll.reregister
            poll
            (Kernel.Async.Token.make 12)
            Kernel.Async.Interest.writable
            source in
          ()))

let bench_timer_wakeup = fun () ->
  with_poll
    (fun poll ->
      match Kernel.Time.Timer.after_ns 1_000_000L with
      | Error error -> Kernel.SystemError.panic
        (Kernel.Error.to_string (Kernel.Error.of_time_timer error))
      | Ok timer ->
          let source = Kernel.Time.Timer.to_source timer in
          let _ = Kernel.Async.Poll.register
            poll
            (Kernel.Async.Token.make 13)
            Kernel.Async.Interest.readable
            source in
          let _ = Kernel.Async.Poll.poll ~timeout:100_000_000L poll in
          let _ = Kernel.Async.Poll.deregister poll source in
          ())

let bench_many_source_poll = fun () ->
  with_pipes 64
    (fun pipes ->
      with_poll
        (fun poll ->
          let rec register index = function
            | [] -> ()
            | Kernel.Fs.File.{ read_end; _ } :: rest ->
                let _ = Kernel.Async.Poll.register
                  poll
                  (Kernel.Async.Token.make index)
                  Kernel.Async.Interest.readable
                  (Kernel.Fs.File.to_source read_end) in
                register (index + 1) rest
          in
          let rec wake = function
            | [] -> ()
            | Kernel.Fs.File.{ write_end; _ } :: rest ->
                let _ = Kernel.Fs.File.write write_end (Kernel.Bytes.of_string "x") in
                wake rest
          in
          register 0 pipes;
          wake pipes;
          let _ = Kernel.Async.Poll.poll ~timeout:100_000_000L ~max_events:128 poll in
          ()))

let benchmarks =
  Bench.[
    with_config ~config:{ iterations = 50; warmup = 10 } "async register+deregister pipe source" bench_register_and_deregister;
    with_config ~config:{ iterations = 50; warmup = 10 } "async pipe wakeup" bench_pipe_wakeup;
    with_config ~config:{ iterations = 50; warmup = 10 } "async reregister pipe source" bench_reregister;
    with_config ~config:{ iterations = 50; warmup = 10 } "async timer wakeup" bench_timer_wakeup;
    with_config ~config:{ iterations = 25; warmup = 5 } "async many-source pipe wakeup" bench_many_source_poll;
  ]

let () =
  Actors.run
    ~main:(fun ~args -> Bench.Cli.main ~name:"kernel_new_async_bench" ~benchmarks ~args)
    ~args:Env.args
    ()
