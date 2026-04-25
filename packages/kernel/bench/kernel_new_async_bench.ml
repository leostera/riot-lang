open Std

module Kernel = Kernel

let panic_file = fun error -> Kernel.SystemError.panic (Kernel.Error.to_string (Kernel.Error.from_fs_file error))

let panic_async = fun error -> Kernel.SystemError.panic (Kernel.Error.to_string (Kernel.Error.from_async error))

let panic_time_timer = fun error -> Kernel.SystemError.panic (Kernel.Error.to_string (Kernel.Error.from_time_timer error))

let panic_udp = fun error -> Kernel.SystemError.panic (Kernel.Error.to_string (Kernel.Error.from_net_udp_socket error))

let panic_process = fun error -> Kernel.SystemError.panic (Kernel.Error.to_string (Kernel.Error.from_process error))

let lift_async result =
  match result with
  | Kernel.Result.Ok value -> value
  | Kernel.Result.Error error -> panic_async error

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
      protect ~finally:(
        fun () ->
          let _ = Kernel.Fs.File.close pipe.read_end in
          let _ = Kernel.Fs.File.close pipe.write_end in ()
      )
        (
          fun () -> fn pipe
        )

let rec close_pipes pipes =
  match pipes with
  | [] -> ()
  | Kernel.Fs.File.{ read_end; write_end } :: rest ->
      let _ = Kernel.Fs.File.close read_end in
      let _ = Kernel.Fs.File.close write_end in close_pipes rest

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
  | Ok pipes ->
      protect ~finally:(
        fun () -> close_pipes pipes
      )
        (
          fun () -> fn (List.reverse pipes)
        )

let with_poll = fun fn ->
  match Kernel.Async.Poll.make () with
  | Error error -> panic_async error
  | Ok poll ->
      protect ~finally:(
        fun () ->
          let _ = Kernel.Async.Poll.close poll in ()
      )
        (
          fun () -> fn poll
        )

let with_process = fun process fn ->
  protect ~finally:(
    fun () ->
      let _ = Kernel.Process.close process in ()
  )
    (
      fun () -> fn process
    )

let with_udp_pair = fun fn ->
  match Kernel.Net.UdpSocket.bind (Kernel.Net.SocketAddr.loopback_v4 ~port:0) with
  | Error error -> panic_udp error
  | Ok server ->
      protect ~finally:(
        fun () ->
          let _ = Kernel.Net.UdpSocket.close server in ()
      )
        (
          fun () ->
            match Kernel.Net.UdpSocket.bind (Kernel.Net.SocketAddr.loopback_v4 ~port:0) with
            | Error error -> panic_udp error
            | Ok client ->
                protect ~finally:(
                  fun () ->
                    let _ = Kernel.Net.UdpSocket.close client in ()
                )
                  (
                    fun () -> fn server client
                  )
        )

let bench_register_and_deregister = fun () ->
  with_pipe
    (
      fun pipe ->
        with_poll
          (
            fun poll ->
              let source = Kernel.Fs.File.to_source pipe.read_end in
              let token = Kernel.Async.Token.make 7 in
              let _ = Kernel.Async.Poll.register poll token Kernel.Async.Interest.readable source in
              let _ = Kernel.Async.Poll.deregister poll source in ()
          )
    )

let bench_pipe_wakeup = fun () ->
  with_pipe
    (
      fun pipe ->
        with_poll
          (
            fun poll ->
              let source = Kernel.Fs.File.to_source pipe.read_end in
              let token = Kernel.Async.Token.make 9 in
              let _ = Kernel.Async.Poll.register poll token Kernel.Async.Interest.readable source in
              let _ = Kernel.Fs.File.write pipe.write_end (Kernel.Bytes.from_string "x") in
              let _ = Kernel.Async.Poll.poll ~timeout:100_000_000L poll in ()
          )
    )

let bench_reregister = fun () ->
  with_pipe
    (
      fun pipe ->
        with_poll
          (
            fun poll ->
              let source = Kernel.Fs.File.to_source pipe.write_end in
              let _ = Kernel.Async.Poll.register poll (Kernel.Async.Token.make 11) Kernel.Async.Interest.writable source in
              let _ = Kernel.Async.Poll.reregister poll (Kernel.Async.Token.make 12) Kernel.Async.Interest.writable source in ()
          )
    )

let bench_timer_wakeup = fun () ->
  with_poll
    (
      fun poll ->
        match Kernel.Time.Timer.after_ns 1_000_000L with
        | Error error -> Kernel.SystemError.panic (Kernel.Error.to_string (Kernel.Error.from_time_timer error))
        | Ok timer ->
            let source = Kernel.Time.Timer.to_source timer in
            let _ = Kernel.Async.Poll.register poll (Kernel.Async.Token.make 13) Kernel.Async.Interest.readable source in
            let _ = Kernel.Async.Poll.poll ~timeout:100_000_000L poll in
            let _ = Kernel.Async.Poll.deregister poll source in ()
    )

let bench_many_source_poll = fun () ->
  with_pipes 64
    (
      fun pipes ->
        with_poll
          (
            fun poll ->
              let rec register index = function
                | [] -> ()
                | Kernel.Fs.File.{ read_end; _ } :: rest ->
                    let _ = Kernel.Async.Poll.register poll (Kernel.Async.Token.make index) Kernel.Async.Interest.readable (Kernel.Fs.File.to_source read_end) in register (index + 1) rest
              in
              let rec wake = function
                | [] -> ()
                | Kernel.Fs.File.{ write_end; _ } :: rest ->
                    let _ = Kernel.Fs.File.write write_end (Kernel.Bytes.from_string "x") in wake rest
              in
              register 0 pipes;
              wake pipes;
              let _ = Kernel.Async.Poll.poll ~timeout:100_000_000L ~max_events:128 poll in ()
          )
    )

let bench_mixed_source_poll = fun () ->
  with_pipe
    (
      fun pipe ->
        with_poll
          (
            fun poll ->
              match Kernel.Time.Timer.after_ns 1_000_000L with
              | Error error -> panic_time_timer error
              | Ok timer ->
                  let stdio = Kernel.Process.{ stdin = Stdin.Null; stdout = Stdout.Null; stderr = Stderr.Null } in
                  match Kernel.Process.spawn ~program:"/bin/sh" ~args:[|
                    "-c";
                    "sleep 0.02";
                  |] ~stdio () with
                  | Error error -> panic_process error
                  | Ok process ->
                      with_process process
                        (
                          fun process ->
                            with_udp_pair
                              (
                                fun server client ->
                                  let timer_source = Kernel.Time.Timer.to_source timer in
                                  let process_source = Kernel.Process.to_source process in
                                  let pipe_source = Kernel.Fs.File.to_source pipe.read_end in
                                  let udp_source = Kernel.Net.UdpSocket.to_source server in
                                  let server_addr =
                                    match Kernel.Net.UdpSocket.local_addr server with
                                    | Ok addr -> addr
                                    | Error error -> panic_udp error
                                  in
                                  let _ = Kernel.Async.Poll.register poll (Kernel.Async.Token.make "pipe") Kernel.Async.Interest.readable pipe_source in
                                  let _ = Kernel.Async.Poll.register poll (Kernel.Async.Token.make "timer") Kernel.Async.Interest.readable timer_source in
                                  let _ = Kernel.Async.Poll.register poll (Kernel.Async.Token.make "process") Kernel.Async.Interest.priority process_source in
                                  let _ = Kernel.Async.Poll.register poll (Kernel.Async.Token.make "udp") Kernel.Async.Interest.readable udp_source in
                                  protect ~finally:(
                                    fun () ->
                                      let _ = Kernel.Async.Poll.deregister poll pipe_source in
                                      let _ = Kernel.Async.Poll.deregister poll timer_source in
                                      let _ = Kernel.Async.Poll.deregister poll process_source in
                                      let _ = Kernel.Async.Poll.deregister poll udp_source in ()
                                  )
                                    (
                                      fun () ->
                                        let _ = Kernel.Fs.File.write pipe.write_end (Kernel.Bytes.from_string "x") in
                                        let _ = Kernel.Net.UdpSocket.send_to client server_addr (Kernel.Bytes.from_string "u") in
                                        let seen_pipe = ref false in
                                        let seen_timer = ref false in
                                        let seen_process = ref false in
                                        let seen_udp = ref false in
                                        let rec mark = function
                                          | [] -> ()
                                          | event :: rest ->
                                              let token = Kernel.Async.Token.unsafe_value (Kernel.Async.Event.token event) in
                                              if token = "pipe" && Kernel.Async.Event.is_readable event then
                                                seen_pipe := true
                                              else
                                                if token = "timer" && Kernel.Async.Event.is_readable event then
                                                  seen_timer := true
                                                else
                                                  if token = "process" && Kernel.Async.Event.is_priority event then
                                                    seen_process := true
                                                  else
                                                    if token = "udp" && Kernel.Async.Event.is_readable event then
                                                      seen_udp := true;
                                              mark rest
                                        in
                                        let rec poll_until attempts =
                                          if !seen_pipe && !seen_timer && !seen_process && !seen_udp then
                                            ()
                                          else
                                            if attempts = 0 then
                                              Kernel.SystemError.panic "expected mixed-source poll to surface all readiness kinds"
                                            else
                                              let events = lift_async (Kernel.Async.Poll.poll ~timeout:100_000_000L ~max_events:16 poll) in mark events;
                                          poll_until (attempts - 1)
                                        in
                                        poll_until 8
                                    )
                              )
                        )
          )
    )

let benchmarks = Bench.[
  with_config ~config:{ iterations = 50; warmup = 10 } "async register+deregister pipe source" bench_register_and_deregister;
  with_config ~config:{ iterations = 50; warmup = 10 } "async pipe wakeup" bench_pipe_wakeup;
  with_config ~config:{ iterations = 50; warmup = 10 } "async reregister pipe source" bench_reregister;
  with_config ~config:{ iterations = 50; warmup = 10 } "async timer wakeup" bench_timer_wakeup;
  with_config ~config:{ iterations = 25; warmup = 5 } "async many-source pipe wakeup" bench_many_source_poll;
  with_config ~config:{ iterations = 25; warmup = 5 } "async mixed-source wakeup" bench_mixed_source_poll;
]

let main ~args = Bench.Cli.main ~name:"kernel_new_async_bench" ~benchmarks ~args

let () = Runtime.run ~main ~args:Env.args ()
