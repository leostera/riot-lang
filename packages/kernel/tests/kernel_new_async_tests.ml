open Std
module Test = Std.Test
module Kernel = Kernel

let ( let* ) value fn = Result.and_then value ~fn

let lift_file result =
  match result with
  | Kernel.Result.Ok value -> Ok value
  | Kernel.Result.Error error -> Error (Kernel.Fs.File.error_to_string error)

let lift_async result =
  match result with
  | Kernel.Result.Ok value -> Ok value
  | Kernel.Result.Error error -> Error (Kernel.Async.error_to_string error)

let lift_process result =
  match result with
  | Kernel.Result.Ok value -> Ok value
  | Kernel.Result.Error error -> Error (Kernel.Process.error_to_string error)

let lift_timer result =
  match result with
  | Kernel.Result.Ok value -> Ok value
  | Kernel.Result.Error error -> Error (Kernel.Time.Timer.error_to_string error)

let lift_udp result =
  match result with
  | Kernel.Result.Ok value -> Ok value
  | Kernel.Result.Error error -> Error (Kernel.Net.UdpSocket.error_to_string error)

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
  let* pipe = lift_file (Kernel.Fs.File.pipe ()) in
  protect
    ~finally:(fun () ->
      let _ = Kernel.Fs.File.close pipe.read_end in
      let _ = Kernel.Fs.File.close pipe.write_end in
      ())
    (fun () -> fn pipe.read_end pipe.write_end)

let with_two_pipes = fun fn ->
  let* first = lift_file (Kernel.Fs.File.pipe ()) in
  let* second = lift_file (Kernel.Fs.File.pipe ()) in
  protect
    ~finally:(fun () ->
      let _ = Kernel.Fs.File.close first.read_end in
      let _ = Kernel.Fs.File.close first.write_end in
      let _ = Kernel.Fs.File.close second.read_end in
      let _ = Kernel.Fs.File.close second.write_end in
      ())
    (fun () -> fn first second)

let rec close_pipes pipes =
  match pipes with
  | [] -> ()
  | (read_end, write_end) :: rest ->
      let _ = Kernel.Fs.File.close read_end in
      let _ = Kernel.Fs.File.close write_end in
      close_pipes rest

let rec close_processes processes =
  match processes with
  | [] -> ()
  | process :: rest ->
      let _ = Kernel.Process.close process in
      close_processes rest

let close_udp = fun socket ->
  let _ = Kernel.Net.UdpSocket.close socket in
  ()

let with_pipes = fun count fn ->
  let rec create remaining acc =
    if remaining = 0 then
      Ok acc
    else
      let* pipe = lift_file (Kernel.Fs.File.pipe ()) in
      create (remaining - 1) ((pipe.read_end, pipe.write_end) :: acc)
  in
  let* pipes = create count [] in
  protect ~finally:(fun () -> close_pipes pipes) (fun () -> fn (List.reverse pipes))

let with_poll = fun fn ->
  let* poll = lift_async (Kernel.Async.Poll.make ()) in
  protect
    ~finally:(fun () ->
      let _ = Kernel.Async.Poll.close poll in
      ())
    (fun () -> fn poll)

let drain_one_byte = fun file ->
  let buffer = Kernel.Bytes.create ~size:1 in
  match Kernel.Fs.File.read file buffer with
  | Kernel.Result.Ok _ -> Ok ()
  | Kernel.Result.Error error -> Error (Kernel.Fs.File.error_to_string error)

let has_token = fun token events ->
  List.any events
    ~fn:(fun event ->
      Kernel.Async.Token.equal token (Kernel.Async.Event.token event))

let with_processes = fun count fn ->
  let stdio = Kernel.Process.{ stdin = Stdin.Null; stdout = Stdout.Null; stderr = Stderr.Null } in
  let rec spawn remaining acc =
    if remaining = 0 then
      Ok acc
    else
      match Kernel.Process.spawn ~program:"/bin/sh" ~args:[|"-c"; "sleep 0.05"|] ~stdio () with
      | Kernel.Result.Ok process -> spawn (remaining - 1) (process :: acc)
      | Kernel.Result.Error error -> Error (Kernel.Process.error_to_string error)
  in
  let* processes = spawn count [] in
  protect ~finally:(fun () -> close_processes processes) (fun () -> fn (List.reverse processes))

let wait_for_event = fun ?(timeout = 100_000_000L) poll ->
  lift_async (Kernel.Async.Poll.poll ~timeout poll)

let test_poll_reports_pipe_readability = fun _ctx ->
  with_pipe
    (fun read_end write_end ->
      with_poll
        (fun poll ->
          let token = Kernel.Async.Token.make 41 in
          let* () = lift_async
            (Kernel.Async.Poll.register
              poll
              token
              Kernel.Async.Interest.readable
              (Kernel.Fs.File.to_source read_end)) in
          let payload = Kernel.Bytes.from_string "x" in
          let* written = lift_file (Kernel.Fs.File.write write_end payload) in
          if written != 1 then
            Error "expected pipe write to write one byte"
          else
            let* events = lift_async (Kernel.Async.Poll.poll ~timeout:100_000_000L poll) in
            let found =
              List.any
                events
                ~fn:(fun event ->
                  Kernel.Async.Event.is_readable event
                  && Kernel.Async.Token.equal token (Kernel.Async.Event.token event))
            in
            if found then
              Ok ()
            else
              Error "expected poll to report readability for pipe source"))

let test_poll_reports_pipe_read_closed = fun _ctx ->
  with_pipe
    (fun read_end write_end ->
      with_poll
        (fun poll ->
          let token = Kernel.Async.Token.make 411 in
          let source = Kernel.Fs.File.to_source read_end in
          let* () = lift_async
            (Kernel.Async.Poll.register poll token Kernel.Async.Interest.readable source) in
          let* () = lift_file (Kernel.Fs.File.close write_end) in
          let* events = lift_async (Kernel.Async.Poll.poll ~timeout:100_000_000L poll) in
          let found =
            List.any
              events
              ~fn:(fun event ->
                Kernel.Async.Event.is_read_closed event
                && Kernel.Async.Token.equal token (Kernel.Async.Event.token event))
          in
          if found then
            Ok ()
          else
            Error "expected poll to report read closure when the pipe writer closes"))

let test_deregister_removes_pipe_source = fun _ctx ->
  with_pipe
    (fun read_end write_end ->
      with_poll
        (fun poll ->
          let token = Kernel.Async.Token.make 42 in
          let source = Kernel.Fs.File.to_source read_end in
          let* () = lift_async
            (Kernel.Async.Poll.register poll token Kernel.Async.Interest.readable source) in
          let* () = lift_async (Kernel.Async.Poll.deregister poll source) in
          let payload = Kernel.Bytes.from_string "x" in
          let* written = lift_file (Kernel.Fs.File.write write_end payload) in
          if written != 1 then
            Error "expected pipe write to write one byte"
          else
            let* events = lift_async (Kernel.Async.Poll.poll ~timeout:0L poll) in
            let found =
              List.any
                events
                ~fn:(fun event ->
                  Kernel.Async.Event.is_readable event
                  && Kernel.Async.Token.equal token (Kernel.Async.Event.token event))
            in
            if found then
              Error "expected deregistered source to stop producing events"
            else
              Ok ()))

let test_reregister_updates_pipe_token = fun _ctx ->
  with_pipe
    (fun _read_end write_end ->
      with_poll
        (fun poll ->
          let token_a = Kernel.Async.Token.make "first" in
          let token_b = Kernel.Async.Token.make "second" in
          let source = Kernel.Fs.File.to_source write_end in
          let* () = lift_async
            (Kernel.Async.Poll.register poll token_a Kernel.Async.Interest.writable source) in
          let* () = lift_async
            (Kernel.Async.Poll.reregister poll token_b Kernel.Async.Interest.writable source) in
          let* events = wait_for_event poll in
          let found =
            List.any
              events
              ~fn:(fun event ->
                Kernel.Async.Event.is_writable event
                && Kernel.String.equal
                  (Kernel.Async.Token.unsafe_value (Kernel.Async.Event.token event))
                  "second")
          in
          if found then
            Ok ()
          else
            Error "expected reregister to replace the writable token"))

let test_reregister_replaces_interest = fun _ctx ->
  with_pipe
    (fun _read_end write_end ->
      with_poll
        (fun poll ->
          let source = Kernel.Fs.File.to_source write_end in
          let token = Kernel.Async.Token.make "replaced-interest" in
          let* () = lift_async
            (Kernel.Async.Poll.register poll token Kernel.Async.Interest.writable source) in
          let* () = lift_async
            (Kernel.Async.Poll.reregister poll token Kernel.Async.Interest.readable source) in
          let* events = lift_async (Kernel.Async.Poll.poll ~timeout:0L poll) in
          let found =
            List.any
              events
              ~fn:(fun event ->
                Kernel.Async.Event.is_writable event
                && Kernel.Async.Token.equal token (Kernel.Async.Event.token event))
          in
          if found then
            Error "expected replaced writable interest to stop producing events"
          else
            Ok ()))

let test_registering_same_source_twice_updates_token = fun _ctx ->
  with_pipe
    (fun _read_end write_end ->
      with_poll
        (fun poll ->
          let source = Kernel.Fs.File.to_source write_end in
          let first = Kernel.Async.Token.make "duplicate-first" in
          let second = Kernel.Async.Token.make "duplicate-second" in
          let* () = lift_async
            (Kernel.Async.Poll.register poll first Kernel.Async.Interest.writable source) in
          let* () = lift_async
            (Kernel.Async.Poll.register poll second Kernel.Async.Interest.writable source) in
          let* events = wait_for_event poll in
          let saw_first =
            List.any
              events
              ~fn:(fun event ->
                Kernel.Async.Event.is_writable event
                && Kernel.Async.Token.equal first (Kernel.Async.Event.token event))
          in
          let saw_second =
            List.any
              events
              ~fn:(fun event ->
                Kernel.Async.Event.is_writable event
                && Kernel.Async.Token.equal second (Kernel.Async.Event.token event))
          in
          if saw_second && not saw_first then
            Ok ()
          else
            Error "expected duplicate register to replace the writable token in place"))

let test_deregister_of_never_registered_source_is_harmless = fun _ctx ->
  with_pipe
    (fun read_end write_end ->
      with_poll
        (fun poll ->
          let source = Kernel.Fs.File.to_source read_end in
          let* () = lift_async (Kernel.Async.Poll.deregister poll source) in
          let* () = lift_async (Kernel.Async.Poll.deregister poll source) in
          let token = Kernel.Async.Token.make "never-registered" in
          let* () = lift_async
            (Kernel.Async.Poll.register poll token Kernel.Async.Interest.readable source) in
          let* written = lift_file (Kernel.Fs.File.write write_end (Kernel.Bytes.from_string "x")) in
          if written != 1 then
            Error "expected pipe write to make progress after noop deregisters"
          else
            let* events = lift_async (Kernel.Async.Poll.poll ~timeout:100_000_000L poll) in
            let found =
              List.any
                events
                ~fn:(fun event ->
                  Kernel.Async.Event.is_readable event
                  && Kernel.Async.Token.equal token (Kernel.Async.Event.token event))
            in
            if found then
              Ok ()
            else
              Error "expected noop deregisters to preserve later source registration"))

let test_poll_handles_many_pipe_sources = fun _ctx ->
  with_pipes 64
    (fun pipes ->
      with_poll
        (fun poll ->
          let rec register index = function
            | [] -> Ok ()
            | (read_end, _) :: rest ->
                let* () = lift_async
                  (Kernel.Async.Poll.register
                    poll
                    (Kernel.Async.Token.make index)
                    Kernel.Async.Interest.readable
                    (Kernel.Fs.File.to_source read_end)) in
                register (index + 1) rest
          in
          let rec write_all = function
            | [] -> Ok ()
            | (_, write_end) :: rest ->
                let* written = lift_file
                  (Kernel.Fs.File.write write_end (Kernel.Bytes.from_string "x")) in
                if written != 1 then
                  Error "expected pipe write to write one byte"
                else
                  write_all rest
          in
          let* () = register 0 pipes in
          let* () = write_all pipes in
          let* events = lift_async
            (Kernel.Async.Poll.poll ~timeout:100_000_000L ~max_events:128 poll) in
          let seen = Kernel.Array.make ~count:64 ~value:false in
          let rec mark = function
            | [] -> ()
            | event :: rest ->
                if Kernel.Async.Event.is_readable event then
                  let token = Kernel.Async.Token.unsafe_value (Kernel.Async.Event.token event) in
                  if token >= 0 && token < 64 then
                    Kernel.Array.set seen ~at:token ~value:true;
                  mark rest
          in
          mark events;
          let rec all_seen index =
            if index = 64 then
              true
            else if Kernel.Array.get_unchecked seen ~at:index then
              all_seen (index + 1)
            else
              false
          in
          if all_seen 0 then
            Ok ()
          else
            Error "expected poll to surface readability for many registered sources"))

let test_token_roundtrips_structured_values = fun _ctx ->
  let token = Kernel.Async.Token.make ("pipe", 99) in
  let tag, value = Kernel.Async.Token.unsafe_value token in
  if Kernel.String.equal tag "pipe" && value = 99 then
    Ok ()
  else
    Error "expected token to roundtrip its structured value"

let test_poll_rejects_invalid_limits = fun _ctx ->
  with_poll
    (fun poll ->
      match (Kernel.Async.Poll.poll ~timeout:(-1L) poll, Kernel.Async.Poll.poll ~max_events:0 poll) with
      | (Kernel.Result.Error (Kernel.Async.InvalidTimeoutNs { timeout_ns }), Kernel.Result.Error (Kernel.Async.InvalidMaxEvents {
        max_events
      })) when timeout_ns = (-1L) && max_events = 0 -> Ok ()
      | (Kernel.Result.Error error, _) -> Error (Kernel.Async.error_to_string error)
      | (_, Kernel.Result.Error error) -> Error (Kernel.Async.error_to_string error)
      | _ -> Error "expected async poll to reject invalid timeout and max_events")

let test_poll_handles_many_process_exits = fun _ctx ->
  with_poll
    (fun poll ->
      with_processes 16
        (fun processes ->
          let rec register index = function
            | [] -> Ok ()
            | process :: rest ->
                let* () = lift_async
                  (Kernel.Async.Poll.register
                    poll
                    (Kernel.Async.Token.make index)
                    Kernel.Async.Interest.priority
                    (Kernel.Process.to_source process)) in
                register (index + 1) rest
          in
          let seen = Kernel.Array.make ~count:16 ~value:false in
          let rec mark_events = function
            | [] -> ()
            | event :: rest ->
                if Kernel.Async.Event.is_priority event then
                  let token = Kernel.Async.Token.unsafe_value (Kernel.Async.Event.token event) in
                  if token >= 0 && token < 16 then
                    Kernel.Array.set seen ~at:token ~value:true;
                  mark_events rest
          in
          let rec mark_exits index = function
            | [] -> Ok ()
            | process :: rest ->
                let* status =
                  match Kernel.Process.try_wait process with
                  | Kernel.Result.Ok status -> Ok status
                  | Kernel.Result.Error error -> Error (Kernel.Process.error_to_string error)
                in
                (
                  match status with
                  | Some (Kernel.Process.Exited 0) ->
                      if index < 16 then
                        Kernel.Array.set seen ~at:index ~value:true
                  | _ -> ()
                );
                mark_exits (index + 1) rest
          in
          let rec all_seen index =
            if index = 16 then
              true
            else if Kernel.Array.get_unchecked seen ~at:index then
              all_seen (index + 1)
            else
              false
          in
          let* () = register 0 processes in
          let rec poll_until attempts =
            if attempts = 0 then
              Error "expected many registered child processes to report exit readiness"
            else
              let* events = lift_async
                (Kernel.Async.Poll.poll ~timeout:100_000_000L ~max_events:32 poll) in
              mark_events events;
              let* () = mark_exits 0 processes in
              if all_seen 0 then
                Ok ()
              else
                poll_until (attempts - 1)
          in
          poll_until 16))

let test_poll_handles_many_timer_sources = fun _ctx ->
  with_poll
    (fun poll ->
      let rec create remaining acc =
        if remaining = 0 then
          Ok acc
        else
          let* timer =
            match Kernel.Time.Timer.after_ns 5_000_000L with
            | Kernel.Result.Ok timer -> Ok timer
            | Kernel.Result.Error error -> Error (Kernel.Time.Timer.error_to_string error)
          in
          create (remaining - 1) (timer :: acc)
      in
      let* timers = create 16 [] in
      let timers = List.reverse timers in
      let rec register index = function
        | [] -> Ok ()
        | timer :: rest ->
            let* () = lift_async
              (Kernel.Async.Poll.register
                poll
                (Kernel.Async.Token.make index)
                Kernel.Async.Interest.readable
                (Kernel.Time.Timer.to_source timer)) in
            register (index + 1) rest
      in
      let seen = Kernel.Array.make ~count:16 ~value:false in
      let rec mark = function
        | [] -> ()
        | event :: rest ->
            if Kernel.Async.Event.is_readable event then
              let token = Kernel.Async.Token.unsafe_value (Kernel.Async.Event.token event) in
              if token >= 0 && token < 16 then
                Kernel.Array.set seen ~at:token ~value:true;
              mark rest
      in
      let rec all_seen index =
        if index = 16 then
          true
        else if Kernel.Array.get_unchecked seen ~at:index then
          all_seen (index + 1)
        else
          false
      in
      let* () = register 0 timers in
      let rec poll_until attempts =
        if attempts = 0 then
          Error "expected many timer sources to wake the poller"
        else
          let* events = lift_async (Kernel.Async.Poll.poll ~timeout:100_000_000L ~max_events:32 poll) in
          mark events;
          if all_seen 0 then
            Ok ()
          else
            poll_until (attempts - 1)
      in
      poll_until 8)

let test_repeated_register_and_deregister_stays_healthy = fun _ctx ->
  with_pipe
    (fun read_end _write_end ->
      with_poll
        (fun poll ->
          let source = Kernel.Fs.File.to_source read_end in
          let rec loop remaining =
            if remaining = 0 then
              Ok ()
            else
              let* () = lift_async
                (Kernel.Async.Poll.register
                  poll
                  (Kernel.Async.Token.make remaining)
                  Kernel.Async.Interest.readable
                  source) in
              let* () = lift_async (Kernel.Async.Poll.deregister poll source) in
              loop (remaining - 1)
          in
          loop 256))

let test_repeated_register_reregister_and_deregister_stays_healthy = fun _ctx ->
  with_pipe
    (fun _read_end write_end ->
      with_poll
        (fun poll ->
          let source = Kernel.Fs.File.to_source write_end in
          let rec loop remaining =
            if remaining = 0 then
              Ok ()
            else
              let token = Kernel.Async.Token.make ("cycle", remaining) in
              let replacement = Kernel.Async.Token.make ("replacement", remaining) in
              let* () = lift_async
                (Kernel.Async.Poll.register poll token Kernel.Async.Interest.writable source) in
              let* () = lift_async
                (Kernel.Async.Poll.reregister poll replacement Kernel.Async.Interest.writable source) in
              let* events = wait_for_event poll in
              let found =
                List.any
                  events
                  ~fn:(fun event ->
                    Kernel.Async.Event.is_writable event
                    && Kernel.Async.Token.equal replacement (Kernel.Async.Event.token event))
              in
              if not found then
                Error "expected repeated reregister cycles to preserve the replacement token"
              else
                let* () = lift_async (Kernel.Async.Poll.deregister poll source) in
                loop (remaining - 1)
          in
          loop 64))

let test_poll_handles_mixed_source_types = fun _ctx ->
  with_pipe
    (fun read_end write_end ->
      with_poll
        (fun poll ->
          let stdio =
            Kernel.Process.{ stdin = Stdin.Null; stdout = Stdout.Null; stderr = Stderr.Null } in
          let* timer = lift_timer (Kernel.Time.Timer.after_ns 5_000_000L) in
          let* process = lift_process
            (Kernel.Process.spawn ~program:"/bin/sh" ~args:[|"-c"; "sleep 0.02"|] ~stdio ()) in
          protect
            ~finally:(fun () ->
              let _ = Kernel.Process.close process in
              ())
            (fun () ->
              let* server = lift_udp
                (Kernel.Net.UdpSocket.bind (Kernel.Net.SocketAddr.loopback_v4 ~port:0)) in
              protect ~finally:(fun () -> close_udp server)
                (fun () ->
                  let* client = lift_udp
                    (Kernel.Net.UdpSocket.bind (Kernel.Net.SocketAddr.loopback_v4 ~port:0)) in
                  protect ~finally:(fun () -> close_udp client)
                    (fun () ->
                      let timer_source = Kernel.Time.Timer.to_source timer in
                      let process_source = Kernel.Process.to_source process in
                      let pipe_source = Kernel.Fs.File.to_source read_end in
                      let udp_source = Kernel.Net.UdpSocket.to_source server in
                      let* server_addr = lift_udp (Kernel.Net.UdpSocket.local_addr server) in
                      let* () = lift_async
                        (Kernel.Async.Poll.register
                          poll
                          (Kernel.Async.Token.make "pipe")
                          Kernel.Async.Interest.readable
                          pipe_source) in
                      let* () = lift_async
                        (Kernel.Async.Poll.register
                          poll
                          (Kernel.Async.Token.make "timer")
                          Kernel.Async.Interest.readable
                          timer_source) in
                      let* () = lift_async
                        (Kernel.Async.Poll.register
                          poll
                          (Kernel.Async.Token.make "process")
                          Kernel.Async.Interest.priority
                          process_source) in
                      let* () = lift_async
                        (Kernel.Async.Poll.register
                          poll
                          (Kernel.Async.Token.make "udp")
                          Kernel.Async.Interest.readable
                          udp_source) in
                      protect
                        ~finally:(fun () ->
                          let _ = Kernel.Async.Poll.deregister poll pipe_source in
                          let _ = Kernel.Async.Poll.deregister poll timer_source in
                          let _ = Kernel.Async.Poll.deregister poll process_source in
                          let _ = Kernel.Async.Poll.deregister poll udp_source in
                          ())
                        (fun () ->
                          let* written = lift_file
                            (Kernel.Fs.File.write write_end (Kernel.Bytes.from_string "x")) in
                          if written != 1 then
                            Error "expected mixed-source pipe write to write one byte"
                          else
                            let* sent = lift_udp
                              (Kernel.Net.UdpSocket.send_to
                                client
                                server_addr
                                (Kernel.Bytes.from_string "u")) in
                            if sent != 1 then
                              Error "expected mixed-source udp send_to to write one byte"
                            else
                              let seen_pipe = ref false in
                              let seen_timer = ref false in
                              let seen_process = ref false in
                              let seen_udp = ref false in
                              let rec mark = function
                                | [] -> ()
                                | event :: rest ->
                                    let token = Kernel.Async.Token.unsafe_value
                                      (Kernel.Async.Event.token event) in
                                    if token = "pipe" && Kernel.Async.Event.is_readable event then
                                      seen_pipe := true
                                    else if token = "timer" && Kernel.Async.Event.is_readable event then
                                      seen_timer := true
                                    else if
                                      token = "process" && Kernel.Async.Event.is_priority event
                                    then
                                      seen_process := true
                                    else if token = "udp" && Kernel.Async.Event.is_readable event then
                                      seen_udp := true;
                                    mark rest
                              in
                              let rec all_seen () = !seen_pipe && !seen_timer && !seen_process && !seen_udp in
                              let rec poll_until attempts =
                                if all_seen () then
                                  Ok ()
                                else if attempts = 0 then
                                  Error "expected mixed source poll to surface pipe, timer, udp, and process readiness"
                                else
                                  let* events = lift_async
                                    (Kernel.Async.Poll.poll ~timeout:100_000_000L ~max_events:16 poll) in
                                  mark events;
                                  poll_until (attempts - 1)
                              in
                              poll_until 12))))))

let test_poll_tolerates_closed_registered_pipe_sources = fun _ctx ->
  with_pipes 8
    (fun pipes ->
      with_poll
        (fun poll ->
          let rec register index = function
            | [] -> Ok ()
            | (read_end, _) :: rest ->
                let* () = lift_async
                  (Kernel.Async.Poll.register
                    poll
                    (Kernel.Async.Token.make index)
                    Kernel.Async.Interest.readable
                    (Kernel.Fs.File.to_source read_end)) in
                register (index + 1) rest
          in
          let rec close_even index = function
            | [] -> ()
            | (read_end, _) :: rest ->
                if index land 1 = 0 then
                  let _ = Kernel.Fs.File.close read_end in
                  ();
                  close_even (index + 1) rest
          in
          let rec write_live index = function
            | [] -> Ok ()
            | (_, write_end) :: rest ->
                if index land 1 = 0 then
                  write_live (index + 1) rest
                else
                  let* written = lift_file
                    (Kernel.Fs.File.write write_end (Kernel.Bytes.from_string "x")) in
                  if written != 1 then
                    Error "expected pipe write to make progress for live registered sources"
                  else
                    write_live (index + 1) rest
          in
          let seen = Kernel.Array.make ~count:8 ~value:false in
          let rec mark = function
            | [] -> ()
            | event :: rest ->
                if Kernel.Async.Event.is_readable event then
                  let token = Kernel.Async.Token.unsafe_value (Kernel.Async.Event.token event) in
                  if token >= 0 && token < 8 then
                    Kernel.Array.set seen ~at:token ~value:true;
                  mark rest
          in
          let rec live_seen index =
            if index = 8 then
              true
            else if index land 1 = 0 then
              live_seen (index + 1)
            else if Kernel.Array.get_unchecked seen ~at:index then
              live_seen (index + 1)
            else
              false
          in
          let* () = register 0 pipes in
          close_even 0 pipes;
          let* () = write_live 0 pipes in
          let rec poll_until attempts =
            if attempts = 0 then
              Error "expected closed registered pipe sources to not poison remaining readiness"
            else
              let* events = lift_async
                (Kernel.Async.Poll.poll ~timeout:100_000_000L ~max_events:32 poll) in
              mark events;
              if live_seen 0 then
                Ok ()
              else
                poll_until (attempts - 1)
          in
          poll_until 8))

let test_interest_add_and_remove_roundtrip = fun _ctx ->
  let both = Kernel.Async.Interest.add Kernel.Async.Interest.readable Kernel.Async.Interest.writable in
  match Kernel.Async.Interest.remove both Kernel.Async.Interest.writable with
  | Some interest ->
      if
        Kernel.Async.Interest.is_readable interest
        && not (Kernel.Async.Interest.is_writable interest)
      then
        Ok ()
      else
        Error "expected Interest.remove to leave only the readable bit"
  | None -> Error "expected removing one bit from a combined interest to keep the other bit"

let test_interest_remove_all_bits_returns_none = fun _ctx ->
  let both = Kernel.Async.Interest.add Kernel.Async.Interest.readable Kernel.Async.Interest.writable in
  match Kernel.Async.Interest.remove both both with
  | None -> Ok ()
  | Some _ -> Error "expected removing all interest bits to return None"

let test_token_id_is_stable = fun _ctx ->
  let token = Kernel.Async.Token.make "stable" in
  if Kernel.Async.Token.id token = Kernel.Async.Token.id token then
    Ok ()
  else
    Error "expected Token.id to stay stable for the same token"

let test_fresh_poll_timeout_zero_is_quiet = fun _ctx ->
  with_poll
    (fun poll ->
      let* events = lift_async (Kernel.Async.Poll.poll ~timeout:0L poll) in
      if events = [] then
        Ok ()
      else
        Error "expected a fresh poller with timeout=0 to be quiet")

let test_poll_max_events_batches_without_dropping_readiness = fun _ctx ->
  with_two_pipes
    (fun first second ->
      with_poll
        (fun poll ->
          let first_token = Kernel.Async.Token.make 1 in
          let second_token = Kernel.Async.Token.make 2 in
          let first_source = Kernel.Fs.File.to_source first.read_end in
          let second_source = Kernel.Fs.File.to_source second.read_end in
          let payload = Kernel.Bytes.from_string "x" in
          let* () = lift_async
            (Kernel.Async.Poll.register poll first_token Kernel.Async.Interest.readable first_source) in
          let* () = lift_async
            (Kernel.Async.Poll.register poll second_token Kernel.Async.Interest.readable second_source) in
          let* _ = lift_file (Kernel.Fs.File.write first.write_end payload) in
          let* _ = lift_file (Kernel.Fs.File.write second.write_end payload) in
          let* first_batch = lift_async
            (Kernel.Async.Poll.poll ~timeout:100_000_000L ~max_events:1 poll) in
          let first_seen =
            if has_token first_token first_batch then
              Some (first.read_end, first_token, second_token)
            else if has_token second_token first_batch then
              Some (second.read_end, second_token, first_token)
            else
              None
          in
          match first_seen with
          | None -> Error "expected the first max_events=1 poll to report one ready source"
          | Some (ready_file, ready_token, other_token) ->
              let* () = drain_one_byte ready_file in
              let* second_batch = lift_async
                (Kernel.Async.Poll.poll ~timeout:100_000_000L ~max_events:1 poll) in
              if
                has_token ready_token first_batch
                && not (has_token other_token first_batch)
                && has_token other_token second_batch
              then
                Ok ()
              else
                Error "expected max_events=1 polls to batch readiness across successive calls"))

let test_duplicate_register_same_token_does_not_duplicate_event = fun _ctx ->
  with_pipe
    (fun read_end write_end ->
      with_poll
        (fun poll ->
          let token = Kernel.Async.Token.make 11 in
          let source = Kernel.Fs.File.to_source read_end in
          let payload = Kernel.Bytes.from_string "x" in
          let* () = lift_async
            (Kernel.Async.Poll.register poll token Kernel.Async.Interest.readable source) in
          let* () = lift_async
            (Kernel.Async.Poll.register poll token Kernel.Async.Interest.readable source) in
          let* _ = lift_file (Kernel.Fs.File.write write_end payload) in
          let* events = lift_async (Kernel.Async.Poll.poll ~timeout:100_000_000L ~max_events:8 poll) in
          let matches =
            List.filter
              events
              ~fn:(fun event ->
                Kernel.Async.Event.is_readable event
                && Kernel.Async.Token.equal token (Kernel.Async.Event.token event))
          in
          if List.length matches <= 1 then
            Ok ()
          else
            Error "expected duplicate registration with the same token to avoid duplicate readiness events"))

let test_pipe_writer_reports_write_closed_after_reader_closes = fun _ctx ->
  with_pipe
    (fun read_end write_end ->
      with_poll
        (fun poll ->
          let token = Kernel.Async.Token.make 12 in
          let source = Kernel.Fs.File.to_source write_end in
          let* () = lift_async
            (Kernel.Async.Poll.register poll token Kernel.Async.Interest.writable source) in
          let* () = lift_file (Kernel.Fs.File.close read_end) in
          let* events = lift_async (Kernel.Async.Poll.poll ~timeout:100_000_000L poll) in
          if
            List.any
              events
              ~fn:(fun event ->
                Kernel.Async.Token.equal token (Kernel.Async.Event.token event)
                && Kernel.Async.Event.is_write_closed event)
          then
            Ok ()
          else
            Error "expected a pipe writer to report write_closed after the reader closes"))

let test_normal_readiness_events_are_not_error_events = fun _ctx ->
  with_pipe
    (fun read_end write_end ->
      with_poll
        (fun poll ->
          let token = Kernel.Async.Token.make 13 in
          let source = Kernel.Fs.File.to_source read_end in
          let payload = Kernel.Bytes.from_string "x" in
          let* () = lift_async
            (Kernel.Async.Poll.register poll token Kernel.Async.Interest.readable source) in
          let* _ = lift_file (Kernel.Fs.File.write write_end payload) in
          let* events = lift_async (Kernel.Async.Poll.poll ~timeout:100_000_000L poll) in
          if
            List.any
              events
              ~fn:(fun event ->
                Kernel.Async.Token.equal token (Kernel.Async.Event.token event)
                && Kernel.Async.Event.is_readable event
                && not (Kernel.Async.Event.is_error event))
          then
            Ok ()
          else
            Error "expected ordinary readiness events to stay out of the error bucket"))

let test_closed_poller_rejects_later_operations = fun _ctx ->
  with_pipe
    (fun read_end _write_end ->
      let source = Kernel.Fs.File.to_source read_end in
      let token = Kernel.Async.Token.make 14 in
      let expect_bad_fd = function
        | Kernel.Result.Error (Kernel.Async.System Kernel.SystemError.BadFileDescriptor) -> true
        | _ -> false
      in
      let* poll = lift_async (Kernel.Async.Poll.make ()) in
      let* () = lift_async (Kernel.Async.Poll.close poll) in
      if
        expect_bad_fd (Kernel.Async.Poll.poll ~timeout:0L poll)
        && expect_bad_fd
          (Kernel.Async.Poll.register poll token Kernel.Async.Interest.readable source)
        && expect_bad_fd
          (Kernel.Async.Poll.reregister poll token Kernel.Async.Interest.readable source)
        && expect_bad_fd (Kernel.Async.Poll.deregister poll source)
      then
        Ok ()
      else
        Error "expected a closed poller to reject later operations with a typed bad-fd error")

let tests = [
  Test.case "Async poll reports pipe readability" test_poll_reports_pipe_readability;
  Test.case "Async poll reports pipe read closure" test_poll_reports_pipe_read_closed;
  Test.case "Async deregister removes pipe source" test_deregister_removes_pipe_source;
  Test.case "Async reregister updates pipe token" test_reregister_updates_pipe_token;
  Test.case "Async reregister replaces writable interest" test_reregister_replaces_interest;
  Test.case "Async duplicate register updates the source token" test_registering_same_source_twice_updates_token;
  Test.case "Async deregister of a never-registered source is harmless" test_deregister_of_never_registered_source_is_harmless;
  Test.case "Async poll handles many pipe sources" test_poll_handles_many_pipe_sources;
  Test.case "Async token roundtrips structured values" test_token_roundtrips_structured_values;
  Test.case "Async poll rejects invalid limits" test_poll_rejects_invalid_limits;
  Test.case "Async.Interest add and remove roundtrip" test_interest_add_and_remove_roundtrip;
  Test.case "Async.Interest remove-all returns None" test_interest_remove_all_bits_returns_none;
  Test.case "Async.Token.id is stable" test_token_id_is_stable;
  Test.case "A fresh Poll.poll timeout=0 is quiet" test_fresh_poll_timeout_zero_is_quiet;
  Test.case "Poll max_events=1 batches readiness across polls" test_poll_max_events_batches_without_dropping_readiness;
  Test.case "Duplicate register with the same token does not duplicate one readiness event" test_duplicate_register_same_token_does_not_duplicate_event;
  Test.case "A pipe writer reports write_closed after the reader closes" test_pipe_writer_reports_write_closed_after_reader_closes;
  Test.case "Normal readiness events are not error events" test_normal_readiness_events_are_not_error_events;
  Test.case "A closed poller rejects later operations consistently" test_closed_poller_rejects_later_operations;
  Test.case "Async poll handles mixed pipe, timer, udp, and process sources" test_poll_handles_mixed_source_types;
  Test.case "Async poll tolerates closed registered pipe sources" test_poll_tolerates_closed_registered_pipe_sources;
  Test.case ~size:Test.Large "Async poll handles many timer sources" test_poll_handles_many_timer_sources;
  Test.case ~size:Test.Large "Async poll handles many process exits" test_poll_handles_many_process_exits;
  Test.case ~size:Test.Large "Async repeated register and deregister stays healthy" test_repeated_register_and_deregister_stays_healthy;
  Test.case ~size:Test.Large "Async repeated register, reregister, and deregister stays healthy" test_repeated_register_reregister_and_deregister_stays_healthy;
]

let main = fun ~args -> Test.Cli.main ~name:"kernel_new_async_tests" ~tests ~args

let () = Actors.run ~main ~args:Env.args ()
