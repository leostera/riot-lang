open Std
module Test = Std.Test
module Kernel = Kernel_new

let ( let* ) = Result.and_then

let lift_file = function
  | Kernel.Result.Ok value -> Ok value
  | Kernel.Result.Error error -> Error (Kernel.Fs.File.error_to_string error)

let lift_async = function
  | Kernel.Result.Ok value -> Ok value
  | Kernel.Result.Error error -> Error (Kernel.Async.error_to_string error)

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

let rec close_pipes = function
  | [] -> ()
  | (read_end, write_end) :: rest ->
      let _ = Kernel.Fs.File.close read_end in
      let _ = Kernel.Fs.File.close write_end in
      close_pipes rest

let rec close_processes = function
  | [] -> ()
  | process :: rest ->
      let _ = Kernel.Process.close process in
      close_processes rest

let with_pipes = fun count fn ->
  let rec create remaining acc =
    if remaining = 0 then
      Ok acc
    else
      let* pipe = lift_file (Kernel.Fs.File.pipe ()) in
      create (remaining - 1) ((pipe.read_end, pipe.write_end) :: acc)
  in
  let* pipes = create count [] in
  protect ~finally:(fun () -> close_pipes pipes) (fun () -> fn (List.rev pipes))

let with_poll = fun fn ->
  let* poll = lift_async (Kernel.Async.Poll.make ()) in
  protect
    ~finally:(fun () ->
      let _ = Kernel.Async.Poll.close poll in
      ())
    (fun () -> fn poll)

let with_processes = fun count fn ->
  let stdio = Kernel.Process.{ stdin = `Null; stdout = `Null; stderr = `Null } in
  let rec spawn remaining acc =
    if remaining = 0 then
      Ok acc
    else
      match Kernel.Process.spawn ~program:"/bin/sh" ~args:[|"-c"; "sleep 0.05"|] ~stdio () with
      | Kernel.Result.Ok process -> spawn (remaining - 1) (process :: acc)
      | Kernel.Result.Error error -> Error (Kernel.Process.error_to_string error)
  in
  let* processes = spawn count [] in
  protect ~finally:(fun () -> close_processes processes) (fun () -> fn (List.rev processes))

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
          let payload = Kernel.Bytes.of_string "x" in
          let* written = lift_file (Kernel.Fs.File.write write_end payload) in
          if written != 1 then
            Error "expected pipe write to write one byte"
          else
            let* events = lift_async (Kernel.Async.Poll.poll ~timeout:100_000_000L poll) in
            let found =
              List.exists
                (fun event ->
                  Kernel.Async.Event.is_readable event
                  && Kernel.Async.Token.equal token (Kernel.Async.Event.token event))
                events
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
            List.exists
              (fun event ->
                Kernel.Async.Event.is_read_closed event
                && Kernel.Async.Token.equal token (Kernel.Async.Event.token event))
              events
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
          let payload = Kernel.Bytes.of_string "x" in
          let* written = lift_file (Kernel.Fs.File.write write_end payload) in
          if written != 1 then
            Error "expected pipe write to write one byte"
          else
            let* events = lift_async (Kernel.Async.Poll.poll ~timeout:0L poll) in
            let found =
              List.exists
                (fun event ->
                  Kernel.Async.Event.is_readable event
                  && Kernel.Async.Token.equal token (Kernel.Async.Event.token event))
                events
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
            List.exists
              (fun event ->
                Kernel.Async.Event.is_writable event
                && Kernel.String.equal
                  (Kernel.Async.Token.unsafe_to_value (Kernel.Async.Event.token event))
                  "second")
              events
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
            List.exists
              (fun event ->
                Kernel.Async.Event.is_writable event
                && Kernel.Async.Token.equal token (Kernel.Async.Event.token event))
              events
          in
          if found then
            Error "expected replaced writable interest to stop producing events"
          else
            Ok ()))

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
                  (Kernel.Fs.File.write write_end (Kernel.Bytes.of_string "x")) in
                if written != 1 then
                  Error "expected pipe write to write one byte"
                else
                  write_all rest
          in
          let* () = register 0 pipes in
          let* () = write_all pipes in
          let* events = lift_async
            (Kernel.Async.Poll.poll ~timeout:100_000_000L ~max_events:128 poll) in
          let seen = Kernel.Array.make 64 false in
          let rec mark = function
            | [] -> ()
            | event :: rest ->
                if Kernel.Async.Event.is_readable event then
                  let token = Kernel.Async.Token.unsafe_to_value (Kernel.Async.Event.token event) in
                  if token >= 0 && token < 64 then
                    Kernel.Array.set seen token true;
                  mark rest
          in
          mark events;
          let rec all_seen index =
            if index = 64 then
              true
            else if Kernel.Array.get seen index then
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
  let tag, value = Kernel.Async.Token.unsafe_to_value token in
  if Kernel.String.equal tag "pipe" && value = 99 then
    Ok ()
  else
    Error "expected token to roundtrip its structured value"

let test_poll_rejects_invalid_limits = fun _ctx ->
  with_poll
    (fun poll ->
      match (Kernel.Async.Poll.poll ~timeout:(-1L) poll, Kernel.Async.Poll.poll ~max_events:0 poll) with
      | (Kernel.Result.Error (Kernel.Async.Invalid_timeout_ns { timeout_ns }), Kernel.Result.Error (Kernel.Async.Invalid_max_events {
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
          let seen = Kernel.Array.make 16 false in
          let rec mark_events = function
            | [] -> ()
            | event :: rest ->
                if Kernel.Async.Event.is_priority event then
                  let token = Kernel.Async.Token.unsafe_to_value (Kernel.Async.Event.token event) in
                  if token >= 0 && token < 16 then
                    Kernel.Array.set seen token true;
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
                let () =
                  match status with
                  | Some (Kernel.Process.Exited 0) ->
                      if index < 16 then
                        Kernel.Array.set seen index true
                  | _ -> ()
                in
                mark_exits (index + 1) rest
          in
          let rec all_seen index =
            if index = 16 then
              true
            else if Kernel.Array.get seen index then
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
              let () = mark_events events in
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
      let timers = List.rev timers in
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
      let seen = Kernel.Array.make 16 false in
      let rec mark = function
        | [] -> ()
        | event :: rest ->
            if Kernel.Async.Event.is_readable event then
              let token = Kernel.Async.Token.unsafe_to_value (Kernel.Async.Event.token event) in
              if token >= 0 && token < 16 then
                Kernel.Array.set seen token true;
              mark rest
      in
      let rec all_seen index =
        if index = 16 then
          true
        else if Kernel.Array.get seen index then
          all_seen (index + 1)
        else
          false
      in
      let* () = register 0 timers in
      let rec poll_until attempts =
        if attempts = 0 then
          Error "expected many timer sources to wake the poller"
        else
          let* events = lift_async
            (Kernel.Async.Poll.poll ~timeout:100_000_000L ~max_events:32 poll) in
          let () = mark events in
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

let tests = [
  Test.case "Async poll reports pipe readability" test_poll_reports_pipe_readability;
  Test.case "Async poll reports pipe read closure" test_poll_reports_pipe_read_closed;
  Test.case "Async deregister removes pipe source" test_deregister_removes_pipe_source;
  Test.case "Async reregister updates pipe token" test_reregister_updates_pipe_token;
  Test.case "Async reregister replaces writable interest" test_reregister_replaces_interest;
  Test.case "Async poll handles many pipe sources" test_poll_handles_many_pipe_sources;
  Test.case "Async token roundtrips structured values" test_token_roundtrips_structured_values;
  Test.case "Async poll rejects invalid limits" test_poll_rejects_invalid_limits;
  Test.case ~size:Test.Large "Async poll handles many timer sources" test_poll_handles_many_timer_sources;
  Test.case ~size:Test.Large "Async poll handles many process exits" test_poll_handles_many_process_exits;
  Test.case ~size:Test.Large "Async repeated register and deregister stays healthy" test_repeated_register_and_deregister_stays_healthy;
]

let main = fun ~args -> Test.Cli.main ~name:"kernel_new_async_tests" ~tests ~args

let () = Actors.run ~main ~args:Env.args ()
