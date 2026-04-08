open Std
module Test = Std.Test
module Kernel = Kernel_new

let ( let* ) = Result.and_then

let lift = function
  | Kernel.Result.Ok value -> Ok value
  | Kernel.Result.Error error -> Error (Kernel.Error.to_string error)

let is_would_block = function
  | Kernel.Error.Would_block -> true
  | _ -> false

let protect = fun ~finally fn ->
  try
    let value = fn () in
    finally ();
    value
  with
  | error ->
      finally ();
      raise error

let with_process = fun process fn ->
  protect
    ~finally:(fun () ->
      let _ = Kernel.Process.close process in
      ())
    (fun () -> fn process)

let with_poll = fun fn ->
  let* poll = lift (Kernel.Async.Poll.make ()) in
  fn poll

let wait_for = fun poll ~token ~interest ~source ~pred ->
  let* () =
    lift (Kernel.Async.Poll.register poll token interest source)
  in
  protect
    ~finally:(fun () ->
      let _ = Kernel.Async.Poll.deregister poll source in
      ())
    (fun () ->
      let* events =
        lift (Kernel.Async.Poll.poll ~timeout:100_000_000L poll)
      in
      let found =
        List.exists
          (fun event ->
            Kernel.Async.Token.equal token (Kernel.Async.Event.token event)
            && pred event)
          events
      in
      if found then
        Ok ()
      else
        Error "expected readiness event")

let wait_readable = fun poll ~token source ->
  wait_for
    poll
    ~token
    ~interest:Kernel.Async.Interest.readable
    ~source
    ~pred:Kernel.Async.Event.is_readable

let wait_writable = fun poll ~token source ->
  wait_for
    poll
    ~token
    ~interest:Kernel.Async.Interest.writable
    ~source
    ~pred:Kernel.Async.Event.is_writable

let read_once = fun poll ~token file ->
  let buffer = Kernel.Bytes.create 128 in
  let rec loop = fun () ->
    match Kernel.Fs.File.read file buffer with
    | Kernel.Result.Ok count ->
        Ok (Kernel.Bytes.sub_string buffer 0 count)
    | Kernel.Result.Error error ->
        if is_would_block error then
          let* () =
            wait_readable poll ~token (Kernel.Fs.File.to_source file)
          in
          loop ()
        else
          Error (Kernel.Error.to_string error)
  in
  loop ()

let write_all = fun poll ~token file buffer ->
  let rec loop = fun pos len ->
    if len = 0 then
      Ok ()
    else
      match Kernel.Fs.File.write file ~pos ~len buffer with
      | Kernel.Result.Ok written ->
          if written <= 0 then
            Error "expected process pipe write to make progress"
          else
            loop (pos + written) (len - written)
      | Kernel.Result.Error error ->
          if is_would_block error then
            let* () =
              wait_writable poll ~token (Kernel.Fs.File.to_source file)
            in
            loop pos len
          else
            Error (Kernel.Error.to_string error)
  in
  loop 0 (Kernel.Bytes.length buffer)

let test_current_pid_is_positive = fun _ctx ->
  if Kernel.Process.current_pid () > 0 then
    Ok ()
  else
    Error "expected current_pid to be positive"

let test_stdout_pipe_roundtrips = fun _ctx ->
  let stdio = Kernel.Process.{
    default_stdio with
    stdin = `Null;
    stdout = `Pipe;
    stderr = `Null;
  } in
  let* process =
    lift
      (Kernel.Process.spawn
         ~program:"/bin/echo"
         ~args:[| "-n"; "hello" |]
         ~stdio
         ())
  in
  with_process process
    (fun process ->
      match Kernel.Process.stdout process with
      | None -> Error "expected stdout pipe"
      | Some stdout ->
          with_poll
            (fun poll ->
              let* payload =
                read_once poll ~token:(Kernel.Async.Token.make 501) stdout
              in
              let* status = lift (Kernel.Process.wait process) in
              if payload = "hello" && status = Kernel.Process.Exited 0 then
                Ok ()
              else
                Error "expected echo stdout payload and zero exit status"))

let test_stdin_and_stdout_pipes_roundtrip = fun _ctx ->
  let stdio = Kernel.Process.{
    default_stdio with
    stdin = `Pipe;
    stdout = `Pipe;
    stderr = `Null;
  } in
  let* process =
    lift
      (Kernel.Process.spawn
         ~program:"/bin/cat"
         ~args:[||]
         ~stdio
         ())
  in
  with_process process
    (fun process ->
      match (Kernel.Process.stdin process, Kernel.Process.stdout process) with
      | (Some stdin, Some stdout) ->
          with_poll
            (fun poll ->
              let payload = Kernel.Bytes.of_string "ping" in
              let* () =
                write_all poll ~token:(Kernel.Async.Token.make 502) stdin payload
              in
              let* () =
                lift (Kernel.Fs.File.close stdin)
              in
              let* echoed =
                read_once poll ~token:(Kernel.Async.Token.make 503) stdout
              in
              let* status = lift (Kernel.Process.wait process) in
              if echoed = "ping" && status = Kernel.Process.Exited 0 then
                Ok ()
              else
                Error "expected cat to echo stdin and exit cleanly")
      | _ ->
          Error "expected stdin and stdout pipes")

let test_stderr_redirect_to_stdout_merges_streams = fun _ctx ->
  let stdio = Kernel.Process.{
    default_stdio with
    stdin = `Null;
    stdout = `Pipe;
    stderr = `Redirect_to_stdout;
  } in
  let* process =
    lift
      (Kernel.Process.spawn
         ~program:"/bin/sh"
         ~args:[| "-c"; "printf out; printf err >&2" |]
         ~stdio
         ())
  in
  with_process process
    (fun process ->
      match Kernel.Process.stdout process with
      | None -> Error "expected stdout pipe"
      | Some stdout ->
          with_poll
            (fun poll ->
              let* payload =
                read_once poll ~token:(Kernel.Async.Token.make 504) stdout
              in
              let* status = lift (Kernel.Process.wait process) in
              if payload = "outerr" && status = Kernel.Process.Exited 0 then
                Ok ()
              else
                Error "expected redirected stderr to be merged into stdout"))

let test_try_wait_reports_running_then_exit = fun _ctx ->
  let stdio = Kernel.Process.{
    default_stdio with
    stdin = `Null;
    stdout = `Null;
    stderr = `Null;
  } in
  let* process =
    lift
      (Kernel.Process.spawn
         ~program:"/bin/sh"
         ~args:[| "-c"; "sleep 0.05" |]
         ~stdio
         ())
  in
  with_process process
    (fun process ->
      let* status =
        lift (Kernel.Process.try_wait process)
      in
      match status with
      | Some _ ->
          Error "expected process to still be running on immediate try_wait"
      | None ->
          let* status = lift (Kernel.Process.wait process) in
          if status = Kernel.Process.Exited 0 then
            Ok ()
          else
            Error "expected process to exit cleanly after wait")

let test_kill_reports_signaled_status = fun _ctx ->
  let stdio = Kernel.Process.{
    default_stdio with
    stdin = `Null;
    stdout = `Null;
    stderr = `Null;
  } in
  let* process =
    lift
      (Kernel.Process.spawn
         ~program:"/bin/sh"
         ~args:[| "-c"; "sleep 5" |]
         ~stdio
         ())
  in
  with_process process
    (fun process ->
      let* () = lift (Kernel.Process.kill process ~signal:9) in
      let* status = lift (Kernel.Process.wait process) in
      if status = Kernel.Process.Signaled 9 then
        Ok ()
      else
        Error "expected killed process to report a signaled status")

let tests = [
  Test.case "Process current_pid is positive" test_current_pid_is_positive;
  Test.case "Process stdout pipe roundtrips" test_stdout_pipe_roundtrips;
  Test.case "Process stdin and stdout pipes roundtrip" test_stdin_and_stdout_pipes_roundtrip;
  Test.case "Process stderr redirect_to_stdout merges streams" test_stderr_redirect_to_stdout_merges_streams;
  Test.case "Process try_wait reports running then exit" test_try_wait_reports_running_then_exit;
  Test.case "Process kill reports signaled status" test_kill_reports_signaled_status;
]

let main = fun ~args ->
  Test.Cli.main ~name:"kernel_new_process_tests" ~tests ~args

let () = Actors.run ~main ~args:Env.args ()
