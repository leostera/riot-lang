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

let with_tempdir = fun prefix fn ->
  match Fs.with_tempdir ~prefix (fun tempdir -> fn (Kernel.Path.v (Path.to_string tempdir))) with
  | Ok result -> result
  | Error err -> Error (IO.error_message err)

let with_process = fun process fn ->
  protect
    ~finally:(fun () ->
      let _ = Kernel.Process.close process in
      ())
    (fun () -> fn process)

let with_file = fun file fn ->
  protect
    ~finally:(fun () ->
      let _ = Kernel.Fs.File.close file in
      ())
    fn

let with_poll = fun fn ->
  let* poll = lift (Kernel.Async.Poll.make ()) in
  protect
    ~finally:(fun () ->
      let _ = Kernel.Async.Poll.close poll in
      ())
    (fun () -> fn poll)

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

let read_all = fun poll ~token file ->
  let buffer = Kernel.Bytes.create 128 in
  let rec loop parts =
    match Kernel.Fs.File.read file buffer with
    | Kernel.Result.Ok 0 ->
        Ok (Kernel.String.concat "" (List.rev parts))
    | Kernel.Result.Ok count ->
        loop (Kernel.Bytes.sub_string buffer 0 count :: parts)
    | Kernel.Result.Error error ->
        if is_would_block error then
          let* () =
            wait_readable poll ~token (Kernel.Fs.File.to_source file)
          in
          loop parts
        else
          Error (Kernel.Error.to_string error)
  in
  loop []

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
              let* status = lift (Kernel.Process.wait process) in
              let* payload =
                read_all poll ~token:(Kernel.Async.Token.make 501) stdout
              in
              if payload = "hello" && status = Kernel.Process.Exited 0 then
                Ok ()
              else
                Error "expected echo stdout payload and zero exit status"))

let test_stdin_and_stdout_pipes_roundtrip = fun _ctx ->
  let stdio = Kernel.Process.{
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
              let* status = lift (Kernel.Process.wait process) in
              let* payload =
                read_all poll ~token:(Kernel.Async.Token.make 504) stdout
              in
              if payload = "outerr" && status = Kernel.Process.Exited 0 then
                Ok ()
              else
                Error "expected redirected stderr to be merged into stdout"))

let test_try_wait_reports_running_then_exit = fun _ctx ->
  let stdio = Kernel.Process.{
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

let test_sigterm_reports_signaled_status = fun _ctx ->
  let stdio = Kernel.Process.{
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
      let* () = lift (Kernel.Process.kill process ~signal:15) in
      let* status = lift (Kernel.Process.wait process) in
      if status = Kernel.Process.Signaled 15 then
        Ok ()
      else
        Error "expected sigterm to report the delivered signal number")

let test_non_zero_exit_status_roundtrips = fun _ctx ->
  let stdio = Kernel.Process.{
    stdin = `Null;
    stdout = `Null;
    stderr = `Null;
  } in
  let* process =
    lift
      (Kernel.Process.spawn
         ~program:"/bin/sh"
         ~args:[| "-c"; "exit 7" |]
         ~stdio
         ())
  in
  with_process process
    (fun process ->
      let* status = lift (Kernel.Process.wait process) in
      if status = Kernel.Process.Exited 7 then
        Ok ()
      else
        Error "expected process exit status to preserve a non-zero code")

let test_spawn_applies_custom_environment = fun _ctx ->
  let stdio = Kernel.Process.{
    stdin = `Null;
    stdout = `Pipe;
    stderr = `Null;
  } in
  let* process =
    lift
      (Kernel.Process.spawn
         ~program:"/bin/sh"
         ~args:[| "-c"; "printf %s \"$KERNEL_NEW_PROCESS_TEST\"" |]
         ~env:[|("KERNEL_NEW_PROCESS_TEST", "env-ok")|]
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
              let* status = lift (Kernel.Process.wait process) in
              let* payload =
                read_all poll ~token:(Kernel.Async.Token.make 505) stdout
              in
              if payload = "env-ok" && status = Kernel.Process.Exited 0 then
                Ok ()
              else
                Error "expected spawned process to see custom environment"))

let test_spawn_applies_current_dir = fun _ctx ->
  with_tempdir "kernel_new_process"
    (fun tempdir ->
      let stdio = Kernel.Process.{
        stdin = `Null;
        stdout = `Pipe;
        stderr = `Null;
      } in
      let* process =
        lift
          (Kernel.Process.spawn
             ~program:"/bin/pwd"
             ~args:[||]
             ~current_dir:tempdir
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
                  let* status = lift (Kernel.Process.wait process) in
                  let* payload =
                    read_all poll ~token:(Kernel.Async.Token.make 506) stdout
                  in
                  let output =
                    if String.length payload != 0 && String.get payload (String.length payload - 1) = '\n' then
                      String.sub payload 0 (String.length payload - 1)
                    else
                      payload
                  in
                  let* expected =
                    lift (Kernel.Fs.File.canonicalize tempdir)
                  in
                  let* actual =
                    lift (Kernel.Fs.File.canonicalize (Kernel.Path.v output))
                  in
                  if Kernel.Path.to_string actual = Kernel.Path.to_string expected
                     && status = Kernel.Process.Exited 0
                  then
                    Ok ()
                  else
                    Error "expected spawned process to run in the configured current_dir")))

let test_file_backed_stdio_roundtrips = fun _ctx ->
  with_tempdir "kernel_new_process"
    (fun tempdir ->
      let input_path = Kernel.Path.(tempdir / "stdin.txt") in
      let output_path = Kernel.Path.(tempdir / "stdout.txt") in
      let* input_file = lift (Kernel.Fs.File.open_write input_path) in
      let* () =
        with_file input_file
          (fun () ->
            let* _ = lift (Kernel.Fs.File.write input_file (Kernel.Bytes.of_string "file-stdio")) in
            Ok ())
      in
      let* stdin_file = lift (Kernel.Fs.File.open_read input_path) in
      protect
        ~finally:(fun () ->
          let _ = Kernel.Fs.File.close stdin_file in
          ())
        (fun () ->
          let* stdout_file =
            lift (Kernel.Fs.File.open_write ~create:true ~truncate:true output_path)
          in
          protect
            ~finally:(fun () ->
              let _ = Kernel.Fs.File.close stdout_file in
              ())
            (fun () ->
              let stdio = Kernel.Process.{
                stdin = `File stdin_file;
                stdout = `File stdout_file;
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
              let* status =
                with_process process (fun process -> lift (Kernel.Process.wait process))
              in
              let* output = lift (Kernel.Fs.File.open_read output_path) in
              let buffer = Kernel.Bytes.create 32 in
              let* payload =
                with_file output
                  (fun () ->
                    let* count = lift (Kernel.Fs.File.read output buffer) in
                    Ok (Kernel.Bytes.sub_string buffer 0 count))
              in
              if status = Kernel.Process.Exited 0 && payload = "file-stdio" then
                Ok ()
              else
                Error "expected file-backed stdio to roundtrip through the child process")))

let tests = [
  Test.case "Process current_pid is positive" test_current_pid_is_positive;
  Test.case "Process stdout pipe roundtrips" test_stdout_pipe_roundtrips;
  Test.case "Process stdin and stdout pipes roundtrip" test_stdin_and_stdout_pipes_roundtrip;
  Test.case "Process stderr redirect_to_stdout merges streams" test_stderr_redirect_to_stdout_merges_streams;
  Test.case "Process try_wait reports running then exit" test_try_wait_reports_running_then_exit;
  Test.case "Process kill reports signaled status" test_kill_reports_signaled_status;
  Test.case "Process sigterm reports signaled status" test_sigterm_reports_signaled_status;
  Test.case "Process preserves non-zero exit status" test_non_zero_exit_status_roundtrips;
  Test.case "Process spawn applies custom environment" test_spawn_applies_custom_environment;
  Test.case "Process spawn applies current_dir" test_spawn_applies_current_dir;
  Test.case "Process file-backed stdio roundtrips" test_file_backed_stdio_roundtrips;
]

let main = fun ~args ->
  Test.Cli.main ~name:"kernel_new_process_tests" ~tests ~args

let () = Actors.run ~main ~args:Env.args ()
