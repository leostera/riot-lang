open Std
module Test = Std.Test
module Kernel = Kernel_new

let ( let* ) = Result.and_then

let lift_process result =
  match result with
  | Kernel.Result.Ok value -> Ok value
  | Kernel.Result.Error error -> Error (Kernel.Process.error_to_string error)

let lift_async result =
  match result with
  | Kernel.Result.Ok value -> Ok value
  | Kernel.Result.Error error -> Error (Kernel.Async.error_to_string error)

let lift_file result =
  match result with
  | Kernel.Result.Ok value -> Ok value
  | Kernel.Result.Error error -> Error (Kernel.Fs.File.error_to_string error)

let is_would_block error =
  match error with
  | Kernel.Fs.File.System system_error -> Kernel.SystemError.is_would_block system_error
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
  match Fs.with_tempdir ~prefix (fun tempdir -> fn (Kernel.Path.of_string (Path.to_string tempdir))) with
  | Ok result -> result
  | Error err -> Error (IO.error_message err)

let with_process = fun process fn ->
  protect
    ~finally:(fun () ->
      let _ = Kernel.Process.close process in
      ())
    (fun () -> fn process)

let rec close_processes = fun processes ->
  match processes with
  | [] -> ()
  | process :: rest ->
      let _ = Kernel.Process.close process in
      close_processes rest

let with_file = fun file fn ->
  protect
    ~finally:(fun () ->
      let _ = Kernel.Fs.File.close file in
      ())
    fn

let with_poll = fun fn ->
  let* poll = lift_async (Kernel.Async.Poll.make ()) in
  protect
    ~finally:(fun () ->
      let _ = Kernel.Async.Poll.close poll in
      ())
    (fun () -> fn poll)

let wait_for = fun poll ~token ~interest ~source ~pred ->
  let* () = lift_async (Kernel.Async.Poll.register poll token interest source) in
  protect
    ~finally:(fun () ->
      let _ = Kernel.Async.Poll.deregister poll source in
      ())
    (fun () ->
      let* events = lift_async (Kernel.Async.Poll.poll ~timeout:100_000_000L poll) in
      let found =
        List.exists
          (fun event -> Kernel.Async.Token.equal token (Kernel.Async.Event.token event) && pred event)
          events
      in
      if found then
        Ok ()
      else
        Error "expected readiness event")

let wait_readable = fun poll ~token source ->
  wait_for poll ~token ~interest:Kernel.Async.Interest.readable ~source ~pred:Kernel.Async.Event.is_readable

let wait_writable = fun poll ~token source ->
  wait_for poll ~token ~interest:Kernel.Async.Interest.writable ~source ~pred:Kernel.Async.Event.is_writable

let read_once = fun poll ~token file ->
  let buffer = Kernel.Bytes.create 128 in
  let rec loop () =
    match Kernel.Fs.File.read file buffer with
    | Kernel.Result.Ok count -> Ok (Kernel.Bytes.sub_string buffer 0 count)
    | Kernel.Result.Error error ->
        if is_would_block error then
          let* () = wait_readable poll ~token (Kernel.Fs.File.to_source file) in
          loop ()
        else
          Error (Kernel.Fs.File.error_to_string error)
  in
  loop ()

let read_all = fun poll ~token file ->
  let buffer = Kernel.Bytes.create 128 in
  let rec loop parts =
    match Kernel.Fs.File.read file buffer with
    | Kernel.Result.Ok 0 -> Ok (Kernel.String.concat "" (List.rev parts))
    | Kernel.Result.Ok count -> loop (Kernel.Bytes.sub_string buffer 0 count :: parts)
    | Kernel.Result.Error error ->
        if is_would_block error then
          let* () = wait_readable poll ~token (Kernel.Fs.File.to_source file) in
          loop parts
        else
          Error (Kernel.Fs.File.error_to_string error)
  in
  loop []

let write_all = fun poll ~token file buffer ->
  let rec loop pos len =
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
            let* () = wait_writable poll ~token (Kernel.Fs.File.to_source file) in
            loop pos len
          else
            Error (Kernel.Fs.File.error_to_string error)
  in
  loop 0 (Kernel.Bytes.length buffer)

let wait_for_exit = fun poll ~token process ->
  let exit_poll_timeout = 1_000_000L in
  let rec loop () =
    let* status = lift_process (Kernel.Process.try_wait process) in
    match status with
    | Some status -> Ok status
    | None ->
        let source = Kernel.Process.to_source process in
        let* () = lift_async
          (Kernel.Async.Poll.register poll token Kernel.Async.Interest.priority source) in
        protect
          ~finally:(fun () ->
            let _ = Kernel.Async.Poll.deregister poll source in
            ())
          (fun () ->
            let* _ = lift_async (Kernel.Async.Poll.poll ~timeout:exit_poll_timeout poll) in
            loop ())
  in
  loop ()

let wait_for_priority_token = fun poll ~token ->
  let rec loop attempts =
    if attempts = 0 then
      Error "expected process source to report priority readiness"
    else
      let* events = lift_async (Kernel.Async.Poll.poll ~timeout:100_000_000L ~max_events:8 poll) in
      if
        List.exists
          (fun event ->
            Kernel.Async.Token.equal token (Kernel.Async.Event.token event)
            && Kernel.Async.Event.is_priority event)
          events
      then
        Ok ()
      else
        loop (attempts - 1)
  in
  loop 8

let test_current_pid_is_positive = fun _ctx ->
  if Kernel.Process.current_pid () > 0 then
    Ok ()
  else
    Error "expected current_pid to be positive"

let test_stdout_pipe_roundtrips = fun _ctx ->
  let stdio = Kernel.Process.{ stdin = Stdin.Null; stdout = Stdout.Pipe; stderr = Stderr.Null } in
  let* process = lift_process
    (Kernel.Process.spawn ~program:"/bin/echo" ~args:[|"-n"; "hello"|] ~stdio ()) in
  with_process process
    (fun process ->
      match Kernel.Process.stdout process with
      | None -> Error "expected stdout pipe"
      | Some stdout ->
          with_poll
            (fun poll ->
              let* status = wait_for_exit poll ~token:(Kernel.Async.Token.make 520) process in
              let* payload = read_all poll ~token:(Kernel.Async.Token.make 501) stdout in
              if payload = "hello" && status = Kernel.Process.Exited 0 then
                Ok ()
              else
                Error "expected echo stdout payload and zero exit status"))

let test_stdin_and_stdout_pipes_roundtrip = fun _ctx ->
  let stdio = Kernel.Process.{ stdin = Stdin.Pipe; stdout = Stdout.Pipe; stderr = Stderr.Null } in
  let* process = lift_process (Kernel.Process.spawn ~program:"/bin/cat" ~args:[||] ~stdio ()) in
  with_process process
    (fun process ->
      match (Kernel.Process.stdin process, Kernel.Process.stdout process) with
      | (Some stdin, Some stdout) ->
          with_poll
            (fun poll ->
              let payload = Kernel.Bytes.of_string "ping" in
              let* () = write_all poll ~token:(Kernel.Async.Token.make 502) stdin payload in
              let* () = lift_file (Kernel.Fs.File.close stdin) in
              let* echoed = read_once poll ~token:(Kernel.Async.Token.make 503) stdout in
              let* status = wait_for_exit poll ~token:(Kernel.Async.Token.make 521) process in
              if echoed = "ping" && status = Kernel.Process.Exited 0 then
                Ok ()
              else
                Error "expected cat to echo stdin and exit cleanly")
      | _ -> Error "expected stdin and stdout pipes")

let test_stderr_redirect_to_stdout_merges_streams = fun _ctx ->
  let stdio =
    Kernel.Process.{ stdin = Stdin.Null; stdout = Stdout.Pipe; stderr = Stderr.RedirectToStdout } in
  let* process = lift_process
    (Kernel.Process.spawn ~program:"/bin/sh" ~args:[|"-c"; "printf out; printf err >&2"|] ~stdio ()) in
  with_process process
    (fun process ->
      match Kernel.Process.stdout process with
      | None -> Error "expected stdout pipe"
      | Some stdout ->
          with_poll
            (fun poll ->
              let* status = wait_for_exit poll ~token:(Kernel.Async.Token.make 522) process in
              let* payload = read_all poll ~token:(Kernel.Async.Token.make 504) stdout in
              if payload = "outerr" && status = Kernel.Process.Exited 0 then
                Ok ()
              else
                Error "expected redirected stderr to be merged into stdout"))

let test_try_wait_reports_running_then_exit = fun _ctx ->
  let stdio = Kernel.Process.{ stdin = Stdin.Null; stdout = Stdout.Null; stderr = Stderr.Null } in
  let* process = lift_process
    (Kernel.Process.spawn ~program:"/bin/sh" ~args:[|"-c"; "sleep 0.05"|] ~stdio ()) in
  with_process process
    (fun process ->
      let* status = lift_process (Kernel.Process.try_wait process) in
      match status with
      | Some _ -> Error "expected process to still be running on immediate try_wait"
      | None ->
          with_poll
            (fun poll ->
              let* status = wait_for_exit poll ~token:(Kernel.Async.Token.make 523) process in
              if status = Kernel.Process.Exited 0 then
                Ok ()
              else
                Error "expected process to exit cleanly after async poll"))

let test_process_source_reports_exit_ready = fun _ctx ->
  let stdio = Kernel.Process.{ stdin = Stdin.Null; stdout = Stdout.Null; stderr = Stderr.Null } in
  let* process = lift_process
    (Kernel.Process.spawn ~program:"/bin/sh" ~args:[|"-c"; "sleep 0.01"|] ~stdio ()) in
  with_process process
    (fun process ->
      with_poll
        (fun poll ->
          let* status = wait_for_exit poll ~token:(Kernel.Async.Token.make 531) process in
          if status = Kernel.Process.Exited 0 then
            Ok ()
          else
            Error "expected async process source to report a clean exit"))

let test_process_source_reregister_updates_token = fun _ctx ->
  let stdio = Kernel.Process.{ stdin = Stdin.Null; stdout = Stdout.Null; stderr = Stderr.Null } in
  let* process = lift_process
    (Kernel.Process.spawn ~program:"/bin/sh" ~args:[|"-c"; "sleep 0.02"|] ~stdio ()) in
  with_process process
    (fun process ->
      with_poll
        (fun poll ->
          let source = Kernel.Process.to_source process in
          let token_a = Kernel.Async.Token.make "first-process-token" in
          let token_b = Kernel.Async.Token.make "second-process-token" in
          let* () = lift_async
            (Kernel.Async.Poll.register poll token_a Kernel.Async.Interest.priority source) in
          protect
            ~finally:(fun () ->
              let _ = Kernel.Async.Poll.deregister poll source in
              ())
            (fun () ->
              let* () = lift_async
                (Kernel.Async.Poll.reregister poll token_b Kernel.Async.Interest.priority source) in
              let* () = wait_for_priority_token poll ~token:token_b in
              match Kernel.Process.try_wait process with
              | Kernel.Result.Ok (Some (Kernel.Process.Exited 0)) -> Ok ()
              | Kernel.Result.Ok _ -> Error "expected reregistered process source to preserve exit readiness"
              | Kernel.Result.Error error -> Error (Kernel.Process.error_to_string error))))

let test_process_close_preserves_registered_exit_source = fun _ctx ->
  let stdio = Kernel.Process.{ stdin = Stdin.Null; stdout = Stdout.Pipe; stderr = Stderr.Null } in
  let* process = lift_process
    (Kernel.Process.spawn ~program:"/bin/sh" ~args:[|"-c"; "sleep 0.02"|] ~stdio ()) in
  with_process process
    (fun process ->
      with_poll
        (fun poll ->
          let source = Kernel.Process.to_source process in
          let token = Kernel.Async.Token.make "closed-process-source" in
          let* () = lift_async
            (Kernel.Async.Poll.register poll token Kernel.Async.Interest.priority source) in
          protect
            ~finally:(fun () ->
              let _ = Kernel.Async.Poll.deregister poll source in
              ())
            (fun () ->
              let* () = lift_process (Kernel.Process.close process) in
              let* () = wait_for_priority_token poll ~token in
              match Kernel.Process.try_wait process with
              | Kernel.Result.Ok (Some (Kernel.Process.Exited 0)) -> Ok ()
              | Kernel.Result.Ok _ -> Error "expected closed process handles to leave exit readiness intact"
              | Kernel.Result.Error error -> Error (Kernel.Process.error_to_string error))))

let test_process_source_deregister_before_close_is_harmless = fun _ctx ->
  let stdio = Kernel.Process.{ stdin = Stdin.Null; stdout = Stdout.Null; stderr = Stderr.Null } in
  let* process = lift_process
    (Kernel.Process.spawn ~program:"/bin/sh" ~args:[|"-c"; "sleep 0.02"|] ~stdio ()) in
  with_process process
    (fun process ->
      with_poll
        (fun poll ->
          let source = Kernel.Process.to_source process in
          let* () = lift_async
            (Kernel.Async.Poll.register
              poll
              (Kernel.Async.Token.make "process-deregister-then-close")
              Kernel.Async.Interest.priority
              source) in
          let* () = lift_async (Kernel.Async.Poll.deregister poll source) in
          let* () = lift_process (Kernel.Process.close process) in
          let* status = wait_for_exit poll ~token:(Kernel.Async.Token.make 533) process in
          if status = Kernel.Process.Exited 0 then
            Ok ()
          else
            Error "expected deregister-before-close process ownership to preserve exit observation"))

let test_try_wait_is_stable_after_priority_readiness = fun _ctx ->
  let stdio = Kernel.Process.{ stdin = Stdin.Null; stdout = Stdout.Null; stderr = Stderr.Null } in
  let* process = lift_process
    (Kernel.Process.spawn ~program:"/bin/sh" ~args:[|"-c"; "sleep 0.02"|] ~stdio ()) in
  with_process process
    (fun process ->
      with_poll
        (fun poll ->
          let source = Kernel.Process.to_source process in
          let token = Kernel.Async.Token.make "process-priority-then-try-wait" in
          let* () = lift_async
            (Kernel.Async.Poll.register poll token Kernel.Async.Interest.priority source) in
          protect
            ~finally:(fun () ->
              let _ = Kernel.Async.Poll.deregister poll source in
              ())
            (fun () ->
              let* () = wait_for_priority_token poll ~token in
              let* first = lift_process (Kernel.Process.try_wait process) in
              let* second = lift_process (Kernel.Process.try_wait process) in
              match (first, second) with
              | (Some (Kernel.Process.Exited 0), Some (Kernel.Process.Exited 0)) -> Ok ()
              | _ -> Error "expected try_wait to stay stable after process priority readiness")))

let test_kill_reports_signaled_status = fun _ctx ->
  let stdio = Kernel.Process.{ stdin = Stdin.Null; stdout = Stdout.Null; stderr = Stderr.Null } in
  let* process = lift_process
    (Kernel.Process.spawn ~program:"/bin/sh" ~args:[|"-c"; "sleep 5"|] ~stdio ()) in
  with_process process
    (fun process ->
      with_poll
        (fun poll ->
          let* () = lift_process (Kernel.Process.kill process ~signal:9) in
          let* status = wait_for_exit poll ~token:(Kernel.Async.Token.make 524) process in
          if status = Kernel.Process.Signaled 9 then
            Ok ()
          else
            Error "expected killed process to report a signaled status"))

let test_sigterm_reports_signaled_status = fun _ctx ->
  let stdio = Kernel.Process.{ stdin = Stdin.Null; stdout = Stdout.Null; stderr = Stderr.Null } in
  let* process = lift_process
    (Kernel.Process.spawn ~program:"/bin/sh" ~args:[|"-c"; "sleep 5"|] ~stdio ()) in
  with_process process
    (fun process ->
      with_poll
        (fun poll ->
          let* () = lift_process (Kernel.Process.kill process ~signal:15) in
          let* status = wait_for_exit poll ~token:(Kernel.Async.Token.make 525) process in
          if status = Kernel.Process.Signaled 15 then
            Ok ()
          else
            Error "expected sigterm to report the delivered signal number"))

let test_non_zero_exit_status_roundtrips = fun _ctx ->
  let stdio = Kernel.Process.{ stdin = Stdin.Null; stdout = Stdout.Null; stderr = Stderr.Null } in
  let* process = lift_process
    (Kernel.Process.spawn ~program:"/bin/sh" ~args:[|"-c"; "exit 7"|] ~stdio ()) in
  with_process process
    (fun process ->
      with_poll
        (fun poll ->
          let* status = wait_for_exit poll ~token:(Kernel.Async.Token.make 526) process in
          if status = Kernel.Process.Exited 7 then
            Ok ()
          else
            Error "expected process exit status to preserve a non-zero code"))

let test_spawn_applies_custom_environment = fun _ctx ->
  let stdio = Kernel.Process.{ stdin = Stdin.Null; stdout = Stdout.Pipe; stderr = Stderr.Null } in
  let* process = lift_process
    (Kernel.Process.spawn
      ~program:"/bin/sh"
      ~args:[|"-c"; "printf %s \"$KERNEL_NEW_PROCESS_TEST\""|]
      ~env:[|("KERNEL_NEW_PROCESS_TEST", "env-ok")|]
      ~stdio
      ()) in
  with_process process
    (fun process ->
      match Kernel.Process.stdout process with
      | None -> Error "expected stdout pipe"
      | Some stdout ->
          with_poll
            (fun poll ->
              let* status = wait_for_exit poll ~token:(Kernel.Async.Token.make 527) process in
              let* payload = read_all poll ~token:(Kernel.Async.Token.make 505) stdout in
              if payload = "env-ok" && status = Kernel.Process.Exited 0 then
                Ok ()
              else
                Error "expected spawned process to see custom environment"))

let test_spawn_applies_current_dir = fun _ctx ->
  with_tempdir "kernel_new_process"
    (fun tempdir ->
      let stdio = Kernel.Process.{ stdin = Stdin.Null; stdout = Stdout.Pipe; stderr = Stderr.Null } in
      let* process = lift_process
        (Kernel.Process.spawn ~program:"/bin/pwd" ~args:[||] ~current_dir:tempdir ~stdio ()) in
      with_process process
        (fun process ->
          match Kernel.Process.stdout process with
          | None -> Error "expected stdout pipe"
          | Some stdout ->
              with_poll
                (fun poll ->
                  let* status = wait_for_exit poll ~token:(Kernel.Async.Token.make 528) process in
                  let* payload = read_all poll ~token:(Kernel.Async.Token.make 506) stdout in
                  let output =
                    if
                      String.length payload != 0
                      && String.get payload (String.length payload - 1) = '\n'
                    then
                      String.sub payload 0 (String.length payload - 1)
                    else
                      payload
                  in
                  let* expected = lift_file (Kernel.Fs.File.canonicalize tempdir) in
                  let* actual = lift_file
                    (Kernel.Fs.File.canonicalize (Kernel.Path.of_string output)) in
                  if
                    Kernel.Path.to_string actual = Kernel.Path.to_string expected
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
      let* input_file = lift_file (Kernel.Fs.File.open_write input_path) in
      let* () =
        with_file input_file
          (fun () ->
            let* _ = lift_file
              (Kernel.Fs.File.write input_file (Kernel.Bytes.of_string "file-stdio")) in
            Ok ())
      in
      let* stdin_file = lift_file (Kernel.Fs.File.open_read input_path) in
      protect
        ~finally:(fun () ->
          let _ = Kernel.Fs.File.close stdin_file in
          ())
        (fun () ->
          let* stdout_file = lift_file
            (Kernel.Fs.File.open_write ~create:true ~truncate:true output_path) in
          protect
            ~finally:(fun () ->
              let _ = Kernel.Fs.File.close stdout_file in
              ())
            (fun () ->
              let stdio =
                Kernel.Process.{
                  stdin = Stdin.File stdin_file;
                  stdout = Stdout.File stdout_file;
                  stderr = Stderr.Null
                } in
              let* process = lift_process
                (Kernel.Process.spawn ~program:"/bin/cat" ~args:[||] ~stdio ()) in
              let* status =
                with_process
                  process
                  (fun process ->
                    with_poll
                      (fun poll -> wait_for_exit poll ~token:(Kernel.Async.Token.make 529) process))
              in
              let* output = lift_file (Kernel.Fs.File.open_read output_path) in
              let buffer = Kernel.Bytes.create 32 in
              let* payload =
                with_file output
                  (fun () ->
                    let* count = lift_file (Kernel.Fs.File.read output buffer) in
                    Ok (Kernel.Bytes.sub_string buffer 0 count))
              in
              if status = Kernel.Process.Exited 0 && payload = "file-stdio" then
                Ok ()
              else
                Error "expected file-backed stdio to roundtrip through the child process")))

let test_file_backed_stdio_uses_no_kernel_pipes = fun _ctx ->
  with_tempdir "kernel_new_process"
    (fun tempdir ->
      let input_path = Kernel.Path.(tempdir / "stdin.txt") in
      let output_path = Kernel.Path.(tempdir / "stdout.txt") in
      let* input_file = lift_file (Kernel.Fs.File.open_write input_path) in
      let* () =
        with_file input_file
          (fun () ->
            let* _ = lift_file
              (Kernel.Fs.File.write input_file (Kernel.Bytes.of_string "file-stdio")) in
            Ok ())
      in
      let* stdin_file = lift_file (Kernel.Fs.File.open_read input_path) in
      protect
        ~finally:(fun () ->
          let _ = Kernel.Fs.File.close stdin_file in
          ())
        (fun () ->
          let* stdout_file = lift_file
            (Kernel.Fs.File.open_write ~create:true ~truncate:true output_path) in
          protect
            ~finally:(fun () ->
              let _ = Kernel.Fs.File.close stdout_file in
              ())
            (fun () ->
              let stdio =
                Kernel.Process.{
                  stdin = Stdin.File stdin_file;
                  stdout = Stdout.File stdout_file;
                  stderr = Stderr.Null
                } in
              let* process = lift_process
                (Kernel.Process.spawn ~program:"/usr/bin/true" ~args:[||] ~stdio ()) in
              with_process process
                (fun process ->
                  let* status =
                    with_poll
                      (fun poll -> wait_for_exit poll ~token:(Kernel.Async.Token.make 532) process)
                  in
                  if
                    status = Kernel.Process.Exited 0
                    && Kernel.Process.stdin process = None
                    && Kernel.Process.stdout process = None
                    && Kernel.Process.stderr process = None
                  then
                    Ok ()
                  else
                    Error "expected file-backed stdio to avoid creating kernel pipe handles"))))

let test_inherited_stdio_uses_no_kernel_pipes = fun _ctx ->
  let stdio =
    Kernel.Process.{ stdin = Stdin.Inherit; stdout = Stdout.Inherit; stderr = Stderr.Inherit } in
  let* process = lift_process (Kernel.Process.spawn ~program:"/usr/bin/true" ~args:[||] ~stdio ()) in
  with_process process
    (fun process ->
      with_poll
        (fun poll ->
          let* status = wait_for_exit poll ~token:(Kernel.Async.Token.make 530) process in
          if
            status = Kernel.Process.Exited 0
            && Kernel.Process.stdin process = None
            && Kernel.Process.stdout process = None
            && Kernel.Process.stderr process = None
          then
            Ok ()
          else
            Error "expected inherited stdio to avoid creating kernel pipes"))

let test_requested_pipe_ownership_matches_stdio = fun _ctx ->
  let stdio = Kernel.Process.{ stdin = Stdin.Pipe; stdout = Stdout.Null; stderr = Stderr.Pipe } in
  let* process = lift_process
    (Kernel.Process.spawn ~program:"/bin/sh" ~args:[|"-c"; "cat >&2"|] ~stdio ()) in
  with_process process
    (fun process ->
      with_poll
        (fun poll ->
          match (
            Kernel.Process.stdin process,
            Kernel.Process.stdout process,
            Kernel.Process.stderr process
          ) with
          | (Some stdin, None, Some stderr) ->
              let payload = Kernel.Bytes.of_string "pipes" in
              let* () = write_all poll ~token:(Kernel.Async.Token.make 507) stdin payload in
              let* () = lift_file (Kernel.Fs.File.close stdin) in
              let* echoed = read_once poll ~token:(Kernel.Async.Token.make 508) stderr in
              let* status = wait_for_exit poll ~token:(Kernel.Async.Token.make 533) process in
              if echoed = "pipes" && status = Kernel.Process.Exited 0 then
                Ok ()
              else
                Error "expected pipe-backed stdio ownership to match the requested channels"
          | _ -> Error "expected only stdin and stderr kernel pipes to be present"))

let test_spawn_missing_program_reports_no_such_file = fun _ctx ->
  let stdio = Kernel.Process.{ stdin = Stdin.Null; stdout = Stdout.Null; stderr = Stderr.Null } in
  match Kernel.Process.spawn ~program:"/definitely/missing/kernel-new-process" ~args:[||] ~stdio () with
  | Kernel.Result.Error (Kernel.Process.System Kernel.SystemError.NoSuchFileOrDirectory) ->
      Ok ()
  | Kernel.Result.Error error ->
      Error (Kernel.Process.error_to_string error)
  | Kernel.Result.Ok process ->
      let _ = Kernel.Process.close process in
      Error "expected missing program spawn to fail"

let test_process_close_tolerates_preclosed_pipe_handles = fun _ctx ->
  let stdio = Kernel.Process.{ stdin = Stdin.Pipe; stdout = Stdout.Pipe; stderr = Stderr.Null } in
  let* process = lift_process (Kernel.Process.spawn ~program:"/bin/cat" ~args:[||] ~stdio ()) in
  match (Kernel.Process.stdin process, Kernel.Process.stdout process) with
  | (Some stdin, Some stdout) ->
      let* () = lift_file (Kernel.Fs.File.close stdin) in
      let* () = lift_file (Kernel.Fs.File.close stdout) in
      let* () = lift_process (Kernel.Process.close process) in
      Ok ()
  | _ ->
      let _ = Kernel.Process.close process in
      Error "expected process pipes to exist"

let test_try_wait_is_stable_after_exit = fun _ctx ->
  let stdio = Kernel.Process.{ stdin = Stdin.Null; stdout = Stdout.Null; stderr = Stderr.Null } in
  let* process = lift_process
    (Kernel.Process.spawn ~program:"/bin/sh" ~args:[|"-c"; "exit 0"|] ~stdio ()) in
  with_process process
    (fun process ->
      with_poll
        (fun poll ->
          let* first = wait_for_exit poll ~token:(Kernel.Async.Token.make 534) process in
          let* second = lift_process (Kernel.Process.try_wait process) in
          match second with
          | Some status when first = Kernel.Process.Exited 0 && status = Kernel.Process.Exited 0 -> Ok ()
          | Some _ -> Error "expected try_wait to remain stable after the process exits"
          | None -> Error "expected exited process to stay observable through try_wait"))

let test_kill_after_exit_reports_no_such_process = fun _ctx ->
  let stdio = Kernel.Process.{ stdin = Stdin.Null; stdout = Stdout.Null; stderr = Stderr.Null } in
  let* process = lift_process
    (Kernel.Process.spawn ~program:"/bin/sh" ~args:[|"-c"; "exit 0"|] ~stdio ()) in
  with_process process
    (fun process ->
      with_poll
        (fun poll ->
          let* _ = wait_for_exit poll ~token:(Kernel.Async.Token.make 535) process in
          match Kernel.Process.kill process ~signal:9 with
          | Kernel.Result.Error (Kernel.Process.System Kernel.SystemError.NoSuchProcess) -> Ok ()
          | Kernel.Result.Error error -> Error (Kernel.Process.error_to_string error)
          | Kernel.Result.Ok () -> Error "expected signaling an exited process to report no_such_process"))

let test_process_close_is_idempotent = fun _ctx ->
  let stdio = Kernel.Process.{ stdin = Stdin.Pipe; stdout = Stdout.Pipe; stderr = Stderr.Null } in
  let* process = lift_process (Kernel.Process.spawn ~program:"/bin/cat" ~args:[||] ~stdio ()) in
  let* () = lift_process (Kernel.Process.close process) in
  let* () = lift_process (Kernel.Process.close process) in
  Ok ()

let test_repeated_spawn_and_poll_exit_stays_healthy = fun _ctx ->
  let stdio = Kernel.Process.{ stdin = Stdin.Null; stdout = Stdout.Null; stderr = Stderr.Null } in
  with_poll
    (fun poll ->
      let rec loop remaining =
        if remaining = 0 then
          Ok ()
        else
          let* process = lift_process
            (Kernel.Process.spawn ~program:"/usr/bin/true" ~args:[||] ~stdio ()) in
          let outcome =
            with_process process
              (fun process ->
                let* status = wait_for_exit poll ~token:(Kernel.Async.Token.make (600 + remaining)) process in
                if status = Kernel.Process.Exited 0 then
                  Ok ()
                else
                  Error "expected repeated spawned process to exit cleanly")
          in
          let* () = outcome in
          loop (remaining - 1)
      in
      loop 32)

let test_many_process_sources_report_burst_exit_readiness = fun _ctx ->
  let stdio = Kernel.Process.{ stdin = Stdin.Null; stdout = Stdout.Null; stderr = Stderr.Null } in
  with_poll
    (fun poll ->
      let rec spawn_many remaining acc =
        if remaining = 0 then
          Ok (List.rev acc)
        else
          let* process = lift_process
            (Kernel.Process.spawn ~program:"/bin/sh" ~args:[|"-c"; "sleep 0.03"|] ~stdio ()) in
          spawn_many (remaining - 1) (process :: acc)
      in
      let* processes = spawn_many 12 [] in
      let sources = List.map Kernel.Process.to_source processes in
      let rec deregister_all = function
        | [] -> ()
        | source :: rest ->
            let _ = Kernel.Async.Poll.deregister poll source in
            deregister_all rest
      in
      protect
        ~finally:(fun () ->
          deregister_all sources;
          close_processes processes)
        (fun () ->
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
          let seen = Kernel.Array.make 12 false in
          let rec mark = function
            | [] -> ()
            | event :: rest ->
                if Kernel.Async.Event.is_priority event then
                  let token = Kernel.Async.Token.unsafe_value (Kernel.Async.Event.token event) in
                  if token >= 0 && token < 12 then
                    Kernel.Array.set seen token true;
                  mark rest
          in
          let rec all_seen index =
            if index = 12 then
              true
            else if Kernel.Array.get seen index then
              all_seen (index + 1)
            else
              false
          in
          let rec mark_observed index = function
            | [] -> Ok ()
            | process :: rest ->
                if Kernel.Array.get seen index then
                  mark_observed (index + 1) rest
                else
                  let* status = lift_process (Kernel.Process.try_wait process) in
                  match status with
                  | Some (Kernel.Process.Exited 0) ->
                      Kernel.Array.set seen index true;
                      mark_observed (index + 1) rest
                  | Some _ ->
                      Error "expected burst-exit process sources to preserve a clean exit status"
                  | None ->
                      mark_observed (index + 1) rest
          in
          let rec poll_until attempts =
            let* () = mark_observed 0 processes in
            if all_seen 0 then
              Ok ()
            else if attempts = 0 then
              Error "expected many process sources to report exit readiness after a burst exit"
            else
              let* events = lift_async
                (Kernel.Async.Poll.poll ~timeout:100_000_000L ~max_events:32 poll) in
              mark events;
              poll_until (attempts - 1)
          in
          let rec verify = function
            | [] -> Ok ()
            | process :: rest ->
                let* first = lift_process (Kernel.Process.try_wait process) in
                let* second = lift_process (Kernel.Process.try_wait process) in
                match (first, second) with
                | (Some (Kernel.Process.Exited 0), Some (Kernel.Process.Exited 0)) -> verify rest
                | _ -> Error "expected burst-exit processes to stay observable through repeated try_wait"
          in
          let* () = register 0 processes in
          let* () = poll_until 16 in
          verify processes))

let tests = [
  Test.case "Process current_pid is positive" test_current_pid_is_positive;
  Test.case "Process stdout pipe roundtrips" test_stdout_pipe_roundtrips;
  Test.case "Process stdin and stdout pipes roundtrip" test_stdin_and_stdout_pipes_roundtrip;
  Test.case "Process stderr redirect_to_stdout merges streams" test_stderr_redirect_to_stdout_merges_streams;
  Test.case "Process try_wait reports running then exit" test_try_wait_reports_running_then_exit;
  Test.case "Process source reports exit readiness" test_process_source_reports_exit_ready;
  Test.case "Process source reregister updates token" test_process_source_reregister_updates_token;
  Test.case "Process close preserves registered exit source" test_process_close_preserves_registered_exit_source;
  Test.case "Process source deregister before close is harmless" test_process_source_deregister_before_close_is_harmless;
  Test.case "Process try_wait is stable after priority readiness" test_try_wait_is_stable_after_priority_readiness;
  Test.case "Process kill reports signaled status" test_kill_reports_signaled_status;
  Test.case "Process sigterm reports signaled status" test_sigterm_reports_signaled_status;
  Test.case "Process preserves non-zero exit status" test_non_zero_exit_status_roundtrips;
  Test.case "Process spawn applies custom environment" test_spawn_applies_custom_environment;
  Test.case "Process spawn applies current_dir" test_spawn_applies_current_dir;
  Test.case "Process file-backed stdio roundtrips" test_file_backed_stdio_roundtrips;
  Test.case "Process file-backed stdio uses no kernel pipes" test_file_backed_stdio_uses_no_kernel_pipes;
  Test.case "Process inherited stdio uses no kernel pipes" test_inherited_stdio_uses_no_kernel_pipes;
  Test.case "Process requested pipe ownership matches stdio" test_requested_pipe_ownership_matches_stdio;
  Test.case "Process spawn missing program reports no-such-file" test_spawn_missing_program_reports_no_such_file;
  Test.case "Process close tolerates preclosed pipe handles" test_process_close_tolerates_preclosed_pipe_handles;
  Test.case "Process try_wait is stable after exit" test_try_wait_is_stable_after_exit;
  Test.case "Process kill after exit reports no-such-process" test_kill_after_exit_reports_no_such_process;
  Test.case "Process close is idempotent" test_process_close_is_idempotent;
  Test.case ~size:Test.Large "Process many sources report burst exit readiness" test_many_process_sources_report_burst_exit_readiness;
  Test.case ~size:Test.Large "Process repeated spawn and poll exit stays healthy" test_repeated_spawn_and_poll_exit_stays_healthy;
]

let main = fun ~args -> Test.Cli.main ~name:"kernel_new_process_tests" ~tests ~args

let () = Actors.run ~main ~args:Env.args ()
