open Std
module Kernel = Kernel

let lift_process result =
  match result with
  | Kernel.Result.Ok value -> value
  | Kernel.Result.Error error -> Kernel.SystemError.panic
    (Kernel.Error.to_string (Kernel.Error.from_process error))

let lift_async result =
  match result with
  | Kernel.Result.Ok value -> value
  | Kernel.Result.Error error -> Kernel.SystemError.panic
    (Kernel.Error.to_string (Kernel.Error.from_async error))

let lift_file result =
  match result with
  | Kernel.Result.Ok value -> value
  | Kernel.Result.Error error -> Kernel.SystemError.panic
    (Kernel.Error.to_string (Kernel.Error.from_fs_file error))

let is_would_block error =
  match error with
  | Kernel.Fs.File.System system_error -> Kernel.SystemError.would_block system_error
  | _ -> false

let with_process = fun process fn ->
  try
    let value = fn process in
    let _ = Kernel.Process.close process in
    value
  with
  | error ->
      let _ = Kernel.Process.close process in
      raise error

let rec close_processes processes =
  match processes with
  | [] -> ()
  | process :: rest ->
      let _ = Kernel.Process.close process in
      close_processes rest

let protect = fun ~finally fn ->
  try
    let value = fn () in
    finally ();
    value
  with
  | error ->
      finally ();
      raise error

let with_poll = fun fn ->
  let poll = lift_async (Kernel.Async.Poll.make ()) in
  protect
    ~finally:(fun () ->
      let _ = Kernel.Async.Poll.close poll in
      ())
    (fun () -> fn poll)

let with_processes = fun count fn ->
  let stdio = Kernel.Process.{ stdin = Stdin.Null; stdout = Stdout.Null; stderr = Stderr.Null } in
  let rec spawn remaining acc =
    if remaining = 0 then
      List.reverse acc
    else
      let process = lift_process
        (Kernel.Process.spawn ~program:"/bin/sh" ~args:[|"-c"; "sleep 0.02"|] ~stdio ()) in
      spawn (remaining - 1) (process :: acc)
  in
  let processes = spawn count [] in
  protect ~finally:(fun () -> close_processes processes) (fun () -> fn processes)

let wait_for = fun poll ~token ~interest ~source ~pred ->
  let _ = lift_async (Kernel.Async.Poll.register poll token interest source) in
  protect
    ~finally:(fun () ->
      let _ = Kernel.Async.Poll.deregister poll source in
      ())
    (fun () ->
      let events = lift_async (Kernel.Async.Poll.poll ~timeout:100_000_000L poll) in
      let found =
        List.any events ~fn:(fun event ->
          Kernel.Async.Token.equal token (Kernel.Async.Event.token event) && pred event)
      in
      if not found then
        Kernel.SystemError.panic "expected readiness event")

let wait_readable = fun poll ~token source ->
  wait_for poll ~token ~interest:Kernel.Async.Interest.readable ~source ~pred:Kernel.Async.Event.is_readable

let read_once = fun poll ~token file ->
  let buffer = Kernel.Bytes.create ~size:128 in
  let rec loop () =
    match Kernel.Fs.File.read file buffer with
    | Kernel.Result.Ok count ->
        let _ = Kernel.Bytes.sub_string buffer ~offset:0 ~len:count in
        ()
    | Kernel.Result.Error error ->
        if is_would_block error then
          (
            wait_readable poll ~token (Kernel.Fs.File.to_source file);
            loop ()
          )
        else
          Kernel.SystemError.panic (Kernel.Error.to_string (Kernel.Error.from_fs_file error))
  in
  loop ()

let drain_all = fun poll ~token file ->
  let buffer = Kernel.Bytes.create ~size:4_096 in
  let rec loop () =
    match Kernel.Fs.File.read file buffer with
    | Kernel.Result.Ok 0 ->
        ()
    | Kernel.Result.Ok _ ->
        loop ()
    | Kernel.Result.Error error ->
        if is_would_block error then
          (
            wait_readable poll ~token (Kernel.Fs.File.to_source file);
            loop ()
          )
        else
          Kernel.SystemError.panic (Kernel.Error.to_string (Kernel.Error.from_fs_file error))
  in
  loop ()

let wait_for_exit = fun poll ~token process ->
  let exit_poll_timeout = 1_000_000L in
  let rec loop () =
    match Kernel.Process.try_wait process with
    | Kernel.Result.Ok (Some _status as status) ->
        status
    | Kernel.Result.Ok None ->
        let source = Kernel.Process.to_source process in
        let _ = lift_async
          (Kernel.Async.Poll.register poll token Kernel.Async.Interest.priority source) in
        protect
          ~finally:(fun () ->
            let _ = Kernel.Async.Poll.deregister poll source in
            ())
          (fun () ->
            let _ = lift_async (Kernel.Async.Poll.poll ~timeout:exit_poll_timeout poll) in
            loop ())
    | Kernel.Result.Error error ->
        Kernel.SystemError.panic (Kernel.Error.to_string (Kernel.Error.from_process error))
  in
  loop ()

let bench_spawn_true = fun () ->
  with_poll
    (fun poll ->
      let process = lift_process
        (Kernel.Process.spawn
          ~program:"/usr/bin/true"
          ~args:[||]
          ~stdio:Kernel.Process.default_stdio
          ()) in
      with_process
        process
        (fun process ->
          let _ = wait_for_exit poll ~token:(Kernel.Async.Token.make 610) process in
          ()))

let bench_spawn_echo_with_pipe = fun () ->
  with_poll
    (fun poll ->
      let stdio = Kernel.Process.{ stdin = Stdin.Null; stdout = Stdout.Pipe; stderr = Stderr.Null } in
      let process = lift_process
        (Kernel.Process.spawn ~program:"/bin/echo" ~args:[|"-n"; "kernel-new"|] ~stdio ()) in
      with_process process
        (fun process ->
          match Kernel.Process.stdout process with
          | None -> Kernel.SystemError.panic "expected stdout pipe"
          | Some stdout ->
              read_once poll ~token:(Kernel.Async.Token.make 601) stdout;
              let _ = wait_for_exit poll ~token:(Kernel.Async.Token.make 611) process in
              ()))

let bench_many_process_exit_sources = fun () ->
  with_poll
    (fun poll ->
      with_processes 16
        (fun processes ->
          let rec register index = function
            | [] -> ()
            | process :: rest ->
                let _ = lift_async
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
            | [] -> ()
            | process :: rest ->
                (
                  match Kernel.Process.try_wait process with
                  | Kernel.Result.Ok (Some (Kernel.Process.Exited 0)) ->
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
          let rec poll_until attempts =
            if all_seen 0 then
              ()
            else if attempts = 0 then
              Kernel.SystemError.panic "expected many child processes to report exit readiness"
            else
              let events = lift_async
                (Kernel.Async.Poll.poll ~timeout:100_000_000L ~max_events:32 poll) in
              mark_events events;
              mark_exits 0 processes;
              poll_until (attempts - 1)
          in
          register 0 processes;
          poll_until 16))

let bench_stdout_pipe_drain_small_output = fun () ->
  with_poll
    (fun poll ->
      let stdio = Kernel.Process.{ stdin = Stdin.Null; stdout = Stdout.Pipe; stderr = Stderr.Null } in
      let process = lift_process
        (Kernel.Process.spawn
          ~program:"/bin/sh"
          ~args:[|"-c"; "dd if=/dev/zero bs=4096 count=1 2>/dev/null"|]
          ~stdio
          ()) in
      with_process process
        (fun process ->
          match Kernel.Process.stdout process with
          | None -> Kernel.SystemError.panic "expected stdout pipe"
          | Some stdout ->
              let _ = wait_for_exit poll ~token:(Kernel.Async.Token.make 612) process in
              drain_all poll ~token:(Kernel.Async.Token.make 602) stdout))

let bench_stdout_pipe_drain_medium_output = fun () ->
  with_poll
    (fun poll ->
      let stdio = Kernel.Process.{ stdin = Stdin.Null; stdout = Stdout.Pipe; stderr = Stderr.Null } in
      let process = lift_process
        (Kernel.Process.spawn
          ~program:"/bin/sh"
          ~args:[|"-c"; "dd if=/dev/zero bs=65536 count=1 2>/dev/null"|]
          ~stdio
          ()) in
      with_process process
        (fun process ->
          match Kernel.Process.stdout process with
          | None -> Kernel.SystemError.panic "expected stdout pipe"
          | Some stdout ->
              let _ = wait_for_exit poll ~token:(Kernel.Async.Token.make 613) process in
              drain_all poll ~token:(Kernel.Async.Token.make 603) stdout))

let benchmarks =
  Bench.[
    with_config ~config:{ iterations = 25; warmup = 5 } "process spawn true and poll exit" bench_spawn_true;
    with_config
      ~config:{ iterations = 25; warmup = 5 }
      "process spawn echo with stdout pipe and poll exit"
      bench_spawn_echo_with_pipe;
    with_config
      ~config:{ iterations = 20; warmup = 5 }
      "process stdout pipe drain after exit: 4KiB"
      bench_stdout_pipe_drain_small_output;
    with_config
      ~config:{ iterations = 15; warmup = 3 }
      "process stdout pipe drain after exit: 64KiB"
      bench_stdout_pipe_drain_medium_output;
    with_config ~config:{ iterations = 15; warmup = 3 } "process many child exit sources" bench_many_process_exit_sources;
  ]

let () =
  Actors.run
    ~main:(fun ~args -> Bench.Cli.main ~name:"kernel_new_process_bench" ~benchmarks ~args)
    ~args:Env.args
    ()
