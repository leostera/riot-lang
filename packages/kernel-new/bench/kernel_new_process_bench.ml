open Std
module Kernel = Kernel_new

let lift = function
  | Kernel.Result.Ok value -> value
  | Kernel.Result.Error error -> Kernel.Error.panic (Kernel.Error.to_string error)

let is_would_block = function
  | Kernel.Error.Would_block -> true
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

let protect = fun ~finally fn ->
  try
    let value = fn () in
    finally ();
    value
  with
  | error ->
      finally ();
      raise error

let wait_for = fun poll ~token ~interest ~source ~pred ->
  let _ = lift (Kernel.Async.Poll.register poll token interest source) in
  protect
    ~finally:(fun () ->
      let _ = Kernel.Async.Poll.deregister poll source in
      ())
    (fun () ->
      let events = lift (Kernel.Async.Poll.poll ~timeout:100_000_000L poll) in
      let found =
        List.exists
          (fun event ->
            Kernel.Async.Token.equal token (Kernel.Async.Event.token event)
            && pred event)
          events
      in
      if not found then
        Kernel.Error.panic "expected readiness event")

let wait_readable = fun poll ~token source ->
  wait_for
    poll
    ~token
    ~interest:Kernel.Async.Interest.readable
    ~source
    ~pred:Kernel.Async.Event.is_readable

let read_once = fun poll ~token file ->
  let buffer = Kernel.Bytes.create 128 in
  let rec loop = fun () ->
    match Kernel.Fs.File.read file buffer with
    | Kernel.Result.Ok count ->
        ignore (Kernel.Bytes.sub_string buffer 0 count)
    | Kernel.Result.Error error ->
        if is_would_block error then (
          wait_readable poll ~token (Kernel.Fs.File.to_source file);
          loop ()
        ) else
          Kernel.Error.panic (Kernel.Error.to_string error)
  in
  loop ()

let bench_spawn_true = fun () ->
  let process =
    lift
      (Kernel.Process.spawn
         ~program:"/usr/bin/true"
         ~args:[||]
         ~stdio:Kernel.Process.default_stdio
         ())
  in
  with_process process
    (fun process ->
      ignore (lift (Kernel.Process.wait process)))

let bench_spawn_echo_with_pipe = fun () ->
  let poll = lift (Kernel.Async.Poll.make ()) in
  let stdio = Kernel.Process.{
    default_stdio with
    stdin = `Null;
    stdout = `Pipe;
    stderr = `Null;
  } in
  let process =
    lift
      (Kernel.Process.spawn
         ~program:"/bin/echo"
         ~args:[| "-n"; "kernel-new" |]
         ~stdio
         ())
  in
  with_process process
    (fun process ->
      match Kernel.Process.stdout process with
      | None ->
          Kernel.Error.panic "expected stdout pipe"
      | Some stdout ->
          read_once poll ~token:(Kernel.Async.Token.make 601) stdout;
          ignore (lift (Kernel.Process.wait process)))

let benchmarks =
  Bench.[
    with_config
      ~config:{ iterations = 25; warmup = 5 }
      "process spawn true and wait"
      bench_spawn_true;
    with_config
      ~config:{ iterations = 25; warmup = 5 }
      "process spawn echo with stdout pipe"
      bench_spawn_echo_with_pipe;
  ]

let () =
  Actors.run
    ~main:(fun ~args ->
      Bench.Cli.main ~name:"kernel_new_process_bench" ~benchmarks ~args)
    ~args:Env.args
    ()
