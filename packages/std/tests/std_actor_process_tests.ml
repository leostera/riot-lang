open Std

type Message.t +=
  | Actor_self_reply of Pid.t
  | Actor_ready of Pid.t
  | Actor_stop

let await = fun ~what selector ->
  try Ok (receive ~selector ~timeout:(Time.Duration.from_secs 1) ())
  with
  | Receive_timeout -> Error ("timed out waiting for " ^ what)

let test_actor_self_in_spawned_actor = fun _ctx ->
  let parent = self () in
  ignore
    (Actor.spawn (fun () ->
       send parent (Actor_self_reply (Actor.self ()));
       Ok ()));
  match await
    ~what:"spawned actor self"
    (function
    | Actor_self_reply pid -> `select pid
    | _ -> `skip)
  with
  | Error _ as err -> err
  | Ok child_pid ->
      if not (Pid.equal child_pid parent) then Ok ()
      else Error "expected child Actor.self to differ from parent pid"

let test_actor_spawn_returns_live_pid = fun _ctx ->
  let parent = self () in
  let child =
    Actor.spawn (fun () ->
      send parent (Actor_ready (Actor.self ()));
      receive
        ~selector:(function
          | Actor_stop -> `select ()
          | _ -> `skip)
        ();
      Ok ())
  in
  let _monitor = Runtime.Actor.monitor child in
  match await
    ~what:"spawned actor ready"
    (function
    | Actor_ready pid -> `select pid
    | _ -> `skip)
  with
  | Error _ as err -> err
  | Ok ready_pid ->
      if Pid.equal ready_pid child then (
        send child Actor_stop;
        ignore
          (await
             ~what:"spawned actor down"
             (function
             | Runtime.Actor.DOWN { pid; _ } when Pid.equal pid child -> `select ()
             | _ -> `skip));
        Ok ()
      ) else
        Error "expected spawn to return the same pid reported by the child"

let test_actor_spawn_link_reports_abnormal_exit = fun _ctx ->
  Actor.set_flags [ Runtime.Actor.TrapExit true ];
  let child =
    Actor.spawn_link (fun () ->
      Error (Failure "boom"))
  in
  match await
    ~what:"linked actor exit"
    (function
    | Runtime.Actor.EXIT { from; reason = Error (Failure message) }
      when Pid.equal from child && String.equal message "boom" -> `select ()
    | _ -> `skip)
  with
  | Ok () -> Ok ()
  | Error _ as err -> err

let test_process_id_is_positive = fun _ctx ->
  if Process.id () > 0l then Ok () else Error "expected Process.id () to be positive"

let test_process_default_stdio_inherits = fun _ctx ->
  match Process.default_stdio with
  | {
   stdin = Process.Stdin.Inherit;
   stdout = Process.Stdout.Inherit;
   stderr = Process.Stderr.Inherit;
  } -> Ok ()
  | _ -> Error "expected Process.default_stdio to inherit all stdio streams"

let tests =
  Test.[
    case "Actor.self inside spawned actor differs from parent" test_actor_self_in_spawned_actor;
    case "Actor.spawn returns a live pid" test_actor_spawn_returns_live_pid;
    case "Actor.spawn_link reports abnormal exit when trapping exits" test_actor_spawn_link_reports_abnormal_exit;
    case "Process.id returns a positive OS pid" test_process_id_is_positive;
    case "Process.default_stdio inherits stdio" test_process_default_stdio_inherits;
  ]

let () =
  Runtime.run ~main:(fun ~args -> Test.Cli.main ~name:"actor_process" ~tests ~args) ~args:Env.args ()
