open Std

type Message.t +=
  | Actor_self_reply of Pid.t
  | Actor_ready of Pid.t
  | Actor_stop

let await = fun ~what selector ->
  try Ok (receive ~selector ~timeout:(Time.Duration.from_secs 1) ()) with
  | Receive_timeout -> Error ("timed out waiting for " ^ what)

let test_actor_self_in_spawned_actor = fun _ctx ->
  let parent = self () in
  ignore
    (
      Actor.spawn
        (fun () ->
          send parent (Actor_self_reply (Actor.self ()));
          Ok ())
    );
  match await
    ~what:"spawned actor self"
    (
      function
      | Actor_self_reply pid -> `select pid
      | _ -> `skip
    ) with
  | Error _ as err -> err
  | Ok child_pid ->
      if not (Pid.equal child_pid parent) then
        Ok ()
      else
        Error "expected child Actor.self to differ from parent pid"

let test_actor_spawn_returns_live_pid = fun _ctx ->
  let parent = self () in
  let child =
    Actor.spawn
      (fun () ->
        send parent (Actor_ready (Actor.self ()));
        receive
          ~selector:(
            function
            | Actor_stop -> `select ()
            | _ -> `skip
          )
          ();
        Ok ())
  in
  let _monitor = Runtime.Actor.monitor child in
  match await
    ~what:"spawned actor ready"
    (
      function
      | Actor_ready pid -> `select pid
      | _ -> `skip
    ) with
  | Error _ as err -> err
  | Ok ready_pid ->
      if Pid.equal ready_pid child then (
        send child Actor_stop;
        ignore
          (
            await
              ~what:"spawned actor down"
              (
                function
                | Runtime.Actor.DOWN { pid; _ } when Pid.equal pid child -> `select ()
                | _ -> `skip
              )
          );
        Ok ()
      ) else
        Error "expected spawn to return the same pid reported by the child"

let is_failure = fun exn ~message ->
  match exn with
  | Failure reason -> String.equal reason message
  | _ -> false

let test_actor_spawn_link_reports_abnormal_exit = fun _ctx ->
  Actor.set_flags [ Runtime.Actor.TrapExit true ];
  let child = Actor.spawn_link (fun () -> Error (Failure "boom")) in
  match await
    ~what:"linked actor exit"
    (
      function
      | Runtime.Actor.EXIT { from; reason = Error exn } when Pid.equal from child
      && is_failure exn ~message:"boom" -> `select ()
      | _ -> `skip
    ) with
  | Ok () -> Ok ()
  | Error _ as err -> err

let tests =
  Test.[
    case "Actor.self inside spawned actor differs from parent" test_actor_self_in_spawned_actor;
    case "Actor.spawn returns a live pid" test_actor_spawn_returns_live_pid;
    case
      "Actor.spawn_link reports abnormal exit when trapping exits"
      test_actor_spawn_link_reports_abnormal_exit;
  ]

let main ~args = Test.Cli.main ~name:"Actor" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
