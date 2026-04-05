open Actors
open Actors.Exception
module Result = Std.Result
module Test = Std.Test

type Message.t +=
  | Blocking_actor_started
  | Blocking_actor_done
  | Fast_actor_done

let blocking_actor = fun ~parent ->
  send parent Blocking_actor_started;
  Kernel.Time.sleep 0.25;
  send parent Blocking_actor_done;
  Result.Ok ()

let fast_actor = fun ~parent ->
  send parent Fast_actor_done;
  Result.Ok ()

let test_spawn_blocked_does_not_occupy_the_normal_scheduler = fun () ->
  let parent = self () in
  let _blocking =
    spawn_blocked (fun () -> blocking_actor ~parent)
  in
  let _ =
    receive
      ~selector:(
        function
        | Blocking_actor_started -> `select ()
        | _ -> `skip
      )
      ~timeout:5.0
      ()
  in
  let _fast =
    spawn (fun () -> fast_actor ~parent)
  in
  let first_completion =
    receive
      ~selector:(
        function
        | Fast_actor_done -> `select `fast
        | Blocking_actor_done -> `select `blocked
        | _ -> `skip
      )
      ~timeout:5.0
      ()
  in
  match first_completion with
  | `blocked -> Result.Error "blocking actor finished before a normal actor could run"
  | `fast ->
      let _ =
        receive
          ~selector:(
            function
            | Blocking_actor_done -> `select ()
            | _ -> `skip
          )
          ~timeout:5.0
          ()
      in
      Result.Ok ()

let test_case = fun _ctx ->
  try test_spawn_blocked_does_not_occupy_the_normal_scheduler () with
  | Receive_timeout -> Result.Error "timed out while testing blocked spawn"
  | exn -> Result.Error (Kernel.Exception.to_string exn)

let () =
  let tests = [
    Test.case "spawn_blocked isolates blocking work from the normal scheduler" test_case;
  ] in
  let normalize_args = function
    | [] -> [ "spawn_blocked_tests"; "run-tests" ]
    | [ exe ] -> [ exe; "run-tests" ]
    | args -> args
  in
  let main ~args =
    match Test.Cli.main ~name:"spawn_blocked_tests" ~tests ~args:(normalize_args args) with
    | Result.Ok () -> Result.Ok ()
    | Result.Error msg -> Result.Error msg
  in
  Actors.run ~main ~args:Std.Env.args ~config:(Actors.Config.make ~scheduler_count:1 ()) ()
