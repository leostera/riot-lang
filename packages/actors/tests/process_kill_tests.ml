open Actors
open Actors.Exception
module Result = Std.Result
module Test = Std.Test

type Message.t +=
  Child_ready of Pid.t

let waiting_child = fun parent ->
  let child_pid = self () in
  send parent (Child_ready child_pid);
  let _ = receive_any ~timeout:10.0 () in
  Result.Ok ()

let test_kill_wakes_waiting_process = fun () ->
  let parent = self () in
  let child =
    spawn (fun () -> waiting_child parent)
  in
  let _ = Process.monitor child in
  let ready_pid =
    receive
      ~selector:(
        function
        | Child_ready pid -> `select pid
        | _ -> `skip
      )
      ~timeout:2.0
      ()
  in
  if Pid.equal ready_pid child then
    (
      Process.kill child ~reason:(Failure "killed by test");
      let down =
        receive
          ~selector:(
            function
            | Process.DOWN { pid; reason; _ } -> `select (pid, reason)
            | _ -> `skip
          )
          ~timeout:2.0
          ()
      in
      match down with
      | pid, Error (Failure msg) when Pid.equal pid child ->
          if Kernel.String.equal msg "killed by test" then
            Result.Ok ()
          else
            Result.Error "received unexpected kill reason"
      | pid, Error _ when Pid.equal pid child -> Result.Error "received unexpected kill reason"
      | _ -> Result.Error "received DOWN for unexpected pid"
    )
  else
    Result.Error "child ready pid mismatch"

let test_case = fun name fn ->
  try fn () with
  | Receive_timeout -> Result.Error (Kernel.String.concat "" [ "timed out in "; name ])
  | exn -> Result.Error (Kernel.String.concat
    ""
    [ "unexpected exception in "; name; ": "; Kernel.Exception.to_string exn ])

let () =
  let tests = [
    Test.case
      "process kill wakes waiting actor"
      (fun _ctx -> test_case "process kill wakes waiting actor" test_kill_wakes_waiting_process);
  ] in
  let normalize_args = function
    | [] -> [ "process_kill_tests"; "run-tests" ]
    | [ exe ] -> [ exe; "run-tests" ]
    | args -> args
  in
  let main ~args =
    match Test.Cli.main ~name:"process_kill_tests" ~tests ~args:(normalize_args args) with
    | Result.Ok () -> Result.Ok ()
    | Result.Error msg -> Result.Error msg
  in
  Actors.run ~main ~args:Std.Env.args ()
