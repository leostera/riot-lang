open Miniriot

type Message.t += Exit

let () =
  let states = ref [] in

  let worker () =
    states := "running" :: !states;
    match receive () with
    | Exit ->
        states := "received_exit" :: !states;
        Process.Normal
    | _ -> Process.Normal
  in

  let main () =
    states := "main_start" :: !states;
    let pid = spawn worker in
    states := "spawned" :: !states;

    yield ();
    (* Let worker start *)
    states := "after_yield" :: !states;

    send pid Exit;
    states := "sent_exit" :: !states;

    yield ();
    yield ();
    (* Let worker process and exit *)
    states := "final" :: !states;

    (* Check we got expected transitions *)
    let has_state s = List.mem s !states in
    if
      has_state "main_start" && has_state "spawned" && has_state "running"
      && has_state "final"
    then Process.Normal
    else Process.Exception (Failure "State transitions missing")
  in

  let status = run ~main in
  Printf.printf "lifecycle_state_transitions: %s\n"
    (if status = 0 then "✓ PASS" else "✗ FAIL");
  Stdlib.exit status
