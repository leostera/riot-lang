open Miniriot

let () =
  let main_pid = ref None in
  let worker_pid = ref None in
  
  let worker () =
    worker_pid := Some (self ());
    Process.Normal
  in
  
  let main () =
    main_pid := Some (self ());
    let spawned_pid = spawn worker in
    yield ();
    
    match !worker_pid with
    | None -> Process.Exception (Failure "Worker didn't set its pid")
    | Some wpid ->
        if Pid.equal spawned_pid wpid && not (Pid.equal wpid (Option.get !main_pid)) then
          Process.Normal
        else
          Process.Exception (Failure "PID mismatch")
  in
  
  let status = run ~main in
  Printf.printf "spawn_self_pid: %s\n" (if status = 0 then "✓ PASS" else "✗ FAIL");
  Stdlib.exit status