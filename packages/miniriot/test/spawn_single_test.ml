open Miniriot

let () =
  let spawned = ref false in
  let worker () =
    spawned := true;
    Process.Normal
  in

  let main () =
    let _pid = spawn worker in
    yield ();
    (* Let worker run *)
    if !spawned then Process.Normal
    else Process.Exception (Failure "Worker didn't run")
  in

  let status = run ~main in
  Printf.printf "spawn_single: %s\n" (if status = 0 then "✓ PASS" else "✗ FAIL");
  Stdlib.exit status
