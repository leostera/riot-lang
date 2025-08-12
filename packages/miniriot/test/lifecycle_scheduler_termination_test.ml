open Miniriot

let () =
  let counter = ref 0 in

  let rec worker n () =
    if n > 0 then (
      incr counter;
      yield ();
      worker (n - 1) ())
    else Process.Normal
  in

  let main () =
    (* Spawn multiple workers *)
    for i = 1 to 5 do
      let _pid = spawn (worker i) in
      ()
    done;

    (* Let them all run for a bit *)
    for _ = 1 to 20 do
      yield ()
    done;

    if !counter > 0 then (* Some work was done *)
      Process.Normal
    else Process.Exception (Failure "No work was done")
  in

  let status = run ~main in
  Printf.printf "lifecycle_scheduler_termination: %s (counter=%d)\n"
    (if status = 0 then "✓ PASS" else "✗ FAIL")
    !counter;
  Stdlib.exit status
