open Miniriot

let () =
  let worker () =
    raise (Failure "Worker error")
  in
  
  let main () =
    let _pid = spawn worker in
    yield (); yield ();
    (* Worker should have crashed but main continues *)
    Process.Normal
  in
  
  let status = run ~main in
  Printf.printf "lifecycle_exception_exit: %s\n" (if status = 0 then "✓ PASS" else "✗ FAIL");
  Stdlib.exit status