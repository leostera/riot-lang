open Miniriot

let () =
  let exited = ref false in
  
  let worker () =
    exited := true;
    Process.Normal
  in
  
  let main () =
    let _pid = spawn worker in
    yield (); yield (); (* Let worker run and exit *)
    
    if !exited then
      Process.Normal
    else
      Process.Exception (Failure "Worker didn't exit")
  in
  
  let status = run ~main in
  Printf.printf "lifecycle_normal_exit: %s\n" (if status = 0 then "✓ PASS" else "✗ FAIL");
  Stdlib.exit status