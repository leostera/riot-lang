open Miniriot

let () =
  let worker_ran = ref false in
  
  let worker () =
    for _ = 1 to 10 do
      worker_ran := true;
      yield ()
    done;
    Process.Normal
  in
  
  let main () =
    let _pid = spawn worker in
    (* Exit immediately - worker may or may not run *)
    Process.Normal
  in
  
  let status = run ~main in
  Printf.printf "lifecycle_main_process_exit: %s (worker_ran=%b)\n" 
    (if status = 0 then "✓ PASS" else "✗ FAIL") !worker_ran;
  Stdlib.exit status