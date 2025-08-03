open Miniriot

let () =
  let count = ref 0 in
  let worker id () =
    incr count;
    Printf.printf "  Worker %d running (count=%d)\n" id !count;
    Process.Normal
  in
  
  let main () =
    for i = 1 to 5 do
      let _pid = spawn (worker i) in
      ()
    done;
    
    (* Let all workers run *)
    for _ = 1 to 10 do
      yield ()
    done;
    
    if !count = 5 then
      Process.Normal
    else
      Process.Exception (Failure (Printf.sprintf "Expected 5 workers, got %d" !count))
  in
  
  let status = run ~main in
  Printf.printf "spawn_multiple: %s\n" (if status = 0 then "✓ PASS" else "✗ FAIL");
  Stdlib.exit status