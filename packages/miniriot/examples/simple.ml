open Miniriot

type Message.t += 
  | Hello

let worker () =
  Printf.printf "[Worker] Starting\n%!";
  match receive () with
  | Hello -> 
      Printf.printf "[Worker] Got Hello!\n%!";
      Process.Normal
  | _ -> 
      Printf.printf "[Worker] Got unknown message\n%!";
      Process.Normal

let main () =
  Printf.printf "[Main] Starting\n%!";
  
  let pid = spawn worker in
  Printf.printf "[Main] Spawned worker %s\n%!" (Pid.to_string pid);
  
  send pid Hello;
  Printf.printf "[Main] Sent Hello\n%!";
  
  (* Give worker time to run *)
  yield ();
  yield ();
  
  Printf.printf "[Main] Done\n%!";
  Process.Normal

let () =
  enable_trace ();
  let status = run ~main in
  Printf.printf "Exit status: %d\n%!" status;
  Stdlib.exit status