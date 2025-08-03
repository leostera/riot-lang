open Miniriot

type Message.t += Hello of string

let () =
  let main () =
    let worker () = Process.Normal in
    let pid = spawn worker in
    yield (); (* Let worker die *)
    
    (* Sending to a dead process should not crash *)
    send pid (Hello "nowhere");
    Process.Normal
  in
  
  let status = run ~main in
  Printf.printf "message_dead_process: %s\n" (if status = 0 then "✓ PASS" else "✗ FAIL");
  Stdlib.exit status