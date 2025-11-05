(* Ultra-simple test of spawn + message passing *)
open Std

type Message.t += Ping | Pong

let main ~args:_ =
  print "[MAIN] Starting\n%!";
  let main_pid = self () in
  
  let child_pid = spawn (fun () ->
    print "[CHILD] Started, self = %s\n%!" (Pid.to_string (self ()));
    print "[CHILD] Waiting for Ping...\n%!";
    let msg = receive_any () in
    print "[CHILD] Received message!\n%!";
    (match msg with
    | Ping ->
        print "[CHILD] Got Ping, sending Pong\n%!";
        send main_pid Pong
    | _ -> print "[CHILD] Got unknown message\n%!");
    Ok ()
  ) in
  
  print "[MAIN] Child spawned as %s\n%!" (Pid.to_string child_pid);
  print "[MAIN] Sending Ping...\n%!";
  send child_pid Ping;
  print "[MAIN] Ping sent, waiting for Pong...\n%!";
  
  let msg = receive_any () in
  print "[MAIN] Received message!\n%!";
  (match msg with
  | Pong -> print "[MAIN] Got Pong! Success!\n%!"
  | _ -> print "[MAIN] Got unknown message\n%!");
  
  Ok ()

let () = Miniriot.run ~main ~args:Std.Env.args ()
