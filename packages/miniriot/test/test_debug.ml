open Miniriot

type Message.t += Hello of string

let () =
  (* enable_trace (); *)
  
  let received = ref None in
  
  let worker () =
    Printf.printf "[Worker] Starting\n%!";
    match receive () with
    | Hello msg ->
        Printf.printf "[Worker] Received: %s\n%!" msg;
        received := Some msg;
        Process.Normal
    | _ -> 
        Printf.printf "[Worker] Unexpected message\n%!";
        Process.Exception (Failure "Unexpected message")
  in
  
  let main () =
    Printf.printf "[Main] Starting\n%!";
    let pid = spawn worker in
    Printf.printf "[Main] Spawned worker %s\n%!" (Pid.to_string pid);
    send pid (Hello "world");
    Printf.printf "[Main] Sent message\n%!";
    
    yield (); (* Let worker start *)
    yield (); (* Let worker receive *)
    yield (); (* Let worker finish *)
    
    Printf.printf "[Main] Checking result: %s\n%!" 
      (match !received with None -> "None" | Some s -> Printf.sprintf "Some(%s)" s);
    
    match !received with
    | Some "world" -> 
        Printf.printf "[Main] Success!\n%!";
        Process.Normal
    | _ -> 
        Printf.printf "[Main] Failed!\n%!";
        Process.Exception (Failure "Message not received correctly")
  in
  
  let status = run ~main in
  Printf.printf "Exit status: %d\n%!" status