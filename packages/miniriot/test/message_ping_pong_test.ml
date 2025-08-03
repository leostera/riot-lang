open Miniriot

type Message.t += Ping | Pong

let () =
  let pings = ref 0 in
  let pongs = ref 0 in
  
  let rec ponger_fixed pinger_pid () =
    match receive () with
    | Ping ->
        incr pongs;
        send pinger_pid Pong;
        ponger_fixed pinger_pid ()
    | Exit -> Process.Normal
    | _ -> ponger_fixed pinger_pid ()
  in
  
  let main () =
    let my_pid = self () in
    let ponger_pid = spawn (ponger_fixed my_pid) in
    
    (* Act as the pinger *)
    for _ = 1 to 3 do
      incr pings;
      send ponger_pid Ping;
      match receive () with
      | Pong -> ()
      | _ -> failwith "Expected Pong"
    done;
    
    send ponger_pid Exit;
    yield ();
    
    if !pings = 3 && !pongs = 3 then
      Process.Normal
    else
      Process.Exception (Failure (Printf.sprintf "Pings: %d, Pongs: %d" !pings !pongs))
  in
  
  let status = run ~main in
  Printf.printf "message_ping_pong: %s\n" (if status = 0 then "✓ PASS" else "✗ FAIL");
  Stdlib.exit status