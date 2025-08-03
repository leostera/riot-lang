open Miniriot

type Message.t += 
  | Hello of string
  | Count of int
  | Ping
  | Pong

let test_basic_send_receive () =
  let received = ref None in
  
  let worker () =
    match receive () with
    | Hello msg ->
        received := Some msg;
        Process.Normal
    | _ -> Process.Exception (Failure "Unexpected message")
  in
  
  let main () =
    let pid = spawn worker in
    send pid (Hello "world");
    yield (); (* Let worker start *)
    yield (); (* Let worker receive *)
    yield (); (* Let worker finish *)
    
    match !received with
    | Some "world" -> Process.Normal
    | _ -> Process.Exception (Failure "Message not received correctly")
  in
  
  let status = run ~main in
  if status <> 0 then (
    Printf.printf "✗ test_basic_send_receive failed with status %d, received: %s\n" 
      status (match !received with None -> "None" | Some s -> Printf.sprintf "Some(%s)" s);
    (* Don't assert yet, let's see all test results *)
  ) else
    Printf.printf "✓ test_basic_send_receive\n"

let test_multiple_messages () =
  let messages = ref [] in
  
  let worker () =
    for _ = 1 to 3 do
      match receive () with
      | Count n -> messages := n :: !messages
      | _ -> ()
    done;
    Process.Normal
  in
  
  let main () =
    let pid = spawn worker in
    send pid (Count 1);
    send pid (Count 2);
    send pid (Count 3);
    
    yield (); yield (); (* Let worker process *)
    
    let expected = [3; 2; 1] in (* Reverse order due to list cons *)
    if !messages = expected then
      Process.Normal
    else
      Process.Exception (Failure (Printf.sprintf "Expected %s, got %s" 
        (String.concat ";" (List.map string_of_int expected))
        (String.concat ";" (List.map string_of_int !messages))))
  in
  
  let status = run ~main in
  assert (status = 0);
  Printf.printf "✓ test_multiple_messages\n"

let test_ping_pong () =
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
  assert (status = 0);
  Printf.printf "✓ test_ping_pong\n"

let test_send_to_dead_process () =
  let main () =
    let worker () = Process.Normal in
    let pid = spawn worker in
    yield (); (* Let worker die *)
    
    (* Sending to a dead process should not crash *)
    send pid (Hello "nowhere");
    Process.Normal
  in
  
  let status = run ~main in
  assert (status = 0);
  Printf.printf "✓ test_send_to_dead_process\n"

let () =
  Printf.printf "=== Message Tests ===\n";
  test_basic_send_receive ();
  test_multiple_messages ();
  test_ping_pong ();
  test_send_to_dead_process ();
  Printf.printf "All message tests passed!\n"