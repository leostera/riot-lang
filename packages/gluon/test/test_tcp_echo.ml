(** Simplified TCP echo server test *)

open Gluon

let test_tcp_echo_server () =
  Format.printf "\nTesting TCP echo server:\n";
  
  let poll = match Gluon.create () with
    | Error _ ->
        Format.printf "✗ Failed to create kqueue\n";
        exit 1
    | Ok p -> p
  in
  
  (* Start server *)
  let addr = Net.Addr.tcp Net.Addr.loopback 0 in
  
  let listener = match Net.TcpListener.bind addr with
    | Error _ ->
        Format.printf "✗ Failed to bind\n";
        exit 1
    | Ok l -> l
  in
  
  (* Get actual port *)
  let server_addr = 
    match Unix.getsockname listener with
    | Unix.ADDR_INET (addr, port) -> 
        Net.Addr.tcp (Unix.string_of_inet_addr addr) port
    | _ -> failwith "Unexpected address type"
  in
  
  Format.printf "✓ Server listening on %a\n" Net.Addr.pp server_addr;
  
  (* Register listener *)
  let listener_token = Token.make "listener" in
  let _ = Gluon.register poll ~fd:listener ~token:listener_token ~interests:Interest.readable in
  
  (* Connect client *)
  let client = match Net.TcpStream.connect server_addr with
    | Error _ ->
        Format.printf "✗ Failed to connect\n";
        Net.TcpListener.close listener;
        exit 1
    | Ok client_status ->
        match client_status with
        | `Connected c | `In_progress c -> c
  in
  
  Format.printf "✓ Client connected\n";
  
  (* Poll for accept *)
  let events = match Gluon.poll ~timeout:100 poll with
    | Error _ ->
        Format.printf "✗ Poll failed\n";
        exit 1
    | Ok e -> e
  in
  
  if Array.length events = 0 || not (Event.is_readable events.(0)) then begin
    Format.printf "✗ No accept event\n";
    Net.TcpStream.close client;
    Net.TcpListener.close listener;
    exit 1
  end;
  
  (* Accept connection *)
  let (server_client, client_addr) = match Net.TcpListener.accept listener with
    | Error _ ->
        Format.printf "✗ Accept failed\n";
        exit 1
    | Ok x -> x
  in
  
  Format.printf "✓ Accepted connection from %a\n" Net.Addr.pp client_addr;
  
  (* Register server's client socket *)
  let server_token = Token.make "server_client" in
  let _ = Gluon.register poll ~fd:server_client ~token:server_token ~interests:Interest.readable in
  
  (* Send data from client *)
  let msg = "Hello, echo server!" in
  let _ = Net.TcpStream.write client (Bytes.of_string msg) in
  Format.printf "✓ Client sent: %S\n" msg;
  
  (* Poll for data *)
  let events = match Gluon.poll ~timeout:100 poll with
    | Error _ ->
        Format.printf "✗ Poll failed\n";
        exit 1
    | Ok e -> e
  in
  
  let found_server_event = Array.exists (fun e -> 
    Event.is_readable e && Token.equal ~eq:(=) (Event.token e) server_token
  ) events in
  
  if not found_server_event then begin
    Format.printf "✗ No data to read on server\n";
    exit 1
  end;
  
  (* Read on server *)
  let buf = Bytes.create 100 in
  let received = match Net.TcpStream.read server_client buf with
    | Error _ ->
        Format.printf "✗ Server read failed\n";
        exit 1
    | Ok n ->
        let s = Bytes.sub_string buf 0 n in
        Format.printf "✓ Server received: %S\n" s;
        s
  in
  
  (* Echo back *)
  let _ = Net.TcpStream.write server_client (Bytes.of_string received) in
  
  (* Register client for reading *)
  let client_token = Token.make "client" in
  let _ = Gluon.register poll ~fd:client ~token:client_token ~interests:Interest.readable in
  
  (* Poll for echo *)
  let events = match Gluon.poll ~timeout:100 poll with
    | Error _ ->
        Format.printf "✗ Poll failed\n";
        exit 1
    | Ok e -> e
  in
  
  let found_client_event = Array.exists (fun e ->
    Event.is_readable e && Token.equal ~eq:(=) (Event.token e) client_token
  ) events in
  
  if not found_client_event then begin
    Format.printf "✗ No echo received\n";
    exit 1
  end;
  
  (* Read echo on client *)
  let buf = Bytes.create 100 in
  let echoed = match Net.TcpStream.read client buf with
    | Error _ ->
        Format.printf "✗ Client read failed\n";
        exit 1
    | Ok n ->
        let s = Bytes.sub_string buf 0 n in
        Format.printf "✓ Client received echo: %S\n" s;
        s
  in
  
  (* Cleanup *)
  Net.TcpStream.close client;
  Net.TcpStream.close server_client;
  Net.TcpListener.close listener;
  
  (* Check result *)
  if echoed = msg then begin
    Format.printf "✓ Echo test passed\n";
    true
  end else begin
    Format.printf "✗ Echo mismatch: expected %S, got %S\n" msg echoed;
    false
  end

let () =
  Format.printf "Gluon TCP Echo Test\n";
  Format.printf "===================\n";
  
  let result = test_tcp_echo_server () in
  exit (if result then 0 else 1)