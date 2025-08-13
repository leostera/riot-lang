(** TCP listener for tusk server 
    
    Uses Miniriot.Net for actor-friendly networking operations that
    properly integrate with the scheduler's I/O polling. *)

open Miniriot

(** Import shared RPC message types *)
open Rpc_messages

(** Default port range for tusk servers *)
let port_range_start = 9876
let port_range_end = 9976

(** Find an available port starting from the default *)
let find_available_port () =
  (* Try to find an available port in the range *)
  let rec try_port port =
    if port > port_range_end then
      port_range_start  (* Fallback to start of range *)
    else
      (* Try to bind to check if port is available *)
      try
        let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
        Unix.setsockopt sock Unix.SO_REUSEADDR true;
        Unix.bind sock (Unix.ADDR_INET (Unix.inet_addr_loopback, port));
        Unix.close sock;
        port  (* Port is available *)
      with _ ->
        (* Port is in use, try next one *)
        try_port (port + 1)
  in
  try_port port_range_start

(** Get project ID based on current working directory *)
let get_project_id () =
  let cwd = Sys.getcwd () in
  (* Hash the path to get a stable project ID *)
  let hash = Hasher.hash_string cwd in
  Hasher.to_string hash

(** Get daemon directory for this project *)
let daemon_dir () =
  let home = Sys.getenv "HOME" in
  let project_id = get_project_id () in
  Printf.sprintf "%s/.tusk/daemons/%s" home project_id

(** Write port and PID files for clients to discover the server *)
let write_port_file port =
  let daemon_path = daemon_dir () in
  let port_file = Filename.concat daemon_path "server.port" in
  let pid_file = Filename.concat daemon_path "server.pid" in

  (* Ensure daemon directory exists *)
  System.mkdirp daemon_path;

  (* Write port number *)
  let oc = open_out port_file in
  output_string oc (string_of_int port);
  close_out oc;
  Printf.printf "[Listener] Server port written to %s\n%!" port_file;
  
  (* Write PID *)
  let pid = Unix.getpid () in
  let oc = open_out pid_file in
  output_string oc (string_of_int pid);
  close_out oc;
  Printf.printf "[Listener] Server PID written to %s\n%!" pid_file

(** Read port file to connect to existing server *)
let read_port_file () =
  let port_file = Filename.concat (daemon_dir ()) "server.port" in
  if Sys.file_exists port_file then (
    let ic = open_in port_file in
    let port = int_of_string (input_line ic) in
    close_in ic;
    Some port)
  else None

(** Remove port and PID files on shutdown *)
let remove_port_file () =
  let daemon_path = daemon_dir () in
  let port_file = Filename.concat daemon_path "server.port" in
  let pid_file = Filename.concat daemon_path "server.pid" in
  (try Sys.remove port_file with _ -> ());
  (try Sys.remove pid_file with _ -> ())

(** Handle a client connection in a separate process *)
let handle_client server_pid stream =
  Printf.printf "[Listener] Client connected\n%!";

  let rec client_loop () =
        (* Read request - simple line-based protocol for now *)
        let buffer = Bytes.create 1024 in
        match Net.TcpStream.read stream buffer () with
        | Ok bytes_read when bytes_read > 0 -> (
            let line = Bytes.sub_string buffer 0 bytes_read in
            let line = String.trim line in

            (* Parse request *)
            match Rpc.request_of_string line with
            | Some request -> (
                (* Forward to server *)
                send server_pid (ClientRequest (self (), request));

                (* Wait for response - TODO: add timeout *)
                let selector = function
                  | ServerResponse response -> `select (`server_response response)
                  | _ -> `skip
                in
                match receive ~selector () with
                | `server_response response -> (
                    let response_str = Rpc.response_to_string response in
                    let response_bytes = Bytes.of_string (response_str ^ "\n") in
                    match
                      Net.TcpStream.write stream response_bytes ~pos:0 ~len:(Bytes.length response_bytes) ()
                    with
                    | Ok _ -> (
                        (* Continue or shutdown *)
                        match request with
                        | Rpc.Shutdown ->
                            Printf.printf
                              "[Listener] Client requested shutdown\n%!";
                            (* Give time for response to be sent *)
                            sleep 0.1;
                            ()
                        | Rpc.Restart ->
                            Printf.printf
                              "[Listener] Client requested restart\n%!";
                            client_loop ()
                        | _ -> client_loop ())
                    | Error _ ->
                        Printf.printf "[Listener] Write error\n%!"))
            | None ->
                let error_bytes = Bytes.of_string "Error:Invalid request\n" in
                ignore (Net.TcpStream.write stream error_bytes ~pos:0 ~len:(Bytes.length error_bytes) ());
                client_loop ())
        | Ok 0 -> Printf.printf "[Listener] Client disconnected\n%!"
        | Ok _ -> Printf.printf "[Listener] Partial read\n%!"; client_loop ()
        | Error `Closed ->
            Printf.printf "[Listener] Client disconnected\n%!"
        | Error _ ->
            Printf.printf "[Listener] Connection closed\n%!"
  in

  client_loop ();
  Net.TcpStream.close stream

(** Start the TCP listener *)
let start server_pid =
  let port = find_available_port () in
  write_port_file port;

  match Net.TcpListener.bind (Net.Addr.tcp Net.Addr.loopback port) with
  | Error _ ->
      Printf.printf "[Listener] Failed to bind on port %d\n%!" port;
      remove_port_file ();
      Process.Exception (Failure "Failed to bind listener")
  | Ok listener -> (
      Printf.printf "[Listener] Server listening on port %d\n%!" port;

      (* Accept loop - uses Miniriot.Net which handles I/O polling *)
      let rec accept_loop () =
        (* Accept a connection - this will suspend until ready *)
        match Net.TcpListener.accept listener with
        | Ok (stream, _addr) ->
            Printf.printf "[Listener] Accepted connection from client\n%!";
            (* Spawn a new process for each client connection *)
            ignore (spawn (fun () -> 
              handle_client server_pid stream;
              Process.Normal));
            accept_loop ()  (* Continue accepting *)
        | Error _ ->
            Printf.printf "[Listener] Accept error\n%!";
            accept_loop ()
      in
      
      try accept_loop ()
      with exn ->
        Printf.printf "[Listener] Fatal error: %s\n%!" (Printexc.to_string exn);
        remove_port_file ();
        Net.TcpListener.close listener;
        Process.Exception exn)

(** Connect to an existing server *)
let connect () =
  match read_port_file () with
  | Some port -> (
      match Net.TcpStream.connect (Net.Addr.tcp Net.Addr.loopback port) with
      | Ok stream -> Ok stream
      | Error _ -> 
          Printf.printf "[Listener] Failed to connect\n%!";
          Error `Connection_refused)
  | None -> Error `Connection_refused

(** Send a request to the server and get response *)
let send_request request =
  match connect () with
  | Ok stream -> (
      let request_str = Rpc.request_to_string request in
      let request_bytes = Bytes.of_string (request_str ^ "\n") in
      match Net.TcpStream.write stream request_bytes () with
      | Ok _ -> (
          let buffer = Bytes.create 1024 in
          match Net.TcpStream.read stream buffer () with
          | Ok bytes_read when bytes_read > 0 ->
              let response_line = Bytes.sub_string buffer 0 bytes_read in
              Net.TcpStream.close stream;
              Rpc.response_of_string (String.trim response_line)
          | _ ->
              Net.TcpStream.close stream;
              Some (Rpc.Error { message = "Failed to read response" }))
      | Error _ ->
          Printf.printf "[Listener] Failed to send request\n%!";
          Net.TcpStream.close stream;
          Some (Rpc.Error { message = "Failed to send request" }))
  | Error `Connection_refused -> Some (Rpc.Error { message = "Could not connect to server" })
