(** TCP listener for tusk server 
    
    Uses Miniriot.Net for actor-friendly networking operations that
    properly integrate with the scheduler's I/O polling. *)

open Miniriot

(** Extend Message.t with our custom messages *)
type Message.t += 
  | ClientRequest of Pid.t * Rpc.request
  | ServerResponse of Rpc.response

(** Default port for tusk server *)
let default_port = 9876

(** Find an available port starting from the default *)
let find_available_port () =
  (* Simply return the default port - if binding fails, we'll handle it gracefully *)
  default_port

(** Write port file for clients to discover the server *)
let write_port_file port =
  let home = Sys.getenv "HOME" in
  let tusk_dir = Filename.concat home ".tusk" in
  let port_file = Filename.concat tusk_dir "server.port" in

  (* Ensure .tusk directory exists *)
  (try Unix.mkdir tusk_dir 0o755
   with Unix.Unix_error (Unix.EEXIST, _, _) -> ());

  (* Write port number *)
  let oc = open_out port_file in
  output_string oc (string_of_int port);
  close_out oc;
  Printf.printf "[Listener] Server port written to %s\n%!" port_file

(** Read port file to connect to existing server *)
let read_port_file () =
  let home = Sys.getenv "HOME" in
  let port_file = Filename.concat home ".tusk/server.port" in
  if Sys.file_exists port_file then (
    let ic = open_in port_file in
    let port = int_of_string (input_line ic) in
    close_in ic;
    Some port)
  else None

(** Remove port file on shutdown *)
let remove_port_file () =
  let home = Sys.getenv "HOME" in
  let port_file = Filename.concat home ".tusk/server.port" in
  try Sys.remove port_file with _ -> ()

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
            Printf.printf "[Listener] Received: %s\n%!" line;

            (* Parse request *)
            match Rpc.request_of_string line with
            | Some request -> (
                (* Forward to server *)
                send server_pid (ClientRequest (self (), request));

                (* Wait for response - TODO: add timeout *)
                match receive () with
                | ServerResponse response -> (
                    let response_str = Rpc.response_to_string response in
                    let response_bytes = Bytes.of_string (response_str ^ "\n") in
                    match
                      Net.TcpStream.write stream response_bytes ()
                    with
                    | Ok _ -> (
                        (* Continue or shutdown *)
                        match request with
                        | Rpc.Shutdown ->
                            Printf.printf
                              "[Listener] Client requested shutdown\n%!";
                            ()
                        | _ -> client_loop ())
                    | Error _ ->
                        Printf.printf "[Listener] Write error\n%!")
                | _ ->
                    let error_bytes = Bytes.of_string "Error:Timeout\n" in
                    ignore (Net.TcpStream.write stream error_bytes ());
                    client_loop ())
            | None ->
                let error_bytes = Bytes.of_string "Error:Invalid request\n" in
                ignore (Net.TcpStream.write stream error_bytes ());
                client_loop ())
        | Ok 0 -> Printf.printf "[Listener] Client disconnected\n%!"
        | Ok _ -> Printf.printf "[Listener] Partial read\n%!"; client_loop ()
        | Error _ ->
            Printf.printf "[Listener] Read error\n%!"
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
