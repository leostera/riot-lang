(** TCP listener for tusk server - provides JSON-RPC over TCP *)

open Miniriot

(** Default port range for tusk servers *)
let port_range_start = 9876

let port_range_end = 9976

(** Find an available port starting from the default *)
let find_available_port () =
  let rec try_port port =
    if port > port_range_end then port_range_start
    else
      try
        let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
        Unix.setsockopt sock Unix.SO_REUSEADDR true;
        Unix.bind sock (Unix.ADDR_INET (Unix.inet_addr_loopback, port));
        Unix.close sock;
        port
      with _ -> try_port (port + 1)
  in
  try_port port_range_start

(** Write port and PID files for clients to discover the server *)
let write_port_file port =
  let workspace = Workspace.scan ~root:(System.getcwd ()) in
  Server_manager.write_daemon workspace ~port;
  Printf.printf "[Listener] Server daemon files written for port %d\n%!" port

(** Read port file to connect to existing server *)
let read_port_file () =
  let workspace = Workspace.scan ~root:(System.getcwd ()) in
  match Server_manager.daemon workspace with
  | Some d -> Some d.port
  | None -> None

(** Remove port and PID files on shutdown *)
let remove_port_file () =
  let workspace = Workspace.scan ~root:(System.getcwd ()) in
  Server_manager.remove_daemon workspace

(** Start the TCP listener *)
let start server_pid =
  let port = find_available_port () in
  write_port_file port;

  (* Handler creates JSON-RPC server, processes message, and sends response *)
  let handler ~req stream =
    Printf.eprintf "[LISTENER DEBUG] Creating JSON-RPC server for request\n";
    flush stderr;
    let jsonrpc_server = Tusk_rpc_server.create server_pid in
    let reply response =
      let response_json = Jsonrpc.response_to_json response in
      let response_str = Json.to_string response_json in
      let _ = Net.TcpServer.send stream (response_str ^ "\n") in
      ()
    in
    Printf.eprintf "[LISTENER DEBUG] Handling JSON-RPC message: %s\n" req;
    flush stderr;
    Jsonrpc.Server.handle_message jsonrpc_server reply req
  in

  match Net.TcpServer.create (Net.Addr.tcp Net.Addr.loopback port) ~handler with
  | Error _ ->
      Printf.printf "[Listener] Failed to start server on port %d\n%!" port;
      remove_port_file ();
      Process.Exception (Failure "Failed to start server")
  | Ok server -> (
      Printf.printf "[Listener] Server listening on port %d\n%!" port;
      (* Listen loop - TcpServer handles everything and will run forever *)
      match Net.TcpServer.listen server with
      | Ok () -> Process.Normal
      | Error _ ->
          remove_port_file ();
          Process.Normal)

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
