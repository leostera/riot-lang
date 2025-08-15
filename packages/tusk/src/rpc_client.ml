(** RPC client for communicating with tusk server *)

open Miniriot

(** Get project ID based on current working directory *)
let get_project_id () =
  let cwd = System.getcwd () in
  (* Hash the path to get a stable project ID *)
  let hash = Hasher.hash_string cwd in
  Hasher.to_string hash

(** Get daemon directory for this project *)
let daemon_dir () =
  let home = System.get_home () in
  let project_id = get_project_id () in
  Printf.sprintf "%s/.tusk/daemons/%s" home project_id

(** Connect to the tusk server *)
let connect () =
  (* Read server port for this project *)
  let daemon_path = daemon_dir () in
  let port_file = Printf.sprintf "%s/server.port" daemon_path in

  (* Check if port file exists *)
  if not (Sys.file_exists port_file) then
    Error "Server is not running for this project"
  else
    (* Also check PID file to see if server process is still alive *)
    let pid_file = Printf.sprintf "%s/server.pid" daemon_path in
    let server_alive =
      if Sys.file_exists pid_file then
        try
          let ic = open_in pid_file in
          let pid_str = input_line ic in
          close_in ic;
          let pid = int_of_string pid_str in
          (* Check if process is still running by sending signal 0 *)
          try
            Unix.kill pid 0;
            true
          with Unix.Unix_error (Unix.ESRCH, _, _) ->
            (* Process doesn't exist *)
            false
        with _ -> false
      else false
    in

    if not server_alive then (
      (* Clean up stale port file *)
      (try Sys.remove port_file with _ -> ());
      (try Sys.remove pid_file with _ -> ());
      Error "Server is not running for this project")
    else
      let port =
        try
          let ic = open_in port_file in
          let port_str = input_line ic in
          close_in ic;
          int_of_string port_str
        with _ -> 0 (* Invalid port *)
      in

      if port = 0 then Error "Server is not running"
      else
        (* Try to connect to server *)
        let addr =
          match Miniriot.Net.Addr.of_host_and_port ~host:"127.0.0.1" ~port with
          | Ok addr -> addr
          | Error _ -> failwith "Failed to create address"
        in

        match Miniriot.Net.TcpStream.connect addr with
        | Ok stream -> Ok stream
        | Error _ -> Error "Server is not running"

(** Execute an RPC call *)
let call request =
  match connect () with
  | Error msg -> Error msg
  | Ok stream ->
      (* Send request *)
      let request_str = Rpc.request_to_string request in
      let msg = request_str ^ "\n" in
      let buffer = Bytes.of_string msg in

      let result =
        match
          Miniriot.Net.TcpStream.write stream buffer ~pos:0
            ~len:(Bytes.length buffer) ()
        with
        | Ok _ -> (
            (* Read response *)
            let response_buffer = Bytes.create 4096 in
            match
              Miniriot.Net.TcpStream.read stream response_buffer ~pos:0
                ~len:4096 ()
            with
            | Ok bytes_read -> (
                let response_str =
                  Bytes.sub_string response_buffer 0 bytes_read
                in
                let response_str = String.trim response_str in

                match Rpc.response_of_string response_str with
                | Some response -> Ok response
                | None ->
                    (* Try to return the raw string if we can't parse it *)
                    Error (Printf.sprintf "Unknown response: %s" response_str))
            | Error _ -> Error "Failed to read response")
        | Error _ -> Error "Failed to send request"
      in
      (* Close the connection *)
      Miniriot.Net.TcpStream.close stream;
      result

(** Convenience function for ping *)
let ping () = call Rpc.Ping

(** Convenience function for getting workspace info *)
let get_workspace () = call Rpc.GetWorkspace

(** Convenience function for getting build graph *)
let get_build_graph () = call Rpc.GetBuildGraph

(** Convenience function for shutting down server *)
let shutdown () = call Rpc.Shutdown

(** Send a raw command to the server and get response *)
let send_raw stream msg =
  let buffer = Bytes.of_string (msg ^ "\n") in
  match
    Miniriot.Net.TcpStream.write stream buffer ~pos:0 ~len:(Bytes.length buffer)
      ()
  with
  | Ok _ -> (
      (* Read response *)
      let response_buffer = Bytes.create 4096 in
      match
        Miniriot.Net.TcpStream.read stream response_buffer ~pos:0 ~len:4096 ()
      with
      | Ok bytes_read ->
          let response_str = Bytes.sub_string response_buffer 0 bytes_read in
          let response_str = String.trim response_str in
          Ok response_str
      | Error _ -> Error "Failed to read response")
  | Error _ -> Error "Failed to send request"

(** Receive a raw response from the server (for streaming) *)
let receive_raw stream =
  let response_buffer = Bytes.create 4096 in
  match
    Miniriot.Net.TcpStream.read stream response_buffer ~pos:0 ~len:4096 ()
  with
  | Ok bytes_read ->
      let response_str = Bytes.sub_string response_buffer 0 bytes_read in
      let response_str = String.trim response_str in
      Ok response_str
  | Error _ -> Error "Failed to read response"

(** Send a command and get a typed response - for compatibility *)
let send_command stream request =
  let request_str = Rpc.request_to_string request in
  match send_raw stream request_str with
  | Ok response_str -> (
      match Rpc.response_of_string response_str with
      | Some response -> response
      | None -> Rpc.Error { message = "Unknown response: " ^ response_str })
  | Error msg -> Rpc.Error { message = msg }
