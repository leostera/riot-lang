(** RPC client for communicating with tusk server *)

open Miniriot

(** Connect to the tusk server *)
let connect () =
  (* Read server port *)
  let home = System.get_home () in
  let port_file = Printf.sprintf "%s/.tusk/server.port" home in
  
  (* Check if port file exists *)
  if not (Sys.file_exists port_file) then
    Error "Server is not running"
  else
    (* Also check PID file to see if server process is still alive *)
    let pid_file = Printf.sprintf "%s/.tusk/server.pid" home in
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
          with Unix.Unix_error(Unix.ESRCH, _, _) ->
            (* Process doesn't exist *)
            false
        with _ -> false
      else
        false
    in
    
    if not server_alive then (
      (* Clean up stale port file *)
      (try Sys.remove port_file with _ -> ());
      (try Sys.remove pid_file with _ -> ());
      Error "Server is not running"
    ) else
      let port = 
        try
          let ic = open_in port_file in
          let port_str = input_line ic in
          close_in ic;
          int_of_string port_str
        with _ ->
          0  (* Invalid port *)
      in
      
      if port = 0 then
        Error "Server is not running"
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
        match Miniriot.Net.TcpStream.write stream buffer ~pos:0 ~len:(Bytes.length buffer) () with
        | Ok _ ->
            (* Read response *)
            let response_buffer = Bytes.create 4096 in
            (match Miniriot.Net.TcpStream.read stream response_buffer ~pos:0 ~len:4096 () with
            | Ok bytes_read ->
                let response_str = Bytes.sub_string response_buffer 0 bytes_read in
                let response_str = String.trim response_str in
                
                (match Rpc.response_of_string response_str with
                | Some response -> Ok response
                | None -> 
                    (* Try to return the raw string if we can't parse it *)
                    Error (Printf.sprintf "Unknown response: %s" response_str))
            | Error _ ->
                Error "Failed to read response")
        | Error _ ->
            Error "Failed to send request"
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