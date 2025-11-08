(** TCP server that manages a listener and handles line-based protocols *)

open Global
open IO

type handler = req:string -> Kernel.Net.Tcp_stream.t -> unit
(** Handler receives request string and stream for responses *)

type t = { listener : Tcp_listener.t; handler : handler }
type error = [ `Connection_refused | `Closed | `System_error of string ]

let read_line (stream : Kernel.Net.Tcp_stream.t) =
  let buffer = Bytes.create 4096 in
  let rec loop acc =
    match Tcp_stream.read stream buffer () with
    | Error _ -> Error "Failed to read from stream"
    | Ok 0 -> Error "Connection closed"
    | Ok n -> (
        let data = Bytes.sub_string buffer 0 n in
        let combined = acc ^ data in
        (* Look for newline *)
        match String.index_opt combined '\n' with
        | Some idx ->
            let line = String.sub combined 0 idx in
            Ok line
        | None -> loop combined)
  in
  loop ""

let rec accept_loop t =
  (* Note: Can't use Log module here as it's not available in Global *)
  (* println "[TCP_SERVER] Awaiting next connection..."; *)
  match Tcp_listener.accept t.listener with
  | Error e ->
      (* println "[TCP_SERVER] accept() failed, server stopping"; *)
      Error e
  | Ok (stream, _client_addr) ->
      (* println "[TCP_SERVER] Connection accepted, spawning handler"; *)
      let _connection_pid =
        Miniriot.spawn (fun () ->
            (* Read lines in a loop using the read_line helper *)
            let rec connection_loop () =
              match read_line stream with
              | Ok req ->
                  (* Call handler with request string and stream *)
                  t.handler ~req stream;
                  (* println "[TCP_SERVER] Handler returned, reading next line on same connection"; *)
                  connection_loop ()
              | Error _ ->
                  (* Connection closed, clean up *)
                  (* println "[TCP_SERVER] Connection closed, cleaning up"; *)
                  Tcp_stream.close stream;
                  Ok ()
            in
            connection_loop ())
      in
      accept_loop t

let listen ?(reuse_addr = true) ?(reuse_port = false) ?(backlog = 128) addr
    ~handler =
  match Tcp_listener.bind ~reuse_addr ~reuse_port ~backlog addr with
  | Error e -> Error e
  | Ok listener ->
      let result = 
        try accept_loop { listener; handler }
        with _ -> Error `Closed
      in
      Tcp_listener.close listener;
      result
