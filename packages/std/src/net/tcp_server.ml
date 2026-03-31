(** TCP server that manages a listener and handles line-based protocols *)
open Global
open IO

type handler = req:string -> Kernel.Net.Tcp_stream.t -> unit

(** Handler receives request string and stream for responses *)
type t = {
  listener: Tcp_listener.t;
  handler: handler;
}

type error =
  | Connection_refused
  | Closed
  | System_error of IO.error

let read_line = fun (stream: Kernel.Net.Tcp_stream.t) ->
  let buffer = Bytes.create 4_096 in
  let rec loop = fun acc ->
    match Tcp_stream.read stream buffer () with
    | Error _ ->
        Error "Failed to read from stream"
    | Ok 0 ->
        Error "Connection closed"
    | Ok n -> (
        let data = Bytes.sub_string buffer 0 n in
        let combined = acc ^ data in
        (* Look for newline *)
        match String.index_opt combined '\n' with
        | Some idx ->
            let line = String.sub combined 0 idx in
            Ok line
        | None -> loop combined
      )
  in
  loop ""

let rec accept_loop = fun t ->
  (* Note: Can't use Log module here as it's not available in Global *)
  (* println "[TCP_SERVER] Awaiting next connection..."; *)
  match Tcp_listener.accept t.listener with
  | Error Tcp_listener.Closed ->
      (* println "[TCP_SERVER] accept() failed, server stopping"; *)
      Error Closed
  | Error (Tcp_listener.System_error s) ->
      Error (System_error s)
  | Error Tcp_listener.Connection_refused ->
      Error Connection_refused
  | Ok (stream, _client_addr) ->
      (* println "[TCP_SERVER] Connection accepted, spawning handler"; *)
      let _connection_pid =
        Miniriot.spawn
          (fun () ->
            (* Read lines in a loop using the read_line helper *)
            let rec connection_loop = fun () ->
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

let listen = fun ?(reuse_addr = true) ?(reuse_port = false) ?(backlog = 128) addr ~handler ->
  match Tcp_listener.bind ~reuse_addr ~reuse_port ~backlog addr with
  | Error Tcp_listener.Closed ->
      Error Closed
  | Error (Tcp_listener.System_error s) ->
      Error (System_error s)
  | Error Tcp_listener.Connection_refused ->
      Error Connection_refused
  | Ok listener ->
      let server = {listener; handler} in
      accept_loop server

let close = fun t -> Tcp_listener.close t.listener
