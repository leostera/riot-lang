(** UDP server convenience wrapper *)
open Global
open IO

type handler = socket:Udp_socket.t -> from:Addr.datagram_addr -> bytes -> len:int -> unit

type t = {
  socket: Udp_socket.t;
  buffer_size: int;
  handler: handler;
}

type error =
  | System_error of IO.error

let bind = fun ?(reuse_addr = true) ?(reuse_port = false) ?(buffer_size = 65_535) addr ~handler ->
  match Udp_socket.bind ~reuse_addr ~reuse_port addr with
  | Ok socket -> Ok { socket; buffer_size; handler }
  | Error (Udp_socket.System_error err) -> Error (System_error err)

let socket = fun t -> t.socket

let local_addr = fun t -> Udp_socket.local_addr t.socket

let close = fun t -> Udp_socket.close t.socket

let serve = fun t ->
  let rec loop () =
    let buffer = Bytes.create t.buffer_size in
    match Udp_socket.recv_from t.socket buffer () with
    | Ok { bytes_read; from } ->
        let payload = Bytes.sub buffer 0 bytes_read in
        ignore
          (
            Runtime.spawn
              (fun () ->
                t.handler ~socket:t.socket ~from payload ~len:bytes_read;
                Ok ())
          );
        loop ()
    | Error (Udp_socket.System_error err) -> Error (System_error err)
  in
  loop ()

let listen = fun ?(reuse_addr = true) ?(reuse_port = false) ?(buffer_size = 65_535) addr ~handler ->
  match bind ~reuse_addr ~reuse_port ~buffer_size addr ~handler with
  | Ok server -> serve server
  | Error err -> Error err
