(** UDP socket for datagram-oriented networking *)
open Global
open IO
open Kernel.Async

type t = Kernel.Net.Udp_socket.t

type error =
  | System_error of IO.error

type recv_result = {
  bytes_read: int;
  from: Kernel.Net.Addr.datagram_addr;
}

let bind = fun ?(reuse_addr = true) ?(reuse_port = false) addr ->
  match Kernel.Net.Udp_socket.bind ~reuse_addr ~reuse_port addr with
  | Ok socket -> Ok socket
  | Error err -> Error (System_error err)

let connect = fun socket addr ->
  match Kernel.Net.Udp_socket.connect socket addr with
  | Ok () -> Ok ()
  | Error err -> Error (System_error err)

let recv = fun socket buffer ?(pos = 0) ?len ?timeout () ->
  let len =
    match len with
    | None -> Bytes.length buffer - pos
    | Some requested -> requested
  in
  let source = Kernel.Net.Udp_socket.to_source socket in
  let timeout = Option.map Time.Duration.to_secs_float timeout in
  let rec recv_loop () =
    match Kernel.Net.Udp_socket.recv socket buffer ~pos ~len with
    | Ok bytes_read -> Ok bytes_read
    | Error IO.Operation_would_block
    | Error IO.Resource_unavailable_try_again -> Runtime.syscall
      ?timeout
      ~name:"UdpSocket.recv"
      ~interest:Interest.readable
      ~source
      recv_loop
    | Error err -> Error (System_error err)
  in
  recv_loop ()

let recv_from = fun socket buffer ?(pos = 0) ?len ?timeout () ->
  let len =
    match len with
    | None -> Bytes.length buffer - pos
    | Some requested -> requested
  in
  let source = Kernel.Net.Udp_socket.to_source socket in
  let timeout = Option.map Time.Duration.to_secs_float timeout in
  let rec recv_loop () =
    match Kernel.Net.Udp_socket.recv_from socket buffer ~pos ~len with
    | Ok (bytes_read, from) -> Ok { bytes_read; from }
    | Error IO.Operation_would_block
    | Error IO.Resource_unavailable_try_again -> Runtime.syscall
      ?timeout
      ~name:"UdpSocket.recv_from"
      ~interest:Interest.readable
      ~source
      recv_loop
    | Error err -> Error (System_error err)
  in
  recv_loop ()

let send = fun socket buffer ?(pos = 0) ?len () ->
  let len =
    match len with
    | None -> Bytes.length buffer - pos
    | Some requested -> requested
  in
  let source = Kernel.Net.Udp_socket.to_source socket in
  let rec send_loop () =
    match Kernel.Net.Udp_socket.send socket buffer ~pos ~len with
    | Ok bytes_written -> Ok bytes_written
    | Error IO.Operation_would_block
    | Error IO.Resource_unavailable_try_again -> Runtime.syscall
      ~name:"UdpSocket.send"
      ~interest:Interest.writable
      ~source
      send_loop
    | Error err -> Error (System_error err)
  in
  send_loop ()

let send_to = fun socket addr buffer ?(pos = 0) ?len () ->
  let len =
    match len with
    | None -> Bytes.length buffer - pos
    | Some requested -> requested
  in
  let source = Kernel.Net.Udp_socket.to_source socket in
  let rec send_loop () =
    match Kernel.Net.Udp_socket.send_to socket addr buffer ~pos ~len with
    | Ok bytes_written -> Ok bytes_written
    | Error IO.Operation_would_block
    | Error IO.Resource_unavailable_try_again -> Runtime.syscall
      ~name:"UdpSocket.send_to"
      ~interest:Interest.writable
      ~source
      send_loop
    | Error err -> Error (System_error err)
  in
  send_loop ()

let local_addr = fun socket ->
  match Kernel.Net.Udp_socket.local_addr socket with
  | Ok addr -> addr
  | Error err -> panic
    (format Format.[ str "UdpSocket.local_addr failed: "; str (IO.error_message err) ])

let close = Kernel.Net.Udp_socket.close
