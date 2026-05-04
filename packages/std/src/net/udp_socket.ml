(** UDP socket for datagram-oriented networking *)
open Global
open IO
open Kernel.Async

type t = Kernel.Net.UdpSocket.t

type error =
  | System_error of IO.error

type recv_result = {
  bytes_read: int;
  from: Addr.datagram_addr;
}

let io_error_of_udp_error = fun __tmp1 ->
  match __tmp1 with
  | Kernel.Net.UdpSocket.InvalidSlice _ -> IO.Invalid_argument
  | Kernel.Net.UdpSocket.InvalidSocketAddr _ -> IO.Invalid_argument
  | Kernel.Net.UdpSocket.WouldBlock -> IO.Operation_would_block
  | Kernel.Net.UdpSocket.TimedOut -> IO.Connection_timed_out
  | Kernel.Net.UdpSocket.ConnectionRefused -> IO.Connection_refused
  | Kernel.Net.UdpSocket.ConnectionReset -> IO.Connection_reset_by_peer
  | Kernel.Net.UdpSocket.NetworkUnreachable -> IO.Network_is_unreachable
  | Kernel.Net.UdpSocket.NotConnected -> IO.Transport_endpoint_not_connected
  | Kernel.Net.UdpSocket.MessageTooLong -> IO.Message_too_long
  | Kernel.Net.UdpSocket.DestinationAddressRequired -> IO.Destination_address_required
  | Kernel.Net.UdpSocket.AddressInUse -> IO.Address_already_in_use
  | Kernel.Net.UdpSocket.AddressNotAvailable -> IO.Cannot_assign_requested_address
  | Kernel.Net.UdpSocket.System error -> IO.from_system_error error

let bind = fun ?(reuse_addr = true) ?(reuse_port = false) addr ->
  match Kernel.Net.UdpSocket.bind ~reuse_addr ~reuse_port addr with
  | Ok socket -> Ok socket
  | Error err -> Error (System_error (io_error_of_udp_error err))

let connect = fun socket addr ->
  match Kernel.Net.UdpSocket.connect socket addr with
  | Ok () -> Ok ()
  | Error err -> Error (System_error (io_error_of_udp_error err))

let recv = fun socket buffer ?(pos = 0) ?len ?timeout () ->
  let len =
    match len with
    | None -> Bytes.length buffer - pos
    | Some requested -> requested
  in
  let source = Kernel.Net.UdpSocket.to_source socket in
  let timeout = Option.map timeout ~fn:Time.Duration.to_secs_float in
  let rec recv_loop () =
    match Kernel.Net.UdpSocket.recv socket buffer ~pos ~len with
    | Ok bytes_read -> Ok bytes_read
    | Error Kernel.Net.UdpSocket.WouldBlock ->
        Runtime.syscall
          ?timeout
          ~name:"UdpSocket.recv"
          ~interest:Interest.readable
          ~source
          recv_loop
    | Error err -> Error (System_error (io_error_of_udp_error err))
  in
  recv_loop ()

let recv_from = fun socket buffer ?(pos = 0) ?len ?timeout () ->
  let len =
    match len with
    | None -> Bytes.length buffer - pos
    | Some requested -> requested
  in
  let source = Kernel.Net.UdpSocket.to_source socket in
  let timeout = Option.map timeout ~fn:Time.Duration.to_secs_float in
  let rec recv_loop () =
    match Kernel.Net.UdpSocket.recv_from socket buffer ~pos ~len with
    | Ok (bytes_read, from) -> Ok { bytes_read; from }
    | Error Kernel.Net.UdpSocket.WouldBlock ->
        Runtime.syscall
          ?timeout
          ~name:"UdpSocket.recv_from"
          ~interest:Interest.readable
          ~source
          recv_loop
    | Error err -> Error (System_error (io_error_of_udp_error err))
  in
  recv_loop ()

let send = fun socket buffer ?(pos = 0) ?len () ->
  let len =
    match len with
    | None -> Bytes.length buffer - pos
    | Some requested -> requested
  in
  let source = Kernel.Net.UdpSocket.to_source socket in
  let rec send_loop () =
    match Kernel.Net.UdpSocket.send socket buffer ~pos ~len with
    | Ok bytes_written -> Ok bytes_written
    | Error Kernel.Net.UdpSocket.WouldBlock ->
        Runtime.syscall ~name:"UdpSocket.send" ~interest:Interest.writable ~source send_loop
    | Error err -> Error (System_error (io_error_of_udp_error err))
  in
  send_loop ()

let send_to = fun socket addr buffer ?(pos = 0) ?len () ->
  let len =
    match len with
    | None -> Bytes.length buffer - pos
    | Some requested -> requested
  in
  let source = Kernel.Net.UdpSocket.to_source socket in
  let rec send_loop () =
    match Kernel.Net.UdpSocket.send_to socket addr buffer ~pos ~len with
    | Ok bytes_written -> Ok bytes_written
    | Error Kernel.Net.UdpSocket.WouldBlock ->
        Runtime.syscall ~name:"UdpSocket.send_to" ~interest:Interest.writable ~source send_loop
    | Error err -> Error (System_error (io_error_of_udp_error err))
  in
  send_loop ()

let local_addr = fun socket ->
  match Kernel.Net.UdpSocket.local_addr socket with
  | Ok addr -> addr
  | Error err ->
      panic
        (format
          Format.[
            str "UdpSocket.local_addr failed: ";
            str (IO.error_message (io_error_of_udp_error err));
          ])

let close = fun socket ->
  match Kernel.Net.UdpSocket.close socket with
  | Ok () -> ()
  | Error _ -> ()
