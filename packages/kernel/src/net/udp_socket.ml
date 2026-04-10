open Global0
open IO
open Async

type t = Socket.datagram_socket

let close = Socket.close

let rec retry_eintr = fun fn ->
  try fn () with
  | Unix.(Unix_error (EINTR, _, _)) -> retry_eintr fn

let bind = fun ?(reuse_addr = true) ?(reuse_port = false) addr ->
  try
    let sock_domain = Addr.to_domain addr in
    let sock_type, sock_addr = Addr.to_unix addr in
    let fd = Socket.make sock_domain sock_type in
    if reuse_addr then
      Unix.setsockopt (Fd.to_unix fd) Unix.SO_REUSEADDR true;
    if reuse_port then
      Unix.setsockopt (Fd.to_unix fd) Unix.SO_REUSEPORT true;
    retry_eintr
      (fun () ->
        Unix.bind (Fd.to_unix fd) sock_addr);
    Ok fd
  with
  | Unix.Unix_error (err, _, _) -> Error (IO.error_of_unix err)

let connect = fun fd addr ->
  try
    let _sock_type, sock_addr = Addr.to_unix addr in
    retry_eintr
      (fun () ->
        Unix.connect (Fd.to_unix fd) sock_addr);
    Ok ()
  with
  | Unix.Unix_error (err, _, _) -> Error (IO.error_of_unix err)

let local_addr = fun fd ->
  try
    let sockaddr =
      retry_eintr (fun () -> Unix.getsockname (Fd.to_unix fd))
    in
    Ok (Addr.of_unix_datagram sockaddr)
  with
  | Unix.Unix_error (err, _, _) -> Error (IO.error_of_unix err)

let recv = fun fd ?(pos = 0) ?len buf ->
  let len = Option.unwrap_or len ~default:(Bytes.length buf - pos) in
  try
    Ok (
      retry_eintr
        (fun () ->
          Unix.recv (Fd.to_unix fd) buf pos len [])
    )
  with
  | Unix.Unix_error (err, _, _) -> Error (IO.error_of_unix err)

let recv_from = fun fd ?(pos = 0) ?len buf ->
  let len = Option.unwrap_or len ~default:(Bytes.length buf - pos) in
  try
    let bytes_read, from_addr =
      retry_eintr
        (fun () ->
          Unix.recvfrom (Fd.to_unix fd) buf pos len [])
    in
    Ok (bytes_read, Addr.of_unix_datagram from_addr)
  with
  | Unix.Unix_error (err, _, _) -> Error (IO.error_of_unix err)

let send = fun fd ?(pos = 0) ?len buf ->
  let len = Option.unwrap_or len ~default:(Bytes.length buf - pos) in
  try
    Ok (
      retry_eintr
        (fun () ->
          Unix.send (Fd.to_unix fd) buf pos len [])
    )
  with
  | Unix.Unix_error (err, _, _) -> Error (IO.error_of_unix err)

let send_to = fun fd addr ?(pos = 0) ?len buf ->
  let len = Option.unwrap_or len ~default:(Bytes.length buf - pos) in
  try
    let _sock_type, sock_addr = Addr.to_unix addr in
    Ok (
      retry_eintr
        (fun () ->
          Unix.sendto (Fd.to_unix fd) buf pos len [] sock_addr)
    )
  with
  | Unix.Unix_error (err, _, _) -> Error (IO.error_of_unix err)

let to_source = fun t ->
  let module Src = struct
    type nonrec t = t

    let register = fun t selector token interest ->
      Adapter.Selector.register selector ~fd:t ~token ~interest

    let reregister = fun t selector token interest ->
      Adapter.Selector.reregister selector ~fd:t ~token ~interest

    let deregister = fun t selector -> Adapter.Selector.deregister selector ~fd:t
  end in
  Source.make (module Src) t
