open Global0
open Async

type t = Socket.listen_socket

let close = Socket.close

(* Helper: retry on EINTR *)

let rec retry_eintr = fun fn ->
  try fn () with
  | Unix.(Unix_error (EINTR, _, _)) -> retry_eintr fn

let bind = fun ?(reuse_addr = true) ?(reuse_port = true) ?(backlog = 128) addr ->
  try
    let sock_domain = Addr.to_domain addr in
    let sock_type, sock_addr = Addr.to_unix addr in
    let fd = Socket.make sock_domain sock_type in
    Unix.setsockopt (Fd.to_unix fd) Unix.SO_REUSEADDR reuse_addr;
    Unix.setsockopt (Fd.to_unix fd) Unix.SO_REUSEPORT reuse_port;
    retry_eintr
      (fun () ->
        Unix.bind (Fd.to_unix fd) sock_addr);
    retry_eintr
      (fun () ->
        Unix.listen (Fd.to_unix fd) backlog);
    Ok fd
  with
  | Unix.Unix_error (err, _, _) -> Error (IO.error_of_unix err)

let accept = fun fd ->
  try
    let raw_fd, client_addr =
      retry_eintr (fun () -> Unix.accept ~cloexec:true (Fd.to_unix fd))
    in
    let addr = Addr.of_unix client_addr in
    let stream = Tcp_stream.of_fd (Fd.of_unix raw_fd) in
    Ok (stream, addr)
  with
  | Unix.Unix_error (err, _, _) -> Error (IO.error_of_unix err)

let local_addr = fun fd ->
  try
    let sockaddr =
      retry_eintr (fun () -> Unix.getsockname (Fd.to_unix fd))
    in
    Ok (Addr.of_unix sockaddr)
  with
  | Unix.Unix_error (err, _, _) -> Error (IO.error_of_unix err)

let to_source = fun t ->
  let module Src = struct
    type nonrec t = t

    let register = fun t selector token interest -> Adapter.Selector.register
    selector
    ~fd:t
    ~token
    ~interest

    let reregister = fun t selector token interest -> Adapter.Selector.reregister
    selector
    ~fd:t
    ~token
    ~interest

    let deregister = fun t selector -> Adapter.Selector.deregister selector ~fd:t
  end in
  Source.make (module Src) t
