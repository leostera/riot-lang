open Async

type t = Socket.listen_socket

let pp = Socket.pp
let close = Socket.close

let bind ?(reuse_addr = true) ?(reuse_port = true) ?(backlog = 128) addr =
  syscall @@ fun () ->
  let sock_domain = Addr.to_domain addr in
  let sock_type, sock_addr = Addr.to_unix addr in
  let fd = Socket.make sock_domain sock_type in
  Unix.setsockopt fd Unix.SO_REUSEADDR reuse_addr;
  Unix.setsockopt fd Unix.SO_REUSEPORT reuse_port;
  Unix.bind fd sock_addr;
  Unix.listen fd backlog;
  Ok fd

let accept fd =
  syscall @@ fun () ->
  let raw_fd, client_addr = Unix.accept ~cloexec:true fd in
  Unix.set_nonblock raw_fd;
  Unix.setsockopt raw_fd Unix.TCP_NODELAY true;
  let addr = Addr.of_unix client_addr in
  let stream = Tcp_stream.of_fd (Fd.make raw_fd) in
  Ok (stream, addr)

let to_source t =
  let module Src = struct
    type nonrec t = t

    let register t selector token interest =
      Adapter.Selector.register selector ~fd:t ~token ~interest

    let reregister t selector token interest =
      Adapter.Selector.reregister selector ~fd:t ~token ~interest

    let deregister t selector = Adapter.Selector.deregister selector ~fd:t
  end in
  Source.make (module Src) t
