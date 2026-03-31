open Async

type 'kind socket = Fd.t

type listen_socket =
  ([
    `listen
  ]) socket

type stream_socket =
  ([
    `stream
  ]) socket

let close = fun t -> Fd.close t

let make = fun sock_domain sock_type ->
    let fd = Unix.socket ~cloexec:true sock_domain sock_type 0 in
    Fd.of_unix fd
