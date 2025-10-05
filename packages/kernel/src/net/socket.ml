open Async

type 'kind socket = Fd.t
type listen_socket = [ `listen ] socket
type stream_socket = [ `stream ] socket

let to_string t = Fd.to_string t
let close t = Unix.close t

let make sock_domain sock_type =
  let fd = Unix.socket ~cloexec:true sock_domain sock_type 0 in
  Unix.set_nonblock fd;
  Fd.make fd
