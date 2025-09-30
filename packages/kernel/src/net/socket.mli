type 'kind socket = Async.Fd.t
type listen_socket = [ `listen ] socket
type stream_socket = [ `stream ] socket

val pp : Format.formatter -> 'kind socket -> unit
val close : 'kind socket -> unit
val make : Unix.socket_domain -> Unix.socket_type -> 'kind socket