type 'kind socket = Fd.t
type listen_socket = [ `listen ] socket
type stream_socket = [ `stream ] socket

val to_string : 'kind socket -> string
val close : 'kind socket -> unit
val make : Unix.socket_domain -> Unix.socket_type -> 'kind socket
