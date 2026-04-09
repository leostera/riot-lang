type t =
  | Async of Async.error
  | Env of Env.error
  | FsFile of Fs.File.error
  | NetIpAddr of Net.IpAddr.error
  | NetSocketAddr of Net.SocketAddr.error
  | NetTcpListener of Net.TcpListener.error
  | NetTcpStream of Net.TcpStream.error
  | NetUdpSocket of Net.UdpSocket.error
  | Process of Process.error
  | TimeSystemTime of Time.SystemTime.error
  | TimeMonotonic of Time.Monotonic.error
  | TimeTimer of Time.Timer.error
val of_async: Async.error -> t

val of_env: Env.error -> t

val of_fs_file: Fs.File.error -> t

val of_net_ip_addr: Net.IpAddr.error -> t

val of_net_socket_addr: Net.SocketAddr.error -> t

val of_net_tcp_listener: Net.TcpListener.error -> t

val of_net_tcp_stream: Net.TcpStream.error -> t

val of_net_udp_socket: Net.UdpSocket.error -> t

val of_process: Process.error -> t

val of_time_system_time: Time.SystemTime.error -> t

val of_time_monotonic: Time.Monotonic.error -> t

val of_time_timer: Time.Timer.error -> t

(** Stable module-oriented tag for the wrapped error. *)
val module_name: t -> string

(** Extract the shared system error when the wrapped module error is rooted in a
    [SystemError.t]. *)
val system: t -> System_error.t option

val to_string: t -> string
