(** `Error.t` is the package-wide typed envelope over module-local errors. *)
type t =
  | Async of Async.error
  | Env of Env.error
  | FsFile of Fs.File.error
  | FsReadDir of Fs.ReadDir.error
  | FsEvents of Fs.Events.error
  | NetAddr of Net.Addr.error
  | NetIpAddr of Net.IpAddr.error
  | NetSocketAddr of Net.SocketAddr.error
  | NetTcpListener of Net.TcpListener.error
  | NetTcpStream of Net.TcpStream.error
  | NetUnixStream of Net.UnixStream.error
  | NetUdpSocket of Net.UdpSocket.error
  | Process of Process.error
  | TimeSystemTime of Time.SystemTime.error
  | TimeMonotonic of Time.Monotonic.error
  | TimeTimer of Time.Timer.error

val from_async: Async.error -> t

val from_env: Env.error -> t

val from_fs_file: Fs.File.error -> t

val from_fs_read_dir: Fs.ReadDir.error -> t

val from_fs_events: Fs.Events.error -> t

val from_net_addr: Net.Addr.error -> t

val from_net_ip_addr: Net.IpAddr.error -> t

val from_net_socket_addr: Net.SocketAddr.error -> t

val from_net_tcp_listener: Net.TcpListener.error -> t

val from_net_tcp_stream: Net.TcpStream.error -> t

val from_net_unix_stream: Net.UnixStream.error -> t

val from_net_udp_socket: Net.UdpSocket.error -> t

val from_process: Process.error -> t

val from_time_system_time: Time.SystemTime.error -> t

val from_time_monotonic: Time.Monotonic.error -> t

val from_time_timer: Time.Timer.error -> t

(** Stable module-oriented tag for the wrapped error. *)
val module_name: t -> string

(**
   Extract the shared system error when the wrapped module error is rooted in a
   `SystemError.t`.
*)
val system: t -> System_error.t option

val to_string: t -> string
