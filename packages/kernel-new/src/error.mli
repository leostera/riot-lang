module System = System_error

type t =
  | Async of Async.error
  | Env of Env.error
  | Fs_file of Fs.File.error
  | Net_ip_addr of Net.IpAddr.error
  | Net_socket_addr of Net.SocketAddr.error
  | Net_tcp_listener of Net.TcpListener.error
  | Net_tcp_stream of Net.TcpStream.error
  | Net_udp_socket of Net.UdpSocket.error
  | Process of Process.error
  | Time_system_time of Time.SystemTime.error
  | Time_monotonic of Time.Monotonic.error

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

val to_string: t -> string

val panic: string -> 'a
