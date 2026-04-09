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
  | Time_timer of Time.Timer.error

let of_async = fun error -> Async error

let of_env = fun error -> Env error

let of_fs_file = fun error -> Fs_file error

let of_net_ip_addr = fun error -> Net_ip_addr error

let of_net_socket_addr = fun error -> Net_socket_addr error

let of_net_tcp_listener = fun error -> Net_tcp_listener error

let of_net_tcp_stream = fun error -> Net_tcp_stream error

let of_net_udp_socket = fun error -> Net_udp_socket error

let of_process = fun error -> Process error

let of_time_system_time = fun error -> Time_system_time error

let of_time_monotonic = fun error -> Time_monotonic error

let of_time_timer = fun error -> Time_timer error

let to_string = function
  | Async error -> Async.error_to_string error
  | Env error -> Env.error_to_string error
  | Fs_file error -> Fs.File.error_to_string error
  | Net_ip_addr error -> Net.IpAddr.error_to_string error
  | Net_socket_addr error -> Net.SocketAddr.error_to_string error
  | Net_tcp_listener error -> Net.TcpListener.error_to_string error
  | Net_tcp_stream error -> Net.TcpStream.error_to_string error
  | Net_udp_socket error -> Net.UdpSocket.error_to_string error
  | Process error -> Process.error_to_string error
  | Time_system_time error -> Time.SystemTime.error_to_string error
  | Time_monotonic error -> Time.Monotonic.error_to_string error
  | Time_timer error -> Time.Timer.error_to_string error

let panic = System_error.panic
