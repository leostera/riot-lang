open Prelude

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

let from_async = fun error -> Async error

let from_env = fun error -> Env error

let from_fs_file = fun error -> FsFile error

let from_fs_read_dir = fun error -> FsReadDir error

let from_fs_events = fun error -> FsEvents error

let from_net_addr = fun error -> NetAddr error

let from_net_ip_addr = fun error -> NetIpAddr error

let from_net_socket_addr = fun error -> NetSocketAddr error

let from_net_tcp_listener = fun error -> NetTcpListener error

let from_net_tcp_stream = fun error -> NetTcpStream error

let from_net_unix_stream = fun error -> NetUnixStream error

let from_net_udp_socket = fun error -> NetUdpSocket error

let from_process = fun error -> Process error

let from_time_system_time = fun error -> TimeSystemTime error

let from_time_monotonic = fun error -> TimeMonotonic error

let from_time_timer = fun error -> TimeTimer error

let module_name = fun value ->
  match value with
  | Async _ -> "async"
  | Env _ -> "env"
  | FsFile _ -> "fs.file"
  | FsReadDir _ -> "fs.read_dir"
  | FsEvents _ -> "fs.events"
  | NetAddr _ -> "net.addr"
  | NetIpAddr _ -> "net.ip_addr"
  | NetSocketAddr _ -> "net.socket_addr"
  | NetTcpListener _ -> "net.tcp_listener"
  | NetTcpStream _ -> "net.tcp_stream"
  | NetUnixStream _ -> "net.unix_stream"
  | NetUdpSocket _ -> "net.udp_socket"
  | Process _ -> "process"
  | TimeSystemTime _ -> "time.system_time"
  | TimeMonotonic _ -> "time.monotonic"
  | TimeTimer _ -> "time.timer"

let system = fun value ->
  match value with
  | Async (Async.System error) -> Some error
  | Env (Env.System error) -> Some error
  | FsFile (Fs.File.System error) -> Some error
  | FsReadDir (Fs.ReadDir.File (Fs.File.System error)) -> Some error
  | FsEvents (Fs.Events.System error) -> Some error
  | NetAddr (Net.Addr.System error) -> Some error
  | NetTcpListener (Net.TcpListener.System error) -> Some error
  | NetTcpStream (Net.TcpStream.System error) -> Some error
  | NetUnixStream (Net.UnixStream.System error) -> Some error
  | NetUdpSocket (Net.UdpSocket.System error) -> Some error
  | Process (Process.System error) -> Some error
  | Process (Process.File (Fs.File.System error)) -> Some error
  | TimeSystemTime (Time.SystemTime.System error) -> Some error
  | TimeMonotonic (Time.Monotonic.System error) -> Some error
  | _ -> None

let detail_to_string = fun value ->
  match value with
  | Async error -> Async.error_to_string error
  | Env error -> Env.error_to_string error
  | FsFile error -> Fs.File.error_to_string error
  | FsReadDir error -> Fs.ReadDir.error_to_string error
  | FsEvents error -> Fs.Events.error_to_string error
  | NetAddr error -> Net.Addr.error_to_string error
  | NetIpAddr error -> Net.IpAddr.error_to_string error
  | NetSocketAddr error -> Net.SocketAddr.error_to_string error
  | NetTcpListener error -> Net.TcpListener.error_to_string error
  | NetTcpStream error -> Net.TcpStream.error_to_string error
  | NetUnixStream error -> Net.UnixStream.error_to_string error
  | NetUdpSocket error -> Net.UdpSocket.error_to_string error
  | Process error -> Process.error_to_string error
  | TimeSystemTime error -> Time.SystemTime.error_to_string error
  | TimeMonotonic error -> Time.Monotonic.error_to_string error
  | TimeTimer error -> Time.Timer.error_to_string error

let to_string = fun error -> String.concat "" [ module_name error; ": "; detail_to_string error ]
