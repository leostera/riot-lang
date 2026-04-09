open Prelude

let ( let* ) = Result.and_then

type t = int

type shutdown =
  | Read
  | Write
  | ReadWrite

type error =
  | InvalidSlice of { pos: int; len: int; buffer_len: int }
  | InvalidSocketAddr of { ip: string; port: int }
  | InvalidConnectState of { state: int }
  | WouldBlock
  | ConnectionRefused
  | ConnectionReset
  | TimedOut
  | BrokenPipe
  | NotConnected
  | ConnectionAborted
  | NetworkUnreachable
  | System of System_error.t

type connect_result =
  | Connected of t
  | InProgress of t

let connect_result_connected = 0

let connect_result_in_progress = 1

let shutdown_read = 0

let shutdown_write = 1

let shutdown_read_write = 2

module FFI = struct
  external connect: string -> int -> ((t * int), int) Result.t = "kernel_new_net_tcp_stream_connect"

  external close: t -> (unit, int) Result.t = "kernel_new_net_socket_close"

  external finish_connect: t -> (unit, int) Result.t = "kernel_new_net_tcp_stream_finish_connect"

  external shutdown: t -> int -> (unit, int) Result.t = "kernel_new_net_tcp_stream_shutdown"

  external read: t -> bytes -> int -> int -> (int, int) Result.t = "kernel_new_net_tcp_stream_read"

  external write: t -> bytes -> int -> int -> (int, int) Result.t = "kernel_new_net_tcp_stream_write"

  external readv: t -> Io.Iovec.t -> (int, int) Result.t = "kernel_new_net_tcp_stream_readv"

  external writev: t -> Io.Iovec.t -> (int, int) Result.t = "kernel_new_net_tcp_stream_writev"

  external local_addr: t -> ((string * int), int) Result.t = "kernel_new_net_socket_local_addr"

  external peer_addr: t -> ((string * int), int) Result.t = "kernel_new_net_tcp_stream_peer_addr"
end

let validate_slice = fun buf ~pos ~len ->
  if pos < 0 || len < 0 || pos + len > Bytes.length buf then
    Result.Error (InvalidSlice { pos; len; buffer_len = Bytes.length buf })
  else
    Result.Ok ()

let socket_addr_of_pair = fun (ip, port) ->
  match Ip_addr.of_string ip with
  | Result.Error _ -> Result.Error (InvalidSocketAddr { ip; port })
  | Result.Ok ip -> (
      match Socket_addr.of_parts ~ip ~port with
      | Result.Ok addr -> Result.Ok addr
      | Result.Error _ -> Result.Error (InvalidSocketAddr { ip = Ip_addr.to_string ip; port })
    )

let error_to_string = fun value ->
  match value with
  | InvalidSlice { pos; len; buffer_len } -> String.concat
    ""
    [
      "invalid buffer slice: pos=";
      Int.to_string pos;
      ", len=";
      Int.to_string len;
      ", buffer_len=";
      Int.to_string buffer_len;
    ]
  | InvalidSocketAddr { ip; port } -> String.concat
    ""
    [ "invalid socket address returned by backend: "; ip; ":"; Int.to_string port ]
  | InvalidConnectState { state } -> String.concat
    ""
    [ "invalid tcp connect state returned by backend: "; Int.to_string state ]
  | WouldBlock -> "operation would block"
  | ConnectionRefused -> "connection refused"
  | ConnectionReset -> "connection reset by peer"
  | TimedOut -> "timed out"
  | BrokenPipe -> "broken pipe"
  | NotConnected -> "socket is not connected"
  | ConnectionAborted -> "connection aborted"
  | NetworkUnreachable -> "network unreachable"
  | System error -> System_error.to_string error

let error_of_system = fun value ->
  match value with
  | System_error.WouldBlock -> WouldBlock
  | System_error.ConnectionRefused -> ConnectionRefused
  | System_error.ConnectionReset -> ConnectionReset
  | System_error.TimedOut -> TimedOut
  | System_error.BrokenPipe -> BrokenPipe
  | System_error.NotConnected -> NotConnected
  | System_error.ConnectionAborted -> ConnectionAborted
  | System_error.NetworkUnreachable -> NetworkUnreachable
  | error -> System error

let shutdown_code = fun value ->
  match value with
  | Read -> shutdown_read
  | Write -> shutdown_write
  | ReadWrite -> shutdown_read_write

let connect = fun addr ->
  let ip = Ip_addr.to_string (Socket_addr.ip addr) in
  let port = Socket_addr.port addr in
  let* (fd, state) =
    Result.map_error (fun code -> error_of_system (System_error.of_code code)) (FFI.connect ip port)
  in
  if state = connect_result_connected then
    Result.Ok (Connected fd)
  else if state = connect_result_in_progress then
    Result.Ok (InProgress fd)
  else
    Result.Error (InvalidConnectState { state })

let close = fun fd ->
  Result.map_error (fun code -> error_of_system (System_error.of_code code)) (FFI.close fd)

let finish_connect = fun fd ->
  Result.map_error (fun code -> error_of_system (System_error.of_code code)) (FFI.finish_connect fd)

let shutdown = fun fd how ->
  Result.map_error
    (fun code -> error_of_system (System_error.of_code code))
    (FFI.shutdown fd (shutdown_code how))

let read = fun fd ?(pos = 0) ?len buf ->
  let len = Option.unwrap_or len ~default:(Bytes.length buf - pos) in
  let* () = validate_slice buf ~pos ~len in
  Result.map_error
    (fun code -> error_of_system (System_error.of_code code))
    (FFI.read fd buf pos len)

let write = fun fd ?(pos = 0) ?len buf ->
  let len = Option.unwrap_or len ~default:(Bytes.length buf - pos) in
  let* () = validate_slice buf ~pos ~len in
  Result.map_error
    (fun code -> error_of_system (System_error.of_code code))
    (FFI.write fd buf pos len)

let read_vectored = fun fd iov ->
  Result.map_error (fun code -> error_of_system (System_error.of_code code)) (FFI.readv fd iov)

let write_vectored = fun fd iov ->
  Result.map_error (fun code -> error_of_system (System_error.of_code code)) (FFI.writev fd iov)

let local_addr = fun fd ->
  let* addr =
    Result.map_error (fun code -> error_of_system (System_error.of_code code)) (FFI.local_addr fd)
  in
  socket_addr_of_pair addr

let peer_addr = fun fd ->
  let* addr =
    Result.map_error (fun code -> error_of_system (System_error.of_code code)) (FFI.peer_addr fd)
  in
  socket_addr_of_pair addr

let to_source = fun fd ->
  let module Source = struct
    type nonrec t = t

    let register = fun fd selector token interest ->
      Async.Adapter.Selector.register selector ~fd ~token ~interest

    let reregister = fun fd selector token interest ->
      Async.Adapter.Selector.reregister selector ~fd ~token ~interest

    let deregister = fun fd selector -> Async.Adapter.Selector.deregister selector ~fd
  end in
  Async.Source.make (module Source) fd
