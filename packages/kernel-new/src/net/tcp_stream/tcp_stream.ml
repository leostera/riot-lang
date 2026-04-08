open Prelude

type t = int

type error = Error.t

type connect_result =
  | Connected of t
  | In_progress of t

let connect_result_connected = 0

let connect_result_in_progress = 1

module FFI = struct
  external connect: string -> int -> ((t * int), int) Result.t = "kernel_new_net_tcp_stream_connect"

  external close: t -> (unit, int) Result.t = "kernel_new_net_socket_close"

  external read: t -> bytes -> int -> int -> (int, int) Result.t = "kernel_new_net_tcp_stream_read"

  external write: t -> bytes -> int -> int -> (int, int) Result.t = "kernel_new_net_tcp_stream_write"

  external readv: t -> IO.Iovec.t -> (int, int) Result.t = "kernel_new_net_tcp_stream_readv"

  external writev: t -> IO.Iovec.t -> (int, int) Result.t = "kernel_new_net_tcp_stream_writev"

  external local_addr: t -> ((string * int), int) Result.t = "kernel_new_net_socket_local_addr"

  external peer_addr: t -> ((string * int), int) Result.t = "kernel_new_net_tcp_stream_peer_addr"
end

let validate_slice = fun buf ~pos ~len ->
  if pos < 0 || len < 0 || pos + len > Bytes.length buf then
    Error.panic "invalid buffer bounds"

let socket_addr_of_pair = fun (ip, port) ->
  match Ip_addr.of_string ip with
  | Result.Ok ip -> Socket_addr.of_parts_unchecked ~ip ~port
  | Result.Error _ -> Error.panic "kernel returned an invalid ip address"

let connect = fun addr ->
  let ip = Ip_addr.to_string (Socket_addr.ip addr) in
  let port = Socket_addr.port addr in
  Result.map_error Error.of_code
    (
      Result.map
        (fun (fd, state) ->
          if state = connect_result_connected then
            Connected fd
          else
            In_progress fd)
        (FFI.connect ip port)
    )

let close = fun fd ->
  Result.map_error Error.of_code (FFI.close fd)

let read = fun fd ?(pos = 0) ?len buf ->
  let len = Option.unwrap_or len ~default:((Bytes.length buf - pos)) in
  validate_slice buf ~pos ~len;
  Result.map_error Error.of_code (FFI.read fd buf pos len)

let write = fun fd ?(pos = 0) ?len buf ->
  let len = Option.unwrap_or len ~default:((Bytes.length buf - pos)) in
  validate_slice buf ~pos ~len;
  Result.map_error Error.of_code (FFI.write fd buf pos len)

let read_vectored = fun fd iov ->
  Result.map_error Error.of_code (FFI.readv fd iov)

let write_vectored = fun fd iov ->
  Result.map_error Error.of_code (FFI.writev fd iov)

let local_addr = fun fd ->
  Result.map_error Error.of_code (Result.map socket_addr_of_pair (FFI.local_addr fd))

let peer_addr = fun fd ->
  Result.map_error Error.of_code (Result.map socket_addr_of_pair (FFI.peer_addr fd))

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
