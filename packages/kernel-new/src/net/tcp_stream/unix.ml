open Prelude

let ( let* ) = Result.and_then

type t = int

type shutdown =
  | Read
  | Write
  | Read_write

type error =
  | Invalid_slice of { pos: int; len: int; buffer_len: int }
  | Would_block
  | Connection_refused
  | Connection_reset
  | Timed_out
  | Broken_pipe
  | Not_connected
  | Connection_aborted
  | Network_unreachable
  | System of System_error.t

type connect_result =
  | Connected of t
  | In_progress of t

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

  external readv: t -> IO.Iovec.t -> (int, int) Result.t = "kernel_new_net_tcp_stream_readv"

  external writev: t -> IO.Iovec.t -> (int, int) Result.t = "kernel_new_net_tcp_stream_writev"

  external local_addr: t -> ((string * int), int) Result.t = "kernel_new_net_socket_local_addr"

  external peer_addr: t -> ((string * int), int) Result.t = "kernel_new_net_tcp_stream_peer_addr"
end

let validate_slice = fun buf ~pos ~len ->
  if pos < 0 || len < 0 || pos + len > Bytes.length buf then
    Result.Error (Invalid_slice { pos; len; buffer_len = Bytes.length buf })
  else
    Result.Ok ()

let socket_addr_of_pair = fun (ip, port) ->
  match Ip_addr.of_string ip with
  | Result.Ok ip -> Socket_addr.of_parts_unchecked ~ip ~port
  | Result.Error _ -> System_error.panic "kernel returned an invalid ip address"

let error_to_string = function
  | Invalid_slice { pos; len; buffer_len } -> String.concat
    ""
    [
      "invalid buffer slice: pos=";
      Int.to_string pos;
      ", len=";
      Int.to_string len;
      ", buffer_len=";
      Int.to_string buffer_len;
    ]
  | Would_block -> "operation would block"
  | Connection_refused -> "connection refused"
  | Connection_reset -> "connection reset by peer"
  | Timed_out -> "timed out"
  | Broken_pipe -> "broken pipe"
  | Not_connected -> "socket is not connected"
  | Connection_aborted -> "connection aborted"
  | Network_unreachable -> "network unreachable"
  | System error -> System_error.to_string error

let error_of_system = function
  | System_error.Would_block -> Would_block
  | System_error.Connection_refused -> Connection_refused
  | System_error.Connection_reset -> Connection_reset
  | System_error.Timed_out -> Timed_out
  | System_error.Broken_pipe -> Broken_pipe
  | System_error.Not_connected -> Not_connected
  | System_error.Connection_aborted -> Connection_aborted
  | System_error.Network_unreachable -> Network_unreachable
  | error -> System error

let shutdown_code = function
  | Read -> shutdown_read
  | Write -> shutdown_write
  | Read_write -> shutdown_read_write

let connect = fun addr ->
  let ip = Ip_addr.to_string (Socket_addr.ip addr) in
  let port = Socket_addr.port addr in
  Result.map_error (fun code -> error_of_system (System_error.of_code code))
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
  Result.map_error (fun code -> error_of_system (System_error.of_code code)) (FFI.close fd)

let finish_connect = fun fd ->
  Result.map_error
    (fun code -> error_of_system (System_error.of_code code))
    (FFI.finish_connect fd)

let shutdown = fun fd how ->
  Result.map_error
    (fun code -> error_of_system (System_error.of_code code))
    (FFI.shutdown fd (shutdown_code how))

let read = fun fd ?(pos = 0) ?len buf ->
  let len = Option.unwrap_or len ~default:((Bytes.length buf - pos)) in
  let* () = validate_slice buf ~pos ~len in
  Result.map_error
    (fun code -> error_of_system (System_error.of_code code))
    (FFI.read fd buf pos len)

let write = fun fd ?(pos = 0) ?len buf ->
  let len = Option.unwrap_or len ~default:((Bytes.length buf - pos)) in
  let* () = validate_slice buf ~pos ~len in
  Result.map_error
    (fun code -> error_of_system (System_error.of_code code))
    (FFI.write fd buf pos len)

let read_vectored = fun fd iov ->
  Result.map_error (fun code -> error_of_system (System_error.of_code code)) (FFI.readv fd iov)

let write_vectored = fun fd iov ->
  Result.map_error (fun code -> error_of_system (System_error.of_code code)) (FFI.writev fd iov)

let local_addr = fun fd ->
  Result.map_error
    (fun code -> error_of_system (System_error.of_code code))
    (Result.map socket_addr_of_pair (FFI.local_addr fd))

let peer_addr = fun fd ->
  Result.map_error
    (fun code -> error_of_system (System_error.of_code code))
    (Result.map socket_addr_of_pair (FFI.peer_addr fd))

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
