open Prelude

let ( let* ) value fn = Result.and_then value ~fn

type t = int

type error =
  | InvalidSlice of { pos: int; len: int; buffer_len: int }
  | InvalidSocketAddr of { ip: string; port: int }
  | WouldBlock
  | TimedOut
  | ConnectionRefused
  | ConnectionReset
  | NetworkUnreachable
  | NotConnected
  | MessageTooLong
  | DestinationAddressRequired
  | AddressInUse
  | AddressNotAvailable
  | System of System_error.t

module FFI = struct
  external bind: string -> int -> bool -> bool -> (t, int) Result.t =
    "kernel_new_net_udp_socket_bind"

  external connect: t -> string -> int -> (unit, int) Result.t = "kernel_new_net_udp_socket_connect"

  external close: t -> (unit, int) Result.t = "kernel_new_net_socket_close"

  external local_addr: t -> (string * int, int) Result.t = "kernel_new_net_socket_local_addr"

  external recv: t -> bytes -> int -> int -> (int, int) Result.t = "kernel_new_net_udp_socket_recv"

  external recv_from: t -> bytes -> int -> int -> ((int * (string * int)), int) Result.t =
    "kernel_new_net_udp_socket_recv_from"

  external send: t -> bytes -> int -> int -> (int, int) Result.t = "kernel_new_net_udp_socket_send"

  external send_to: t -> string -> int -> bytes -> (int * int) -> (int, int) Result.t =
    "kernel_new_net_udp_socket_send_to"
end

let validate_slice = fun buf ~pos ~len ->
  if pos < 0 || len < 0 || pos + len > Bytes.length buf then
    Result.Error (InvalidSlice { pos; len; buffer_len = Bytes.length buf })
  else
    Result.Ok ()

let socket_addr_of_pair = fun (ip, port) ->
  match Ip_addr.from_string ip with
  | Result.Error _ -> Result.Error (InvalidSocketAddr { ip; port })
  | Result.Ok ip -> (
      match Socket_addr.from_parts ~ip ~port with
      | Result.Ok addr -> Result.Ok addr
      | Result.Error _ -> Result.Error (InvalidSocketAddr { ip = Ip_addr.to_string ip; port })
    )

let error_to_string = fun value ->
  match value with
  | InvalidSlice { pos; len; buffer_len } ->
      String.concat
        ""
        [
          "invalid buffer slice: pos=";
          Int.to_string pos;
          ", len=";
          Int.to_string len;
          ", buffer_len=";
          Int.to_string buffer_len;
        ]
  | InvalidSocketAddr { ip; port } ->
      String.concat
        ""
        [ "invalid socket address returned by backend: "; ip; ":"; Int.to_string port; ]
  | WouldBlock -> "operation would block"
  | TimedOut -> "timed out"
  | ConnectionRefused -> "connection refused"
  | ConnectionReset -> "connection reset by peer"
  | NetworkUnreachable -> "network unreachable"
  | NotConnected -> "socket is not connected"
  | MessageTooLong -> "message too long"
  | DestinationAddressRequired -> "destination address required"
  | AddressInUse -> "address already in use"
  | AddressNotAvailable -> "address not available"
  | System error -> System_error.to_string error

let error_of_system = fun value ->
  match value with
  | System_error.WouldBlock -> WouldBlock
  | System_error.TimedOut -> TimedOut
  | System_error.ConnectionRefused -> ConnectionRefused
  | System_error.ConnectionReset -> ConnectionReset
  | System_error.NetworkUnreachable -> NetworkUnreachable
  | System_error.NotConnected -> NotConnected
  | System_error.MessageTooLong -> MessageTooLong
  | System_error.DestinationAddressRequired -> DestinationAddressRequired
  | System_error.AddressInUse -> AddressInUse
  | System_error.AddressNotAvailable -> AddressNotAvailable
  | error -> System error

let bind = fun ?(reuse_addr = true) ?(reuse_port = false) addr ->
  let ip = Ip_addr.to_string (Socket_addr.ip addr) in
  let port = Socket_addr.port addr in
  Result.map_err
    (FFI.bind ip port reuse_addr reuse_port)
    ~fn:(fun code -> error_of_system (System_error.from_code code))

let connect = fun socket addr ->
  let ip = Ip_addr.to_string (Socket_addr.ip addr) in
  let port = Socket_addr.port addr in
  Result.map_err
    (FFI.connect socket ip port)
    ~fn:(fun code -> error_of_system (System_error.from_code code))

let close = fun socket ->
  Result.map_err
    (FFI.close socket)
    ~fn:(fun code -> error_of_system (System_error.from_code code))

let local_addr = fun socket ->
  let* addr =
    Result.map_err
      (FFI.local_addr socket)
      ~fn:(fun code -> error_of_system (System_error.from_code code))
  in
  socket_addr_of_pair addr

let recv = fun socket ?(pos = 0) ?len buf ->
  let len = Option.unwrap_or len ~default:(Bytes.length buf - pos) in
  let* () = validate_slice buf ~pos ~len in
  Result.map_err
    (FFI.recv socket buf pos len)
    ~fn:(fun code -> error_of_system (System_error.from_code code))

let recv_from = fun socket ?(pos = 0) ?len buf ->
  let len = Option.unwrap_or len ~default:(Bytes.length buf - pos) in
  let* () = validate_slice buf ~pos ~len in
  let* (read_count, addr) =
    Result.map_err
      (FFI.recv_from socket buf pos len)
      ~fn:(fun code -> error_of_system (System_error.from_code code))
  in
  let* addr = socket_addr_of_pair addr in
  Result.Ok (read_count, addr)

let send = fun socket ?(pos = 0) ?len buf ->
  let len = Option.unwrap_or len ~default:(Bytes.length buf - pos) in
  let* () = validate_slice buf ~pos ~len in
  Result.map_err
    (FFI.send socket buf pos len)
    ~fn:(fun code -> error_of_system (System_error.from_code code))

let send_to = fun socket addr ?(pos = 0) ?len buf ->
  let len = Option.unwrap_or len ~default:(Bytes.length buf - pos) in
  let* () = validate_slice buf ~pos ~len in
  let ip = Ip_addr.to_string (Socket_addr.ip addr) in
  let port = Socket_addr.port addr in
  Result.map_err
    (FFI.send_to socket ip port buf (pos, len))
    ~fn:(fun code -> error_of_system (System_error.from_code code))

let to_source = fun fd ->
  let module Source = struct
    type nonrec t = t

    let register = fun fd selector token interest ->
      Async.Adapter.Selector.register
        selector
        ~fd
        ~token
        ~interest

    let reregister = fun fd selector token interest ->
      Async.Adapter.Selector.reregister
        selector
        ~fd
        ~token
        ~interest

    let deregister = fun fd selector -> Async.Adapter.Selector.deregister selector ~fd
  end in
  Async.Source.make (module Source) fd
