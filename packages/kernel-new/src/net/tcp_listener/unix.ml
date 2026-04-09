open Prelude

let ( let* ) = Result.and_then

type t = int

type error =
  | InvalidBacklog of { backlog: int }
  | InvalidSocketAddr of { ip: string; port: int }
  | WouldBlock
  | AddressInUse
  | AddressNotAvailable
  | ConnectionAborted
  | System of System_error.t

module FFI = struct
  external bind: string -> int -> bool -> bool -> int -> (t, int) Result.t = "kernel_new_net_tcp_listener_bind"

  external accept: t -> ((Tcp_stream.t * (string * int)), int) Result.t = "kernel_new_net_tcp_listener_accept"

  external close: t -> (unit, int) Result.t = "kernel_new_net_socket_close"

  external local_addr: t -> ((string * int), int) Result.t = "kernel_new_net_socket_local_addr"
end

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
  | InvalidBacklog { backlog } -> String.concat
    ""
    [ "invalid listener backlog: "; Int.to_string backlog ]
  | InvalidSocketAddr { ip; port } -> String.concat
    ""
    [ "invalid socket address returned by backend: "; ip; ":"; Int.to_string port ]
  | WouldBlock -> "operation would block"
  | AddressInUse -> "address already in use"
  | AddressNotAvailable -> "address not available"
  | ConnectionAborted -> "connection aborted"
  | System error -> System_error.to_string error

let error_of_system = fun value ->
  match value with
  | System_error.WouldBlock -> WouldBlock
  | System_error.AddressInUse -> AddressInUse
  | System_error.AddressNotAvailable -> AddressNotAvailable
  | System_error.ConnectionAborted -> ConnectionAborted
  | error -> System error

let bind = fun ?(reuse_addr = true) ?(reuse_port = false) ?(backlog = 128) addr ->
  if backlog <= 0 then
    Result.Error (InvalidBacklog { backlog })
  else
    let ip = Ip_addr.to_string (Socket_addr.ip addr) in
    let port = Socket_addr.port addr in
    Result.map_error
      (fun code -> error_of_system (System_error.of_code code))
      (FFI.bind ip port reuse_addr reuse_port backlog)

let accept = fun listener ->
  let* (stream, addr) =
    Result.map_error (fun code -> error_of_system (System_error.of_code code)) (FFI.accept listener)
  in
  let* addr = socket_addr_of_pair addr in
  Result.Ok (stream, addr)

let close = fun listener ->
  Result.map_error (fun code -> error_of_system (System_error.of_code code)) (FFI.close listener)

let local_addr = fun listener ->
  let* addr =
    Result.map_error
      (fun code -> error_of_system (System_error.of_code code))
      (FFI.local_addr listener)
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
