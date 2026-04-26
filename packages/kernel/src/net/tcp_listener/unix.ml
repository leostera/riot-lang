open Prelude

let ( let* ) value fn = Result.and_then value ~fn

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
  external bind: string -> int -> bool -> bool -> int -> (t, int) Result.t =
    "kernel_new_net_tcp_listener_bind"

  external accept: t -> ((Tcp_stream.t * (string * int)), int) Result.t =
    "kernel_new_net_tcp_listener_accept"

  external close: t -> (unit, int) Result.t = "kernel_new_net_socket_close"

  external local_addr: t -> ((string * int), int) Result.t = "kernel_new_net_socket_local_addr"
end

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
  | InvalidBacklog { backlog } ->
      String.concat "" [ "invalid listener backlog: "; Int.to_string backlog ]
  | InvalidSocketAddr { ip; port } ->
      String.concat
        ""
        [ "invalid socket address returned by backend: "; ip; ":"; Int.to_string port; ]
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
    FFI.bind ip port reuse_addr reuse_port backlog
    |> Result.map_err ~fn:(fun code -> error_of_system (System_error.from_code code))

let accept = fun listener ->
  let* (stream, addr) =
    FFI.accept listener
    |> Result.map_err ~fn:(fun code -> error_of_system (System_error.from_code code))
  in
  let* addr = socket_addr_of_pair addr in Result.Ok (stream, addr)

let close = fun listener ->
  FFI.close listener
  |> Result.map_err ~fn:(fun code -> error_of_system (System_error.from_code code))

let local_addr = fun listener ->
  let* addr =
    FFI.local_addr listener
    |> Result.map_err ~fn:(fun code -> error_of_system (System_error.from_code code))
  in
  socket_addr_of_pair addr

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
