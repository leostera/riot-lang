open Prelude

type t = int

type error =
  System of System_error.t

module FFI = struct
  external bind: string -> int -> bool -> bool -> int -> (t, int) Result.t = "kernel_new_net_tcp_listener_bind"

  external accept: t -> ((Tcp_stream.t * (string * int)), int) Result.t = "kernel_new_net_tcp_listener_accept"

  external close: t -> (unit, int) Result.t = "kernel_new_net_socket_close"

  external local_addr: t -> ((string * int), int) Result.t = "kernel_new_net_socket_local_addr"
end

let socket_addr_of_pair = fun (ip, port) ->
  match Ip_addr.of_string ip with
  | Result.Ok ip -> Socket_addr.of_parts_unchecked ~ip ~port
  | Result.Error _ -> System_error.panic "kernel returned an invalid ip address"

let error_to_string = function
  | System error -> System_error.to_string error

let bind = fun ?(reuse_addr = true) ?(reuse_port = false) ?(backlog = 128) addr ->
  let ip = Ip_addr.to_string (Socket_addr.ip addr) in
  let port = Socket_addr.port addr in
  Result.map_error
    (fun code -> System (System_error.of_code code))
    (FFI.bind ip port reuse_addr reuse_port backlog)

let accept = fun listener ->
  Result.map_error
    (fun code -> System (System_error.of_code code))
    (Result.map (fun (stream, addr) -> (stream, socket_addr_of_pair addr)) (FFI.accept listener))

let close = fun listener ->
  Result.map_error (fun code -> System (System_error.of_code code)) (FFI.close listener)

let local_addr = fun listener ->
  Result.map_error
    (fun code -> System (System_error.of_code code))
    (Result.map socket_addr_of_pair (FFI.local_addr listener))

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
