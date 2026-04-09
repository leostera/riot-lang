open Prelude

let ( let* ) = Result.and_then

type t = int

type error =
  | Invalid_slice of {
      pos: int;
      len: int;
      buffer_len: int;
    }
  | System of System_error.t

module FFI = struct
  external bind: string -> int -> bool -> bool -> (t, int) Result.t = "kernel_new_net_udp_socket_bind"

  external connect: t -> string -> int -> (unit, int) Result.t = "kernel_new_net_udp_socket_connect"

  external close: t -> (unit, int) Result.t = "kernel_new_net_socket_close"

  external local_addr: t -> ((string * int), int) Result.t = "kernel_new_net_socket_local_addr"

  external recv: t -> bytes -> int -> int -> (int, int) Result.t = "kernel_new_net_udp_socket_recv"

  external recv_from: t -> bytes -> int -> int -> ((int * (string * int)), int) Result.t = "kernel_new_net_udp_socket_recv_from"

  external send: t -> bytes -> int -> int -> (int, int) Result.t = "kernel_new_net_udp_socket_send"

  external send_to: t -> string -> int -> bytes -> (int * int) -> (int, int) Result.t = "kernel_new_net_udp_socket_send_to"
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
  | Invalid_slice { pos; len; buffer_len } ->
      String.concat ""
        [
          "invalid buffer slice: pos=";
          Int.to_string pos;
          ", len=";
          Int.to_string len;
          ", buffer_len=";
          Int.to_string buffer_len;
        ]
  | System error ->
      System_error.to_string error

let bind = fun ?(reuse_addr = true) ?(reuse_port = false) addr ->
  let ip = Ip_addr.to_string (Socket_addr.ip addr) in
  let port = Socket_addr.port addr in
  Result.map_error (fun code -> System (System_error.of_code code)) (FFI.bind ip port reuse_addr reuse_port)

let connect = fun socket addr ->
  let ip = Ip_addr.to_string (Socket_addr.ip addr) in
  let port = Socket_addr.port addr in
  Result.map_error (fun code -> System (System_error.of_code code)) (FFI.connect socket ip port)

let close = fun socket ->
  Result.map_error (fun code -> System (System_error.of_code code)) (FFI.close socket)

let local_addr = fun socket ->
  Result.map_error (fun code -> System (System_error.of_code code)) (Result.map socket_addr_of_pair (FFI.local_addr socket))

let recv = fun socket ?(pos = 0) ?len buf ->
  let len = Option.unwrap_or len ~default:((Bytes.length buf - pos)) in
  let* () = validate_slice buf ~pos ~len in
  Result.map_error (fun code -> System (System_error.of_code code)) (FFI.recv socket buf pos len)

let recv_from = fun socket ?(pos = 0) ?len buf ->
  let len = Option.unwrap_or len ~default:((Bytes.length buf - pos)) in
  let* () = validate_slice buf ~pos ~len in
  Result.map_error
    (fun code -> System (System_error.of_code code))
    (Result.map
      (fun (read_count, addr) -> (read_count, socket_addr_of_pair addr))
      (FFI.recv_from socket buf pos len))

let send = fun socket ?(pos = 0) ?len buf ->
  let len = Option.unwrap_or len ~default:((Bytes.length buf - pos)) in
  let* () = validate_slice buf ~pos ~len in
  Result.map_error (fun code -> System (System_error.of_code code)) (FFI.send socket buf pos len)

let send_to = fun socket addr ?(pos = 0) ?len buf ->
  let len = Option.unwrap_or len ~default:((Bytes.length buf - pos)) in
  let* () = validate_slice buf ~pos ~len in
  let ip = Ip_addr.to_string (Socket_addr.ip addr) in
  let port = Socket_addr.port addr in
  Result.map_error (fun code -> System (System_error.of_code code)) (FFI.send_to socket ip port buf (pos, len))

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
