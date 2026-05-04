(** Network address handling *)
open Global

type 't raw_addr = Kernel.Net.SocketAddr.t

type tcp_addr = Kernel.Net.IpAddr.t

type stream_addr = Kernel.Net.SocketAddr.t

type datagram_addr = Kernel.Net.SocketAddr.t

type socket_addr = Kernel.Net.SocketAddr.t

type error =
  | System_error of IO.error
  | Invalid_port_number of string
  | Invalid_format of string

let loopback = Kernel.Net.IpAddr.v4_loopback

let tcp = fun ip port ->
  match Kernel.Net.SocketAddr.make ~ip ~port with
  | Ok addr -> addr
  | Error err -> panic ("Std.Net.Addr.tcp: " ^ Kernel.Net.SocketAddr.error_to_string err)

let udp = fun ip port ->
  match Kernel.Net.SocketAddr.make ~ip ~port with
  | Ok addr -> addr
  | Error err -> panic ("Std.Net.Addr.udp: " ^ Kernel.Net.SocketAddr.error_to_string err)

let io_error_of_resolver_error = fun __tmp1 ->
  match __tmp1 with
  | Kernel.Net.Addr.System err -> IO.from_system_error err
  | error -> IO.Unknown_error (Kernel.Net.Addr.error_to_string error)

let from_host_and_port = fun ~host ~port ->
  match Kernel.Net.Addr.resolve_first_stream ~host ~port with
  | Ok addr -> Ok addr
  | Error (Kernel.Net.Addr.InvalidPort { port }) -> Error (Invalid_port_number (Int.to_string port))
  | Error err -> Error (System_error (io_error_of_resolver_error err))

let from_host_and_port_datagram = fun ~host ~port ->
  match Kernel.Net.Addr.resolve_first_datagram ~host ~port with
  | Ok addr -> Ok addr
  | Error (Kernel.Net.Addr.InvalidPort { port }) -> Error (Invalid_port_number (Int.to_string port))
  | Error err -> Error (System_error (io_error_of_resolver_error err))

let split_host_port = fun value ->
  if String.length value = 0 then
    None
  else if String.get_unchecked value ~at:0 = '[' then
    match String.index_of value ~char:']' with
    | None -> None
    | Some close_index ->
        if
          close_index + 1 >= String.length value
          || String.get_unchecked value ~at:(close_index + 1) != ':'
        then
          None
        else
          Some (
            String.sub value ~offset:1 ~len:(close_index - 1),
            String.sub value ~offset:(close_index + 2) ~len:(String.length value - close_index - 2)
          )
  else
    match String.last_index value ':' with
    | None -> None
    | Some index ->
        Some (
          String.sub value ~offset:0 ~len:index,
          String.sub value ~offset:(index + 1) ~len:(String.length value - index - 1)
        )

let parse_port = fun value ->
  match Int.parse value with
  | Some port -> Ok port
  | None -> Error (Invalid_port_number value)

let parse = fun s ->
  match split_host_port s with
  | None -> Error (Invalid_format "missing port")
  | Some (host, port_text) -> (
      match parse_port port_text with
      | Error err -> Error err
      | Ok port -> from_host_and_port ~host ~port
    )

let parse_datagram = fun s ->
  match split_host_port s with
  | None -> Error (Invalid_format "missing port")
  | Some (host, port_text) -> (
      match parse_port port_text with
      | Error err -> Error err
      | Ok port -> from_host_and_port_datagram ~host ~port
    )

let ip = fun addr -> Kernel.Net.IpAddr.to_string (Kernel.Net.SocketAddr.ip addr)

let port = fun addr -> Kernel.Net.SocketAddr.port addr
