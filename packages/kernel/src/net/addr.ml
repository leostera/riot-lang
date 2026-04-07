open Global0
open Collections
open Async

type 't raw_addr = string

type tcp_addr =
  ([
    `v4
    | `v6
  ]) raw_addr

type stream_addr =
[
  `Tcp of tcp_addr * int
]

type datagram_addr =
[
  `Udp of tcp_addr * int
]

type socket_addr =
[
  stream_addr
  | datagram_addr
]

module Ipaddr = struct
  let to_unix: tcp_addr -> Unix.inet_addr = Unix.inet_addr_of_string

  let of_unix: Unix.inet_addr -> tcp_addr = Unix.string_of_inet_addr
end

let loopback: tcp_addr = "0.0.0.0"

let tcp = fun host port ->
  assert (String.length host > 0);
  `Tcp (host, port)

let udp = fun host port ->
  assert (String.length host > 0);
  `Udp (host, port)

let to_unix = fun addr ->
  match addr with
  | `Tcp (host, port) -> (Unix.SOCK_STREAM, Unix.ADDR_INET (Ipaddr.to_unix host, port))
  | `Udp (host, port) -> (Unix.SOCK_DGRAM, Unix.ADDR_INET (Ipaddr.to_unix host, port))

let to_domain = fun addr ->
  match addr with
  | `Tcp (_host, _) -> Unix.PF_INET
  | `Udp (_host, _) -> Unix.PF_INET

let of_unix = fun sockaddr ->
  match sockaddr with
  | Unix.ADDR_INET (host, port) -> tcp (Ipaddr.of_unix host) port
  | Unix.ADDR_UNIX addr -> panic
    (Format.format Format.[ str "unsupported unix addresses: "; str addr ])

let of_unix_datagram = fun sockaddr ->
  match sockaddr with
  | Unix.ADDR_INET (host, port) -> udp (Ipaddr.of_unix host) port
  | Unix.ADDR_UNIX addr -> panic
    (Format.format Format.[ str "unsupported unix addresses: "; str addr ])

let to_string = fun t -> t

let resolve_host_addresses = fun host service ->
  IO.unix_syscall
    (fun () ->
      let info = Unix.getaddrinfo host service [] in
      List.filter_map
        (fun Unix.{ ai_addr; _ } ->
          match ai_addr with
          | Unix.ADDR_INET (addr, _port) -> Some (Unix.string_of_inet_addr addr)
          | Unix.ADDR_UNIX _ -> None)
        info)

let of_addr_info = fun
  (Unix.{
    ai_family;
    ai_addr;
    ai_socktype;
    ai_protocol;
    _
  }) ->
  match (ai_family, ai_socktype, ai_addr) with
  | ((Unix.PF_INET | Unix.PF_INET6), (Unix.SOCK_DGRAM | Unix.SOCK_STREAM), Unix.ADDR_INET (addr, port)) ->
      if ai_socktype = Unix.SOCK_STREAM || ai_protocol = 6 then
        Some (tcp (Unix.string_of_inet_addr addr) port)
      else
        None
  | _ -> None

let of_addr_info_datagram = fun
  (Unix.{
    ai_family;
    ai_addr;
    ai_socktype;
    ai_protocol;
    _
  }) ->
  match (ai_family, ai_socktype, ai_addr) with
  | ((Unix.PF_INET | Unix.PF_INET6), (Unix.SOCK_DGRAM | Unix.SOCK_STREAM), Unix.ADDR_INET (addr, port)) ->
      if ai_socktype = Unix.SOCK_DGRAM || ai_protocol = 17 then
        Some (udp (Unix.string_of_inet_addr addr) port)
      else
        None
  | _ -> None

let is_ip_address = fun host ->
  (* Simple check: if it only contains digits, dots, and colons, it's likely an IP *)
  let is_ip_char c = (c >= '0' && c <= '9') || c = '.' || c = ':' in
  String.length host > 0 && String.fold_left (fun acc c -> acc && is_ip_char c) true host

let resolve_host = fun ~host ~port ~wrap ->
  if is_ip_address host then
    try
      let _ = Unix.inet_addr_of_string host in
      Ok (wrap host port)
    with
    | _ -> (
        match resolve_host_addresses host (Int.to_string port) with
        | Ok (ip :: _) -> Ok (wrap ip port)
        | Ok [] -> Error (IO.Unknown_error "No address info found")
        | Error err -> Error err
      )
  else
    match resolve_host_addresses host (Int.to_string port) with
    | Ok (ip :: _) -> Ok (wrap ip port)
    | Ok [] -> Error (IO.Unknown_error "No address info found")
    | Error err -> Error err

let of_host_and_port = fun ~host ~port ->
  resolve_host ~host ~port ~wrap:tcp

let of_host_and_port_datagram = fun ~host ~port ->
  resolve_host ~host ~port ~wrap:udp

let get_info = fun (`Tcp (host, port)) ->
  match resolve_host_addresses host (Int.to_string port) with
  | Ok hosts -> Ok (List.map (fun resolved_host -> tcp resolved_host port) hosts)
  | Error err -> Error err

let get_info_datagram = fun (`Udp (host, port)) ->
  match resolve_host_addresses host (Int.to_string port) with
  | Ok hosts -> Ok (List.map (fun resolved_host -> udp resolved_host port) hosts)
  | Error err -> Error err

let ip = fun addr ->
  match addr with
  | `Tcp (ip, _)
  | `Udp (ip, _) -> ip

let port = fun addr ->
  match addr with
  | `Tcp (_, port)
  | `Udp (_, port) -> port
