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

module Ipaddr = struct
  let to_unix : tcp_addr -> Unix.inet_addr = Unix.inet_addr_of_string

  let of_unix : Unix.inet_addr -> tcp_addr = Unix.string_of_inet_addr
end

let loopback : tcp_addr = "0.0.0.0"

let tcp = fun host port ->
  assert (String.length host > 0);
  `Tcp (host, port)

let to_unix = fun addr ->
  match addr with
  | `Tcp (host, port) -> (Unix.SOCK_STREAM, Unix.ADDR_INET (Ipaddr.to_unix host, port))

let to_domain = fun addr ->
  match addr with
  | `Tcp (_host, _) -> Unix.PF_INET

let of_unix = fun sockaddr ->
  match sockaddr with
  | Unix.ADDR_INET (host, port) -> tcp (Ipaddr.of_unix host) port
  | Unix.ADDR_UNIX addr -> panic ("unsupported unix addresses: " ^ addr)

let to_string = fun t -> t

let of_addr_info = fun
  (Unix.{
    ai_family;
    ai_addr;
    ai_socktype;
    ai_protocol;
    _
  }) ->
  match (ai_family, ai_socktype, ai_addr) with
  | ((Unix.PF_INET | Unix.PF_INET6), (Unix.SOCK_DGRAM | Unix.SOCK_STREAM), Unix.ADDR_INET (addr, port)) -> (
      match ai_protocol with
      | 6 -> Some (tcp (Unix.string_of_inet_addr addr) port)
      | _ -> None
    )
  | _ -> None

let get_info = fun host service ->
  IO.unix_syscall
    (fun () ->
      let info = Unix.getaddrinfo host service [] in
      List.filter_map of_addr_info info)

let is_ip_address = fun host ->
  (* Simple check: if it only contains digits, dots, and colons, it's likely an IP *)
  let is_ip_char = fun c -> (c >= '0' && c <= '9') || c = '.' || c = ':' in
  String.length host > 0 && String.fold_left (fun acc c -> acc && is_ip_char c) true host

let of_host_and_port = fun ~host ~port ->
  (* Fast path: if host looks like an IP address, try to use it directly *)
  if is_ip_address host then
    try
      let _ = Unix.inet_addr_of_string host in
      Ok (tcp host port)
    with
    | _ -> (
        (* Invalid IP format, fall back to DNS *)
        match get_info host (Int.to_string port) with
        | Ok (ip :: _) -> Ok ip
        | Ok [] -> Error (IO.Unknown_error "No address info found")
        | Error err -> Error err
      )
  else
    (* Hostname, need DNS resolution *)
    match get_info host (Int.to_string port) with
    | Ok (ip :: _) -> Ok ip
    | Ok [] -> Error (IO.Unknown_error "No address info found")
    | Error err -> Error err

let get_info = fun (`Tcp (host, port)) -> get_info host (Int.to_string port)

let ip = fun (`Tcp (ip, _)) -> ip

let port = fun (`Tcp (_, port)) -> port
