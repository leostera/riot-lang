open Async

type 't raw_addr = string
type tcp_addr = [ `v4 | `v6 ] raw_addr
type stream_addr = [ `Tcp of tcp_addr * int ]

module Ipaddr = struct
  let to_unix : tcp_addr -> Unix.inet_addr = Unix.inet_addr_of_string
  let of_unix : Unix.inet_addr -> tcp_addr = Unix.string_of_inet_addr
end

let loopback : tcp_addr = "0.0.0.0"

let tcp host port =
  assert (String.length host > 0);
  `Tcp (host, port)

let to_unix addr =
  match addr with
  | `Tcp (host, port) ->
      (Unix.SOCK_STREAM, Unix.ADDR_INET (Ipaddr.to_unix host, port))

let to_domain addr = match addr with `Tcp (_host, _) -> Unix.PF_INET

let of_unix sockaddr =
  match sockaddr with
  | Unix.ADDR_INET (host, port) -> tcp (Ipaddr.of_unix host) port
  | Unix.ADDR_UNIX addr -> failwith ("unsupported unix addresses: " ^ addr)

let pp ppf (addr : stream_addr) =
  match addr with
  | `Tcp (host, port) -> Format.fprintf ppf "%s:%d" host port

let to_string t = t

let of_addr_info Unix.{ ai_family; ai_addr; ai_socktype; ai_protocol; _ } =
  match (ai_family, ai_socktype, ai_addr) with
  | ( (Unix.PF_INET | Unix.PF_INET6),
      (Unix.SOCK_DGRAM | Unix.SOCK_STREAM),
      Unix.ADDR_INET (addr, port) ) -> (
      match ai_protocol with
      | 6 -> Some (tcp (Unix.string_of_inet_addr addr) port)
      | _ -> None)
  | _ -> None

let get_info host service =
  syscall @@ fun () ->
  let info = Unix.getaddrinfo host service [] in
  Ok (List.filter_map of_addr_info info)

let of_host_and_port ~host ~port =
  match get_info host (Int.to_string port) with
  | Ok (ip :: _) -> Ok ip
  | Ok [] -> Error `No_info
  | Error err -> Error err

let get_info (`Tcp (host, port)) = get_info host (Int.to_string port)
let ip (`Tcp (ip, _)) = ip
let port (`Tcp (_, port)) = port