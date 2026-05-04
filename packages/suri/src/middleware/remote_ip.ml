open Std

(** Check if IP string is in trusted proxy list *)
let is_trusted_proxy = fun proxies ip_str -> List.exists (String.equal ip_str) proxies

type resolve_error =
  | UntrustedPeer of { peer_ip: string }
  | EmptyForwardedFor
  | InvalidForwardedIp of { value: string }
  | NoClientIpInForwardedChain

let resolve_error_to_string = fun __tmp1 ->
  match __tmp1 with
  | UntrustedPeer { peer_ip } -> "socket peer is not a trusted proxy: " ^ peer_ip
  | EmptyForwardedFor -> "forwarded IP chain is empty"
  | InvalidForwardedIp { value } -> "forwarded IP chain contains invalid IP literal: " ^ value
  | NoClientIpInForwardedChain -> "forwarded IP chain does not contain an untrusted client IP"

let is_digit = fun char -> char >= '0' && char <= '9'

let is_hex_digit = fun char ->
  (char >= '0' && char <= '9') || (char >= 'a' && char <= 'f') || (char >= 'A' && char <= 'F')

let is_valid_ipv4_octet = fun value ->
  let len = String.length value in
  if len = 0 || len > 3 || not (String.for_all value ~fn:is_digit) then
    false
  else
    match Int.from_string_opt value with
    | Some octet -> octet >= 0 && octet <= 255
    | None -> false

let is_valid_ipv4_literal = fun value ->
  match String.split_on_char '.' value with
  | [ a; b; c; d ] ->
      is_valid_ipv4_octet a
      && is_valid_ipv4_octet b
      && is_valid_ipv4_octet c
      && is_valid_ipv4_octet d
  | _ -> false

let is_valid_ipv6_char = fun char -> is_hex_digit char || char = ':' || char = '.'

let is_valid_ipv6_literal = fun value ->
  String.contains value ":"
  && String.for_all value ~fn:is_valid_ipv6_char
  && not (String.contains value ":::")

let is_valid_ip_literal = fun value -> is_valid_ipv4_literal value || is_valid_ipv6_literal value

(** Extract IPs from X-Forwarded-For header *)
let parse_forwarded_for = fun header_value ->
  String.split_on_char ',' header_value
  |> List.map ~fn:String.trim
  |> List.filter ~fn:(fun s -> String.length s > 0)

(** Find real client IP by walking X-Forwarded-For chain from right to left *)
let find_real_ip_result = fun proxies forwarded_ips ->
  (* Walk from right to left (closest proxy first) *)
  let rec walk_chain = fun __tmp1 ->
    match __tmp1 with
    | [] -> Error NoClientIpInForwardedChain
    | ip_str :: rest ->
        if not (is_valid_ip_literal ip_str) then
          Error (InvalidForwardedIp { value = ip_str })
        else if is_trusted_proxy proxies ip_str then
          walk_chain rest
        else
          (* First untrusted IP = real client *)
          Ok ip_str
  in
  match forwarded_ips with
  | [] -> Error EmptyForwardedFor
  | _ -> walk_chain (List.rev forwarded_ips)

let find_real_ip = fun proxies forwarded_ips ->
  match find_real_ip_result proxies forwarded_ips with
  | Ok ip -> Some ip
  | Error _ -> None

let resolve_real_ip_result = fun ~proxies ~peer_ip ~header_value ->
  if not (is_trusted_proxy proxies peer_ip) then
    Error (UntrustedPeer { peer_ip })
  else
    header_value
    |> parse_forwarded_for
    |> find_real_ip_result proxies

let resolve_real_ip = fun ~proxies ~peer_ip ~header_value ->
  match resolve_real_ip_result ~proxies ~peer_ip ~header_value with
  | Ok ip -> Some ip
  | Error _ -> None

(** Remote IP middleware *)
let middleware = fun ?(header = "x-forwarded-for") () ~proxies ~conn ~next ->
  (* If no proxies configured, pass through unchanged (safe default) *)
  if List.length proxies = 0 then
    next conn
  else
    let current_peer = Conn.peer conn in
    if not (is_trusted_proxy proxies current_peer.ip) then
      next conn
    else
      (* Get the header value *)
      let headers = Conn.headers conn in
      match Net.Http.Header.get headers header with
      | None ->
          (* No header present, use existing peer *)
          next conn
      | Some header_value -> (
          match resolve_real_ip_result ~proxies ~peer_ip:current_peer.ip ~header_value with
          | Ok real_ip_str ->
              let new_peer = { current_peer with ip = real_ip_str } in
              let conn' = Conn.with_peer new_peer conn in
              next conn'
          | Error _ ->
              (* Couldn't determine real IP, use existing peer *)
              next conn
        )
