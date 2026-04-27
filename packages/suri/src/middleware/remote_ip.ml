open Std

(** Check if IP string is in trusted proxy list *)
let is_trusted_proxy = fun proxies ip_str -> List.exists (String.equal ip_str) proxies

let is_digit = fun char -> char >= '0' && char <= '9'

let is_hex_digit = fun char ->
  (char >= '0' && char <= '9') || (char >= 'a' && char <= 'f') || (char >= 'A' && char <= 'F')

let is_valid_ipv4_octet = fun value ->
  let len = String.length value in
  if len = 0 || len > 3 || not (String.for_all value ~fn:is_digit) then
    false
  else
    match Int.of_string_opt value with
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
let find_real_ip = fun proxies forwarded_ips ->
  (* Walk from right to left (closest proxy first) *)
  let rec walk_chain = function
    | [] -> None
    | ip_str :: rest ->
        if not (is_valid_ip_literal ip_str) then
          None
        else if is_trusted_proxy proxies ip_str then
          walk_chain rest
        else
          (* First untrusted IP = real client *)
          Some ip_str
  in
  walk_chain (List.rev forwarded_ips)

let resolve_real_ip = fun ~proxies ~peer_ip ~header_value ->
  if not (is_trusted_proxy proxies peer_ip) then
    None
  else
    header_value
    |> parse_forwarded_for
    |> find_real_ip proxies

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
          match resolve_real_ip ~proxies ~peer_ip:current_peer.ip ~header_value with
          | Some real_ip_str ->
              let new_peer = { current_peer with ip = real_ip_str } in
              let conn' = Conn.with_peer new_peer conn in
              next conn'
          | None ->
              (* Couldn't determine real IP, use existing peer *)
              next conn
        )
