open Prelude

type error =
  | InvalidPort of { port: int }

type t = {
  ip: Ip_addr.t;
  port: int;
}

let ( let* ) value fn = Result.and_then value ~fn

let error_to_string = fun value ->
  match value with
  | InvalidPort { port } -> String.concat "" [ "invalid socket port: "; Int.to_string port ]

let validate_port = fun port ->
  if port < 0 || port > 65_535 then
    Result.Error (InvalidPort { port })
  else
    Result.Ok ()

let unsafe_make = fun ~ip ~port -> { ip; port }

let make = fun ~ip ~port ->
  let* () = validate_port port in
  Result.Ok { ip; port }

let from_parts = make

let loopback_v4 = fun ~port -> unsafe_make ~ip:Ip_addr.v4_loopback ~port

let loopback_v6 = fun ~port -> unsafe_make ~ip:Ip_addr.v6_loopback ~port

let ip = fun value -> value.ip

let port = fun value -> value.port

let to_parts = fun value -> (value.ip, value.port)

let has_colon = fun value ->
  let rec loop index =
    if index = String.length value then
      false
    else if String.get_unchecked value ~at:index = ':' then
      true
    else
      loop (index + 1)
  in
  loop 0

let to_string = fun value ->
  let ip = Ip_addr.to_string value.ip in
  if has_colon ip then
    String.concat "" [ "["; ip; "]:"; Int.to_string value.port ]
  else
    String.concat "" [ ip; ":"; Int.to_string value.port ]
