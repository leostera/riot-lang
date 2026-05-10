open Std

type protocol =
  | Tcp
  | Udp
  | Sctp

type t = {
  port: int;
  protocol: protocol;
}

let tcp = fun port -> { port; protocol = Tcp }

let udp = fun port -> { port; protocol = Udp }

let sctp = fun port -> { port; protocol = Sctp }

let protocol_to_string = fun protocol ->
  match protocol with
  | Tcp -> "tcp"
  | Udp -> "udp"
  | Sctp -> "sctp"

let to_string = fun value -> Int.to_string value.port ^ "/" ^ protocol_to_string value.protocol

let protocol_equal = fun left right ->
  match (left, right) with
  | (Tcp, Tcp)
  | (Udp, Udp)
  | (Sctp, Sctp) -> true
  | _ -> false

let equal = fun left right ->
  Int.equal left.port right.port && protocol_equal left.protocol right.protocol

let of_string = fun value ->
  match String.split_on_char '/' value with
  | [ port ] -> (
      match Int.parse port with
      | Some port -> Ok (tcp port)
      | None -> Error (Error.JsonError ("invalid Docker port: " ^ value))
    )
  | [ port; protocol ] -> (
      match Int.parse port with
      | None -> Error (Error.JsonError ("invalid Docker port: " ^ value))
      | Some port -> (
          match String.lowercase_ascii protocol with
          | "tcp" -> Ok (tcp port)
          | "udp" -> Ok (udp port)
          | "sctp" -> Ok (sctp port)
          | _ -> Error (Error.JsonError ("invalid Docker port protocol: " ^ value))
        )
    )
  | _ -> Error (Error.JsonError ("invalid Docker port: " ^ value))
