open Std

type t =
  Tcp

let tcp = fun () -> Tcp

let handshake = fun (Tcp) ~accepted_at ~stream ~peer ~buffer_size ->
    let conn = Connection.make ~accepted_at ~stream ~buffer_size ~peer () in
    Ok conn
