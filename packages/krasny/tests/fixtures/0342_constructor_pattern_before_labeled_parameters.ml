type t =
  | Tcp

let handshake Tcp ~accepted_at ~stream ~peer ~buffer_size =
  Ok ()
