(** Private TLS engine used by `Std.Net.TlsStream`. *)
open Global
open IO

type engine

external init: unit -> unit = "std_tls_init"

external is_available: unit -> bool = "std_tls_is_available"

external version: unit -> string = "std_tls_version"

external create_client_engine: hostname:string -> engine = "std_tls_create_client_engine"

external create_server_engine_raw: cert_file:string -> key_file:string -> engine =
  "std_tls_create_server_engine"

let create_server_engine = fun ~cert_path ~key_path ->
  create_server_engine_raw
    ~cert_file:(Path.to_string cert_path)
    ~key_file:(Path.to_string key_path)

external pump_encrypted_in: engine -> bytes -> pos:int -> len:int -> int =
  "std_tls_pump_encrypted_in"

external read_encrypted_out: engine -> bytes -> int = "std_tls_read_encrypted_out"

external read_decrypted_raw: engine -> bytes -> pos:int -> len:int -> int = "std_tls_read_decrypted"

external write_plaintext_raw: engine -> bytes -> pos:int -> len:int -> int =
  "std_tls_write_plaintext"

type handshake_result =
  | Handshake_done
  | Need_network_read
  | Need_network_write

external do_handshake_raw: engine -> int = "std_tls_do_handshake"

let do_handshake = fun engine ->
  match do_handshake_raw engine with
  | 0 -> Handshake_done
  | -1 -> Need_network_read
  | -2 -> Need_network_write
  | _ -> panic "Invalid do_handshake result"

external handshake_complete: engine -> bool = "std_tls_handshake_complete"

external alpn_protocol: engine -> string option = "std_tls_alpn_protocol"

type read_result =
  | Read of int
  | Need_network_read
  | Need_network_write
  | Eof

type write_result =
  | Written of int
  | Need_network_read
  | Need_network_write

let read_decrypted = fun engine buf ~pos ~len ->
  let bytes_read = read_decrypted_raw engine buf ~pos ~len in
  if bytes_read > 0 then
    Read bytes_read
  else if bytes_read = 0 then
    Eof
  else if bytes_read = (-1) then
    Need_network_read
  else if bytes_read = (-2) then
    Need_network_write
  else
    Eof

let write_plaintext = fun engine buf ~pos ~len ->
  let bytes_written = write_plaintext_raw engine buf ~pos ~len in
  if bytes_written > 0 then
    Written bytes_written
  else if bytes_written = (-1) then
    Need_network_read
  else if bytes_written = (-2) then
    Need_network_write
  else
    Need_network_write
