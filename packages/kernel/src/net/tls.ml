(** TLS engine using OpenSSL BIO pairs *)
open Global0
open IO

(* Opaque type for TLS engine *)

type engine

(* External FFI declarations *)

external init: unit -> unit = "kernel_tls_init"

external is_available: unit -> bool = "kernel_tls_is_available"

external version: unit -> string = "kernel_tls_version"

external create_client_engine: hostname:string -> engine = "kernel_tls_create_client_engine"

external create_server_engine: cert_file:string -> key_file:string -> engine = "kernel_tls_create_server_engine"

external pump_encrypted_in: engine -> bytes -> pos:int -> len:int -> int = "kernel_tls_pump_encrypted_in"

external read_encrypted_out: engine -> bytes -> int = "kernel_tls_read_encrypted_out"

external _read_decrypted: engine -> bytes -> pos:int -> len:int -> int = "kernel_tls_read_decrypted"

external _write_plaintext: engine -> bytes -> pos:int -> len:int -> int = "kernel_tls_write_plaintext"

type handshake_result =
  | Handshake_done
  | Need_network_read
  | Need_network_write

external do_handshake_raw: engine -> int = "kernel_tls_do_handshake"

let do_handshake = fun engine ->
  match do_handshake_raw engine with
  | 0 -> Handshake_done
  | -1 -> Need_network_read
  | -2 -> Need_network_write
  | _ -> panic "Invalid do_handshake result"

external handshake_complete: engine -> bool = "kernel_tls_handshake_complete"

external alpn_protocol: engine -> string option = "kernel_tls_alpn_protocol"

(* Result types *)

type read_result =
  | Read of int
  | Need_network_read
  | Need_network_write
  | Eof

type write_result =
  Written of int
  | Need_network_read
  | Need_network_write

(* Wrapper for read_decrypted *)

let read_decrypted = fun engine buf ~pos ~len ->
  let n = _read_decrypted engine buf pos len in
  if n > 0 then
    Read n
  else if n = 0 then
    Eof
  else if n = (-1) then
    Need_network_read
  else if n = (-2) then
    Need_network_write
  else
    Eof

(* Shouldn't happen, treat as EOF *)

(* Wrapper for write_plaintext *)

let write_plaintext = fun engine buf ~pos ~len ->
  let n = _write_plaintext engine buf pos len in
  if n > 0 then
    Written n
  else if n = (-1) then
    Need_network_read
  else if n = (-2) then
    Need_network_write
  else
    Need_network_write

(* Shouldn't happen, retry write *)
