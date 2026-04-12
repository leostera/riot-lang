(** Private TLS engine used by `Std.Net.TlsStream`.

    This stays std-owned because it is transport-agnostic policy and buffering layered over
    OpenSSL, not a kernel readiness or socket primitive. *)
open Global
open IO

type engine
val init: unit -> unit

val is_available: unit -> bool

val version: unit -> string

val create_client_engine: hostname:string -> engine

val create_server_engine: cert_file:string -> key_file:string -> engine

val pump_encrypted_in: engine -> bytes -> pos:int -> len:int -> int

val read_encrypted_out: engine -> bytes -> int

type read_result =
  | Read of int
  | Need_network_read
  | Need_network_write
  | Eof
val read_decrypted: engine -> bytes -> pos:int -> len:int -> read_result

type write_result =
  | Written of int
  | Need_network_read
  | Need_network_write
val write_plaintext: engine -> bytes -> pos:int -> len:int -> write_result

type handshake_result =
  | Handshake_done
  | Need_network_read
  | Need_network_write
val do_handshake: engine -> handshake_result

val handshake_complete: engine -> bool

val alpn_protocol: engine -> string option
