(** Low-level cryptographic hash functions via C bindings *)

val md5 : string -> string
(** Raw MD5 hash - returns 16-byte digest *)

val sha1 : string -> string
(** Raw SHA1 hash - returns 20-byte digest *)

val sha256 : string -> string
(** Raw SHA256 hash - returns 32-byte digest *)

val sha512 : string -> string
(** Raw SHA512 hash - returns 64-byte digest *)

val bytes_to_hex : string -> string
(** Convert raw bytes to hex string *)
