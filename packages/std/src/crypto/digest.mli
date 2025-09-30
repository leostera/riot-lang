(** Digest functions for converting hashes to various formats *)

val hex : Kernel.Crypto.hash -> string
(** Convert hash to hexadecimal string *)

val base64 : Kernel.Crypto.hash -> string
(** Convert hash to base64 string *)

val base64_url : Kernel.Crypto.hash -> string
(** Convert hash to URL-safe base64 string *)

val bytes : Kernel.Crypto.hash -> bytes
(** Get raw bytes of hash *)

val to_int64 : Kernel.Crypto.hash -> int64
(** Convert hash to int64 (truncates if necessary) *)

val to_int : Kernel.Crypto.hash -> int
(** Convert hash to int (truncates) *)
