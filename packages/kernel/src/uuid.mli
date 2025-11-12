(** Low-level UUID generation using platform libraries *)

type t = bytes
(** UUID represented as 16 bytes *)

(** {1 Generation} *)

val v4 : unit -> bytes
(** Generate random UUID v4 using platform's cryptographic RNG *)

val v7 : unit -> bytes
(** Generate timestamp-ordered UUID v7 (RFC 9562) - sortable by creation time *)

(** {1 Conversion} *)

val to_string : bytes -> string
(** Convert UUID to canonical string format (lowercase with dashes) *)

val of_string : string -> (bytes, [ `Invalid_uuid of string ]) Result.t
(** Parse UUID string *)

val compare : bytes -> bytes -> int
(** Compare UUIDs lexicographically *)

(** {1 Query} *)

val is_nil : bytes -> bool
(** Check if UUID is nil (all zeros) *)

val version : bytes -> int option
(** Extract UUID version (1-8), returns None for invalid *)

(** {1 Constants} *)

val nil : bytes
(** The nil UUID (all zeros) *)

val max : bytes
(** The max UUID (all ones) *)

val ns_dns : bytes
(** DNS namespace UUID *)

val ns_url : bytes
(** URL namespace UUID *)

val ns_oid : bytes
(** ISO OID namespace UUID *)

val ns_x500 : bytes
(** X.500 DN namespace UUID *)

(** {1 Helpers} *)

val equal : bytes -> bytes -> bool
(** Test UUID equality *)

val to_bytes : bytes -> bytes
(** Copy UUID to new bytes *)

val of_bytes : bytes -> (bytes, [ `Invalid_uuid of string ]) Result.t
(** Create UUID from 16 bytes *)
