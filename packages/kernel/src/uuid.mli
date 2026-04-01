(** Low-level UUID generation using platform libraries *)
(** UUID represented as 16 bytes *)
type t = bytes
(** {1 Generation} *)
(** Generate random UUID v4 using platform's cryptographic RNG *)
val v4: unit -> bytes
(** Generate timestamp-ordered UUID v7 (RFC 9562) - sortable by creation time *)
val v7: unit -> bytes

(** {1 Conversion} *)
(** Convert UUID to canonical string format (lowercase with dashes) *)
val to_string: bytes -> string
(** Parse UUID string *)
val of_string: string -> (bytes, [
    | `Invalid_uuid of string
  ]) Result.t
(** Compare UUIDs lexicographically *)
val compare: bytes -> bytes -> int

(** {1 Query} *)
(** Check if UUID is nil (all zeros) *)
val is_nil: bytes -> bool
(** Extract UUID version (1-8), returns None for invalid *)
val version: bytes -> int option

(** {1 Constants} *)
(** The nil UUID (all zeros) *)
val nil: bytes
(** The max UUID (all ones) *)
val max: bytes
(** DNS namespace UUID *)
val ns_dns: bytes
(** URL namespace UUID *)
val ns_url: bytes
(** ISO OID namespace UUID *)
val ns_oid: bytes
(** X.500 DN namespace UUID *)
val ns_x500: bytes

(** {1 Helpers} *)
(** Test UUID equality *)
val equal: bytes -> bytes -> bool
(** Copy UUID to new bytes *)
val to_bytes: bytes -> bytes
(** Create UUID from 16 bytes *)
val of_bytes: bytes -> (bytes, [
    | `Invalid_uuid of string
  ]) Result.t
