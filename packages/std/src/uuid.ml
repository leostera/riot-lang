(** UUID implementation using native Kernel.UUID for v4/v7 *)

open Global
open IO

(** {1 Type} *)

type t = Kernel.UUID.t
(** UUID represented as 16 bytes *)

(** {1 Creation - Using Native Platform APIs} *)

let v4 () = Kernel.UUID.v4 ()
(** Generate random UUID v4 using platform's cryptographic RNG *)

let v7 () = Kernel.UUID.v7 ()
(** Generate timestamp-ordered UUID v7 (RFC 9562) - sortable by creation time *)

let v5 ~namespace:_ ~name:_ =
  raise (Invalid_argument "UUID.v5 not yet implemented")

let v3 ~namespace:_ ~name:_ =
  raise (Invalid_argument "UUID.v3 not yet implemented")

let v4_from_bytes bytes =
  match Kernel.UUID.of_bytes bytes with
  | Ok uuid -> uuid
  | Error _ -> raise (Invalid_argument "UUID.v4_from_bytes: invalid bytes")

let v7_from_parts ~time_ms:_ ~rand_a:_ ~rand_b:_ =
  raise (Invalid_argument "UUID.v7_from_parts not yet implemented")

(** {1 Constants} *)

let nil = Kernel.UUID.nil
let max = Kernel.UUID.max
let ns_dns = Kernel.UUID.ns_dns
let ns_url = Kernel.UUID.ns_url
let ns_oid = Kernel.UUID.ns_oid
let ns_x500 = Kernel.UUID.ns_x500

(** {1 Parsing} *)

let of_string = Kernel.UUID.of_string

let of_bytes = Kernel.UUID.of_bytes

(** {1 Serialization} *)

let to_string ?(upper=false) uuid =
  let str = Kernel.UUID.to_string uuid in
  if upper then String.uppercase_ascii str else str

let to_string_nodash ?(upper=false) uuid =
  let str = to_string ~upper:false uuid in
  let result = String.concat "" (String.split_on_char '-' str) in
  if upper then String.uppercase_ascii result else result

let to_bytes = Kernel.UUID.to_bytes

(** {1 Comparison} *)

let equal = Kernel.UUID.equal
let compare = Kernel.UUID.compare
let is_nil = Kernel.UUID.is_nil

(** {1 Query} *)

let version = Kernel.UUID.version

let variant _uuid = 0x8  (* RFC 4122 variant *)

let time _uuid = None  (* TODO: extract from v7 *)
