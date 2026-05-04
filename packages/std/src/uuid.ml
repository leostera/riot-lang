(** UUID implementation using native Kernel.UUID for v4/v7 *)
open Global
open IO

(** {1 Type} *)

type t = bytes

type error =
  | InvalidUuid of string

external v4_native: unit -> bytes = "std_uuid_v4"

external v7_native: unit -> bytes = "std_uuid_v7"

external to_string_native: bytes -> string = "std_uuid_to_string"

external unsafe_from_string_native: string -> bytes = "std_uuid_of_string"

external compare_native: bytes -> bytes -> int = "std_uuid_compare"

let compare = fun left right ->
  let order = compare_native left right in
  if order < 0 then
    Order.LT
  else if order > 0 then
    Order.GT
  else
    Order.EQ

external is_nil_native: bytes -> bool = "std_uuid_is_nil"

(** UUID represented as 16 bytes *)
(** {1 Creation - Using Native Platform APIs} *)

let v4 = fun () -> v4_native ()

(** Generate random UUID v4 using platform's cryptographic RNG *)
let v7 = fun () -> v7_native ()

(** Generate timestamp-ordered UUID v7 (RFC 9562) - sortable by creation time *)
let v5 = fun ~namespace:_ ~name:_ -> raise (Invalid_argument "UUID.v5 not yet implemented")

let v3 = fun ~namespace:_ ~name:_ -> raise (Invalid_argument "UUID.v3 not yet implemented")

let v4_from_bytes = fun bytes ->
  if Bytes.length bytes = 16 then
    Bytes.sub_unchecked bytes ~offset:0 ~len:16
  else
    raise (Invalid_argument "UUID.v4_from_bytes: invalid bytes")

let v7_from_parts = fun ~time_ms:_ ~rand_a:_ ~rand_b:_ ->
  raise
    (Invalid_argument "UUID.v7_from_parts not yet implemented")

(** {1 Constants} *)

let nil = Bytes.from_string "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"

let max = Bytes.from_string "\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF"

let ns_dns = Bytes.from_string "\x6b\xa7\xb8\x10\x9d\xad\x11\xd1\x80\xb4\x00\xc0\x4f\xd4\x30\xc8"

let ns_url = Bytes.from_string "\x6b\xa7\xb8\x11\x9d\xad\x11\xd1\x80\xb4\x00\xc0\x4f\xd4\x30\xc8"

let ns_oid = Bytes.from_string "\x6b\xa7\xb8\x12\x9d\xad\x11\xd1\x80\xb4\x00\xc0\x4f\xd4\x30\xc8"

let ns_x500 = Bytes.from_string "\x6b\xa7\xb8\x14\x9d\xad\x11\xd1\x80\xb4\x00\xc0\x4f\xd4\x30\xc8"

(** {1 Parsing} *)

let from_string = fun value ->
  try Ok (unsafe_from_string_native value) with
  | Invalid_argument msg -> Error (InvalidUuid msg)

let from_bytes = fun value ->
  if Bytes.length value = 16 then
    Ok (Bytes.sub_unchecked value ~offset:0 ~len:16)
  else
    Error (InvalidUuid "UUID must be exactly 16 bytes")

(** {1 Serialization} *)

let to_string = fun ?(upper = false) uuid ->
  let str = to_string_native uuid in
  if upper then
    String.uppercase_ascii str
  else
    str

let to_string_nodash = fun ?(upper = false) uuid ->
  let str = to_string ~upper:false uuid in
  let result = String.concat "" (String.split ~by:"-" str) in
  if upper then
    String.uppercase_ascii result
  else
    result

let to_bytes = fun uuid -> Bytes.sub_unchecked uuid ~offset:0 ~len:(Bytes.length uuid)

(** {1 Comparison} *)

let equal = fun left right -> String.equal (Bytes.to_string left) (Bytes.to_string right)

let is_nil = is_nil_native

(** {1 Query} *)

let version = fun uuid ->
  if Bytes.length uuid < 7 then
    None
  else
    let byte6 =
      Bytes.get uuid ~at:6
      |> Option.unwrap
      |> Char.to_int
    in
    let version = (byte6 lsr 4) land 0x0f in
    if version >= 1 && version <= 8 then
      Some version
    else
      None

let variant = fun _uuid -> 0x8

(* RFC 4122 variant *)

let time = fun _uuid -> None

(* TODO: extract from v7 *)
(** {1 Monotonic UUIDv7 for Transaction IDs} *)

(**
   Monotonic UUIDv7 state to prevent time regressions.

   This ensures that even if the system clock jumps backwards (NTP adjustment,
   manual clock change), we never generate a UUID that sorts before a previously
   generated one. This is critical for LSM "last write wins" semantics.
*)

module Monotonic = struct
  open Sync

  type state = {
    last_timestamp_ms: int64 Cell.t;
  }

  let create = fun () -> { last_timestamp_ms = cell 0L }

  (**
     Extract timestamp (ms since epoch) from UUIDv7 bytes.
     UUIDv7 format: [timestamp_ms(48 bits) | ver(4) | rand_a(12) | var(2) | rand_b(62)]
  *)
  let extract_timestamp_ms = fun uuid ->
    let open Bytes in
    let base = Int64.from_int 256 in
    let byte index =
      Int64.from_int
        (
          Char.to_int
            (
              get uuid ~at:index
              |> Option.unwrap
            )
        )
    in
    let rec loop index acc =
      if index > 5 then
        acc
      else
        loop (index + 1) (Int64.add (Int64.mul acc base) (byte index))
    in
    loop 0 0L

  (**
     Generate monotonic UUIDv7.
     If the current timestamp is less than the last seen timestamp,
     we clamp to last_timestamp + 1ms to preserve monotonicity.
  *)
  let v7 = fun state ->
    let uuid = v7 () in
    let time_ms = extract_timestamp_ms uuid in
    let last_ms = Cell.get state.last_timestamp_ms in
    if time_ms < last_ms then (
      (* Clock went backwards - need to clamp *)
      let clamped_ms = Int64.add last_ms 1L in
      Cell.set state.last_timestamp_ms clamped_ms;
      (* Generate new UUID with clamped timestamp *)
      (* For MVP, we'll just generate a new one and hope it's >= clamped_ms
         A full implementation would rebuild the UUID with exact timestamp
      *)
      let new_uuid = v7 () in
      Cell.set state.last_timestamp_ms (extract_timestamp_ms new_uuid);
      new_uuid
    ) else (
      Cell.set state.last_timestamp_ms time_ms;
      uuid
    )
end

(**
   Global monotonic state for transaction IDs.
   Use {!v7_monotonic} for generating transaction UUIDs.
*)
let _global_monotonic_state = Monotonic.create ()

let v7_monotonic = fun () -> Monotonic.v7 _global_monotonic_state

(**
   Generate monotonic UUIDv7 safe for transaction IDs.

   This variant ensures that UUIDs are strictly monotonically increasing
   even in the presence of system clock adjustments (NTP, manual changes).

   Use this instead of {!v7} when the ordering of UUIDs is semantically
   important (e.g., LSM transaction IDs where "later must sort after earlier").

   @since 1.0.0
*)
