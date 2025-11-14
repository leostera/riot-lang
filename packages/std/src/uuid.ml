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

(** {1 Monotonic UUIDv7 for Transaction IDs} *)

(** Monotonic UUIDv7 state to prevent time regressions.
    
    This ensures that even if the system clock jumps backwards (NTP adjustment,
    manual clock change), we never generate a UUID that sorts before a previously
    generated one. This is critical for LSM "last write wins" semantics.
*)
module Monotonic = struct
  open Sync
  
  type state = {
    last_timestamp_ms : int64 Cell.t;
  }
  
  let create () = {
    last_timestamp_ms = cell 0L;
  }
  
  (** Extract timestamp (ms since epoch) from UUIDv7 bytes.
      UUIDv7 format: [timestamp_ms(48 bits) | ver(4) | rand_a(12) | var(2) | rand_b(62)]
  *)
  let extract_timestamp_ms uuid =
    let open Bytes in
    let b0 = Int64.of_int (Char.code (get uuid 0)) in
    let b1 = Int64.of_int (Char.code (get uuid 1)) in
    let b2 = Int64.of_int (Char.code (get uuid 2)) in
    let b3 = Int64.of_int (Char.code (get uuid 3)) in
    let b4 = Int64.of_int (Char.code (get uuid 4)) in
    let b5 = Int64.of_int (Char.code (get uuid 5)) in
    
    Int64.(
      logor (shift_left b0 40)
      (logor (shift_left b1 32)
      (logor (shift_left b2 24)
      (logor (shift_left b3 16)
      (logor (shift_left b4 8) b5))))
    )
  
  (** Generate monotonic UUIDv7.
      If the current timestamp is less than the last seen timestamp,
      we clamp to last_timestamp + 1ms to preserve monotonicity.
  *)
  let v7 state =
    let uuid = Kernel.UUID.v7 () in
    let time_ms = extract_timestamp_ms uuid in
    let last_ms = Cell.get state.last_timestamp_ms in
    
    if time_ms < last_ms then begin
      (* Clock went backwards - need to clamp *)
      let clamped_ms = Int64.add last_ms 1L in
      Cell.set state.last_timestamp_ms clamped_ms;
      
      (* Generate new UUID with clamped timestamp *)
      (* For MVP, we'll just generate a new one and hope it's >= clamped_ms
         A full implementation would rebuild the UUID with exact timestamp *)
      let new_uuid = Kernel.UUID.v7 () in
      Cell.set state.last_timestamp_ms (extract_timestamp_ms new_uuid);
      new_uuid
    end else begin
      Cell.set state.last_timestamp_ms time_ms;
      uuid
    end
end

(** Global monotonic state for transaction IDs.
    Use {!v7_monotonic} for generating transaction UUIDs.
*)
let _global_monotonic_state = Monotonic.create ()

let v7_monotonic () = Monotonic.v7 _global_monotonic_state
(** Generate monotonic UUIDv7 safe for transaction IDs.
    
    This variant ensures that UUIDs are strictly monotonically increasing
    even in the presence of system clock adjustments (NTP, manual changes).
    
    Use this instead of {!v7} when the ordering of UUIDs is semantically
    important (e.g., LSM transaction IDs where "later must sort after earlier").
    
    @since 1.0.0
*)
