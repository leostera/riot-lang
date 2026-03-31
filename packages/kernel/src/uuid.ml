(** Low-level UUID generation using platform libraries *)
open Global0
open IO

type t = bytes

(** {1 External Functions} *)

external v4 : unit -> bytes = "kernel_uuid_v4"

external v7 : unit -> bytes = "kernel_uuid_v7"

external to_string : bytes -> string = "kernel_uuid_to_string"

external of_string_native : string -> bytes = "kernel_uuid_of_string"

external compare : bytes -> bytes -> int = "kernel_uuid_compare"

external is_nil_native : bytes -> bool = "kernel_uuid_is_nil"

(** {1 Safe Wrappers} *)

let of_string = fun str ->
  try Result.Ok (of_string_native str) with
  | Invalid_argument msg -> Result.Error (`Invalid_uuid msg)

let is_nil = is_nil_native

(** {1 Constants} *)

let nil = Stdlib.Bytes.make 16 '\x00'

let max = Stdlib.Bytes.make 16 '\xFF'

(* Standard namespace UUIDs per RFC 4122 *)

let ns_dns = Stdlib.Bytes.of_string "\x6b\xa7\xb8\x10\x9d\xad\x11\xd1\x80\xb4\x00\xc0\x4f\xd4\x30\xc8"

let ns_url = Stdlib.Bytes.of_string "\x6b\xa7\xb8\x11\x9d\xad\x11\xd1\x80\xb4\x00\xc0\x4f\xd4\x30\xc8"

let ns_oid = Stdlib.Bytes.of_string "\x6b\xa7\xb8\x12\x9d\xad\x11\xd1\x80\xb4\x00\xc0\x4f\xd4\x30\xc8"

let ns_x500 = Stdlib.Bytes.of_string "\x6b\xa7\xb8\x14\x9d\xad\x11\xd1\x80\xb4\x00\xc0\x4f\xd4\x30\xc8"

(** {1 Helpers} *)

let equal = fun a b ->
  Stdlib.Bytes.equal a b

let to_bytes = fun uuid -> Stdlib.Bytes.copy uuid

let of_bytes = fun bytes ->
  if Stdlib.Bytes.length bytes = 16 then
    Result.Ok (Stdlib.Bytes.copy bytes)
  else
    Result.Error (`Invalid_uuid "UUID must be exactly 16 bytes")

let version = fun uuid ->
  if Stdlib.Bytes.length uuid < 7 then
    None
  else
    let byte6 = Stdlib.Bytes.get uuid 6 |> Stdlib.Char.code in
    let ver = (byte6 lsr 4) land 0x0f in
    if ver >= 1 && ver <= 8 then
      Some ver
    else
      None
