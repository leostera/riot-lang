(**
   # Encoding.Octal

   Octal numeric text formatting and parsing helpers.

   This module is for integer values rendered as octal text, which makes it
   useful for archive metadata, permission strings, and protocol fields that
   use octal numerals.

   Accepted input forms for decoding:

   - bare octal digits such as `"755"`
   - prefixed octal strings such as `"0o755"`
   - signed forms such as `"-10"` or `"+0o10"`

   ## Examples

   ```ocaml
   open Std

   let file_mode = Encoding.Octal.encode_int 0o755
   let parsed = Encoding.Octal.decode_int "755"
   let signed = Encoding.Octal.decode_int64 "-10"
   ignore (file_mode, parsed, signed)
   ```
*)
open Global

type decode_error = [`Invalid_octal]

(** Encode an [`int`] as octal digits. *)
val encode_int: int -> string

(** Encode an [`int32`] as octal digits. *)
val encode_int32: int32 -> string

(** Encode an [`int64`] as octal digits. *)
val encode_int64: int64 -> string

(** Decode an octal string into an [`int`]. *)
val decode_int: string -> (int, decode_error) result

(** Decode an octal string into an [`int32`]. *)
val decode_int32: string -> (int32, decode_error) result

(** Decode an octal string into an [`int64`]. *)
val decode_int64: string -> (int64, decode_error) result
