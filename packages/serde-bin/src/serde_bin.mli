open Std

(**
   Compact schema-driven binary encoding on top of [Serde].

   Scalars use fixed-width little-endian bytes instead of text or varints:
   - [bool]: 1 byte
   - [int32]: 4 bytes
   - [int64]: 8 bytes
   - [float]: 8 raw IEEE754 bytes
   - [int]: 8-byte signed integer on the wire, range-checked on decode

   Records and variants are positional. Field names and constructor names are
   compile-time schema only and are not written into the payload.
*)
val size_of: 'value Serde.Ser.t -> 'value -> (int, Serde.error) result

val encode_into_bytes: 'value Serde.Ser.t -> bytes -> 'value -> (int, Serde.error) result

val to_string: 'value Serde.Ser.t -> 'value -> (string, Serde.error) result

val to_writer: 'value Serde.Ser.t -> IO.Writer.t -> 'value -> (unit, Serde.error) result

val decode_prefix: 'value Serde.De.t -> string -> ('value * int, Serde.error) result

val from_string: 'value Serde.De.t -> string -> ('value, Serde.error) result

val from_reader: 'value Serde.De.t -> IO.Reader.t -> ('value, Serde.error) result
