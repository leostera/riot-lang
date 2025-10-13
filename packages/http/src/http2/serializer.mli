(** HTTP/2 Frame Serializer *)

open Std

val serialize_frame : Frame.t -> string

val write_uint24_be : int -> string
val write_uint32_be : int -> string
val write_uint16_be : int -> string
