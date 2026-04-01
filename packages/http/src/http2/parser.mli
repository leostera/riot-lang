(** HTTP/2 Frame Parser *)
open Std

type config = {
  max_frame_size: int;
}
type 'a parse_result =
  | Done of { value: 'a; remaining: string }
  | Need_more
  | Error of string
val parse_frame: string -> Frame.t parse_result

val parse_frame_header:
  ?config:config -> string -> (int * Frame.frame_type * Frame.flags * Frame.stream_id) parse_result

val read_uint24_be: string -> int -> int option

val read_uint32_be: string -> int -> int option

val read_uint16_be: string -> int -> int option
