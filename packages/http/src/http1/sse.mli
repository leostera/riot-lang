(** Server-Sent Events Parser *)
open Std

(** Parse a single SSE line. Returns None for empty lines or comments. *)
type event = {
  data: string;
  event_type: string option;
  id: string option;
  retry: int option;
}
val parse_line_slice: IO.IoVec.IoSlice.t -> event option

val parse_line: string -> event option
