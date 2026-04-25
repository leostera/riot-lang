open Std

(**
   Byte cursor over a stable source slice.

   `Cursor` is intentionally byte-oriented because parser spans are byte
   offsets into the original source. It never owns or copies source text; all
   slice-returning functions point back into the slice passed to `create`.
*)
type t

val create: IO.IoVec.IoSlice.t -> t

val source: t -> IO.IoVec.IoSlice.t

val position: t -> int

val is_eof: t -> bool

val peek: t -> char option

val peek_n: t -> int -> char option

val advance: t -> unit

val skip_while: t -> (char -> bool) -> unit

val take_slice: t -> (char -> bool) -> IO.IoVec.IoSlice.t

(**
   Materialize the slice returned by `take_slice`. Prefer `take_slice` in hot
   paths that can keep working over source views.
*)
val take_while: t -> (char -> bool) -> string

val slice_view: t -> int -> int -> IO.IoVec.IoSlice.t

(** Materialize a source sub-slice. Prefer `slice_view` where possible. *)
val slice: t -> int -> int -> string
