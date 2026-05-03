open Global

(**
   JSON parsing over IoSlice.

   `JsonStream` is an additive parser surface that reuses {!Json.t} and
   {!Json.error}, but parses from `Std.IO.IoSlice` instead of
   heap strings by default.

   The direct `from_slice` entry point avoids copying the whole
   source input into a fresh heap string before parsing. `from_string` stays as
   the convenience adapter for ordinary callers.
*)
type t = Json.t
type error = Json.error

val from_string: string -> (t, error) Result.t

val from_slice: IO.IoVec.IoSlice.t -> (t, error) Result.t

val error_to_string: error -> string
