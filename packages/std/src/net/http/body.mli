(**
   Lazy HTTP body storage.

   `Http.Body.t` keeps request/response payloads on their existing storage until a caller
   explicitly materializes them.
*)
type t

(** Empty body. *)
val empty: t

(** Wrap an owned string body. *)
val from_string: string -> t

(** Wrap a borrowed off-heap slice body without materializing it. *)
val from_slice: IO.IoVec.IoSlice.t -> t

(** Returns the body length in bytes. *)
val length: t -> int

(** Returns [true] when the body is empty. *)
val is_empty: t -> bool

(** Materialize the body as an owned string. *)
val to_string: t -> string

(** Returns the borrowed slice when the body is already slice-backed. *)
val to_slice_opt: t -> IO.IoVec.IoSlice.t option
