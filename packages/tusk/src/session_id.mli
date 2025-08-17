(** Session ID module - provides opaque session identifiers for build operations
*)

type t
(** The opaque type of a session ID. Session IDs are used to track and correlate
    build operations and their associated events across the tusk build system.
*)

val make : unit -> t
(** [make ()] generates a new unique session ID. The ID is guaranteed to be
    unique within the current process runtime. *)

val to_string : t -> string
(** [to_string id] converts a session ID to its string representation. This is
    useful for serialization, logging, and display purposes. *)

val of_string : string -> t
(** [of_string s] creates a session ID from a string representation. This is the
    inverse of [to_string] and is used for deserialization. *)
