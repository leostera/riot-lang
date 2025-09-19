(** Path manipulation module - Type-safe filesystem paths

    All paths are guaranteed to be valid UTF-8 strings. Similar to Rust's
    std::path::Path, but owned (like PathBuf) since OCaml has GC. *)

type t
(** Abstract type representing a filesystem path (always valid UTF-8) *)

(** Path-specific errors *)
type error =
  | InvalidUtf8 of { path : string }
  | SystemInvalidUtf8 of { syscall : string; path : string }
  | SystemError of string

val of_string : string -> (t, error) Result.t
(** Create a path from a string, returning an error if the string is not valid
    UTF-8. *)

val v : string -> t
(** Create a path from a string, panics if the string is not valid UTF-8.
    Convenient function for when you know the path string is valid. *)

val to_string : t -> string
(** Convert a path to a string (always valid UTF-8) *)

val join : t -> t -> t
(** Join two paths together *)

val ( / ) : t -> t -> t
(** Join paths together - allows chaining: a / b / c *)

val parent : t -> t option
(** Get the parent directory of a path *)

val basename : t -> string
(** Get the basename (filename) of a path *)

val dirname : t -> t
(** Get the directory name of a path *)

val extension : t -> string option
(** Get file extension if present *)

val remove_extension : t -> t
(** Remove extension from path *)

val add_extension : t -> string -> t
(** Add or replace extension *)

val is_absolute : t -> bool
(** Check if path is absolute *)

val is_relative : t -> bool
(** Check if path is relative *)

val components : t -> t list
(** Split path into components. For example:
    - "a/b/c" returns [a; b; c]
    - "/a/b/c" returns [/; a; b; c]
    - "a/b/../c" returns [a; b; ..; c] (no normalization) *)

val normalize : t -> t
(** Normalize a path (remove . and .. components) *)

val exists : t -> bool
(** Check if path exists on filesystem *)

val is_directory : t -> bool
(** Check if path is a directory *)

val is_file : t -> bool
(** Check if path is a file *)

val equal : t -> t -> bool
(** Compare two paths for equality *)

val pp : Format.formatter -> t -> unit
(** Pretty print a path *)
