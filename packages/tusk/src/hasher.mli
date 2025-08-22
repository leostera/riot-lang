(** Content-based hashing module for build artifacts *)

type hash
(** Opaque hash type *)

val hash_string : string -> hash
(** Hash a string content *)

val hash_file : string -> hash
(** Hash a file's content *)

val to_string : hash -> string
(** Convert hash to string for storage/display *)

val of_string : string -> hash
(** Create hash from string (for loading from storage) *)

val equal : hash -> hash -> bool
(** Compare two hashes for equality *)

val hash_files : Std.Path.t list -> hash
(** Hash multiple files by combining their individual hashes *)

val hash_strings : string list -> hash
(** Hash multiple strings (typically other hashes) into a single hash *)
