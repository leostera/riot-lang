(** Content-based hashing module for build artifacts *)

(** Opaque hash type - internally a string but hidden from users *)
type hash

(** Hash a string content *)
val hash_string : string -> hash

(** Hash a file's content *)
val hash_file : string -> hash

(** Convert hash to string for storage/display *)
val to_string : hash -> string

(** Create hash from string (for loading from storage) *)
val of_string : string -> hash

(** Compare two hashes for equality *)
val equal : hash -> hash -> bool