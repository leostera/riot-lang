(** File scanner for finding OCaml source files *)

open Std

type t
(** A file scanner configured with a root directory and exclusion patterns *)

val create : root:Path.t -> ?exclude_patterns:string list -> unit -> t
(** Create a new file scanner.
    
    @param root The root directory to scan
    @param exclude_patterns Directory names to exclude (default: ["."; "_build"; "target"])
*)

val scan : t -> Path.t list
(** Scan the configured directory tree and return all .ml and .mli files.
    
    Returns a list of file paths, excluding files in directories matching
    the exclusion patterns.
*)
