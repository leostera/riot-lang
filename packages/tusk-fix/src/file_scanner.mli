(** File scanner for finding OCaml source files *)

open Std

(** A file scanner configured with a root directory and exclusion patterns *)
(** Create a new file scanner.
    
    @param root The root directory to scan
    @param exclude_patterns Directory names to exclude (default: ["."; "_build"; "target"])
*)
type t
val create : root:Path.t ->
?exclude_patterns:string list ->
?should_ignore:(Path.t -> bool) ->
unit ->
t

(** Create a scanner over multiple roots.

    [should_ignore] is applied to both files and directories during discovery,
    so ignored subtrees are pruned eagerly.
*)
val create_many : roots:Path.t list ->
?exclude_patterns:string list ->
?should_ignore:(Path.t -> bool) ->
unit ->
t

(** Scan the configured directory tree and return all .ml and .mli files.
    
    Returns a list of file paths, excluding files in directories matching
    the exclusion patterns.
*)
val scan : t -> Path.t list

(** Start streaming discovered files to [owner] via [Messages.ScannerDiscovered]
    and finish with [Messages.ScannerComplete]. *)
val start : owner:Pid.t -> t -> Pid.t
