open Std
open Model

(** Workspace manager - caches workspace and avoids repeated scanning *)

type cached_workspace = {
  workspace : Workspace.t;
  root : string;
  last_scanned : Datetime.t;
}

val clear_cache : unit -> unit
val get_cached_root : unit -> string option

val scan : Path.t -> (Workspace.t, Error.t) result
(** Scans a directory and its parents until it finds a workspace root, then
    loads it *)

val load : root:Path.t -> (Workspace.t, Error.t) result
