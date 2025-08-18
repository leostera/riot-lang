(** Workspace manager - caches workspace and avoids repeated scanning *)

val get_workspace : root:string -> Workspace.workspace
(** Get workspace for the given root directory. Uses cache when possible to
    avoid repeated scanning. *)

val clear_cache : unit -> unit
(** Clear the workspace cache. Useful when workspace structure changes. *)

val get_cached_root : unit -> string option
(** Get the root directory of the currently cached workspace, if any. *)
