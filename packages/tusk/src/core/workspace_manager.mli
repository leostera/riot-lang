(** Workspace manager - caches workspace and avoids repeated scanning *)

val get_workspace : root:string -> Workspace.t
(** Get workspace, using cache when possible *)

val scan : Std.Path.t -> (Workspace.t, Error.t) result
(** Scans a directory and its parents until it finds a workspace root, then
    loads it *)
