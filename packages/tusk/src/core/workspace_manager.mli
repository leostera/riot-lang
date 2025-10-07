open Model
(** Workspace manager - caches workspace and avoids repeated scanning *)

val scan : Std.Path.t -> (Workspace.t, Error.t) result
(** Scans a directory and its parents until it finds a workspace root, then
    loads it *)
