open Std

val find_workspace_root : Path.t -> Path.t option
(** Starting from the given directory, walk up the filesystem tree looking for a
    tusk.toml with a [workspace] section. Returns None if no workspace is found.
*)

val scan : Path.t -> (Workspace.t, string) result
(** Scan for a workspace starting from the given path. Walks up to find the
    workspace root, loads the workspace manifest, discovers all member packages
    and external dependencies, and returns a fully populated Workspace.t *)

val load : root:Path.t -> (Workspace.t, string) result
(** Alias for scan *)
