open Std

type load_error =
  | PackageNotFound of { dependant: string option; package: string; path: string }
  | PackageTomlReadFailed of { package: string; path: string }
  | PackageTomlParseFailed of { package: string; path: string }
  | PackageFromTomlFailed of { package: string; path: string }
val load_error_to_string: load_error -> string
(** Starting from the given directory, walk up the filesystem tree looking for a
    tusk.toml with a [workspace] section. Returns None if no workspace is found.
*)
val find_workspace_root: Path.t -> Path.t option
(** Scan for a workspace starting from the given path. Walks up to find the
    workspace root, loads the workspace manifest, discovers all member packages
    and external dependencies, and returns a fully populated Workspace.t along with
    any errors encountered while loading external packages. *)
val scan: Path.t -> ((Workspace.t * load_error list), string) result
(** Alias for scan *)
val load: root:Path.t -> ((Workspace.t * load_error list), string) result
