open Std

type t
type load_error =
  | PackageNotFound of { dependant: string option; package: string; path: string }
  | PackageTomlReadFailed of { package: string; path: string }
  | PackageTomlParseFailed of { package: string; path: string }
  | PackageFromTomlFailed of { package: string; path: string; error: string }
val load_error_to_string: load_error -> string

val create: unit -> t

val load_riot_toml: t -> Path.t -> (Std.Data.Toml.value, string) result

(** Starting from the given directory, walk up the filesystem tree looking for a
    riot.toml with a [workspace] section. Returns None if no workspace is found.
*)
val find_workspace_root: t -> Path.t -> Path.t option

(** Scan for a workspace starting from the given path. Walks up to find the
    workspace root, loads the workspace manifest, discovers all member packages
    and external dependencies, and returns a fully populated Workspace.t along with
    any errors encountered while loading external packages. *)
val scan: t -> Path.t -> ((Workspace.t * load_error list), string) result

(** Alias for scan *)
val load: t -> root:Path.t -> ((Workspace.t * load_error list), string) result
