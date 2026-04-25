open Std

type t

type manifest_load_error =
  | ManifestReadFailed of { path: Path.t; error: IO.error }
  | ManifestParseFailed of { path: Path.t; error: Std.Data.Toml.error }

type scan_error =
  | WorkspaceTomlLoadFailed of { path: Path.t; error: manifest_load_error }
  | WorkspaceManifestDecodeFailed of { path: Path.t; error: Workspace_manifest.error }
  | PackageTomlLoadFailed of { path: Path.t; error: manifest_load_error }
  | PackageManifestDecodeFailed of { path: Path.t; error: Package_manifest.error }
  | NoWorkspaceRootFound
  | ScanException of { message: string }

type load_error =
  | PackageNotFound of { dependant: string option; package: string; path: string }
  | PackageTomlReadFailed of { package: string; path: string }
  | PackageTomlParseFailed of { package: string; path: string }
  | PackageFromTomlFailed of { package: string; path: string; error: Package_manifest.error }

val manifest_load_error_message: manifest_load_error -> string

val scan_error_message: scan_error -> string

val load_error_to_string: load_error -> string

val create: unit -> t

val clear_cache: t -> unit

val load_riot_toml: t -> Path.t -> (Std.Data.Toml.value, manifest_load_error) result

(**
   Starting from the given directory, walk up the filesystem tree looking for a
   riot.toml with a [workspace] section. Returns None if no workspace is found.
*)
val find_workspace_root: t -> Path.t -> Path.t option

(**
   Scan from the given path.

   If a workspace root is found, load the full workspace. Otherwise, if a
   standalone package [riot.toml] is found while walking upward, synthesize a
   one-package workspace rooted at that package directory.

   Returns the populated workspace and any package load errors encountered
   while loading external path dependencies.
*)
val scan: t -> Path.t -> ((Workspace_manifest.t * load_error list), scan_error) result

(** Alias for scan *)
val load: t -> root:Path.t -> ((Workspace_manifest.t * load_error list), scan_error) result
