open Std

(** On-disk cache layout for one registry. *)
type t
type create_error =
  | HomeDirectoryUnavailable
val create_error_message: create_error -> string

(** Create a registry cache rooted under the Riot home directory.

    Use [`riot_home`] to override the default Riot home location in tests or
    custom environments.
*)
val create: ?riot_home:Path.t -> registry_name:string -> unit -> (t, create_error) result

(** Return the Riot home directory used by the cache. *)
val riot_home: t -> Path.t

(** Return the registry name. *)
val registry_name: t -> string

(** Return the root directory for this registry cache. *)
val registry_dir: t -> Path.t

(** Return the directory storing sparse-index data. *)
val index_dir: t -> Path.t

(** Return the directory storing downloaded package archives. *)
val archive_dir: t -> Path.t

(** Return the archive path for one package version. *)
val archive_path: t -> package_name:string -> version:string -> Path.t

(** Return the directory storing extracted source trees. *)
val src_dir: t -> Path.t

(** Return the extracted source directory for one package version. *)
val package_src_dir: t -> package_name:string -> version:string -> Path.t
